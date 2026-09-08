--------------------------------------------------------------------------------
-- Product_Store Component Implementation Body
--------------------------------------------------------------------------------

with Basic_Types;
with Crc_16;
with Data_Product_Enums;
with Packed_U32;
with Packet_Types;
with Product_Store_Enums;
with Serializer_Types;
with Store_Copy_Info;
with Sys_Time.Arithmetic;

package body Component.Product_Store.Implementation is

   use Product_Store_Types;
   use Product_Store_Enums.Store_Copy;
   use type Basic_Types.Byte_Array;
   use type Basic_Types.Byte_Array_Access;

   -- Shorthand for the store copy selector type:
   subtype Store_Copy_Type is Product_Store_Enums.Store_Copy.E;

   -- Constants describing the layout of each copy of the store. A copy holds a
   -- header of the CRC, followed by the monotonic save counter, followed by the
   -- save time. Each entry then holds a one byte stored length (zero means the
   -- entry has never been saved), followed by the data product's timestamp (if
   -- configured), followed by the data product's value. The CRC covers everything
   -- after itself (the save counter, save time, and entries):
   Crc_Length : constant Natural := Crc_16.Crc_16_Type'Length;
   Counter_Length : constant Natural := Packed_U32.Serialization.Serialized_Length;
   Time_Length : constant Natural := Sys_Time.Serialization.Serialized_Length;
   Stored_Length_Length : constant Natural := 1;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the two memory regions it is to
   -- manage, which hold the two copies of the data product store.
   --
   -- Init Parameters:
   -- Bytes_A : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes
   -- to be used for storing copy A of the data products. The size of this byte
   -- array MUST be at least Store_Description.Store_Size bytes in length (which
   -- includes the CRC, save counter, and save time header). Only the first
   -- Store_Description.Store_Size bytes will be used by this component. This
   -- allocation must not overlap the allocation provided for Bytes_B. The two
   -- copies may be placed in different memory banks so that a fault contained to
   -- one bank cannot corrupt both copies.
   -- Bytes_B : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes
   -- to be used for storing copy B of the data products, with the same size
   -- requirement as Bytes_A. This allocation must not overlap the allocation
   -- provided for Bytes_A.
   -- Restore_On_Set_Up : Boolean - If set to True, the component will attempt to
   -- restore the stored data products into the data product database during Set_Up,
   -- seeding the database with the values saved before the last reboot. If neither
   -- store copy holds a valid CRC (i.e. the store was never written or was
   -- corrupted) the restore is skipped and error events are produced.
   -- Ticks_Per_Save : Positive - The number of ticks that must be received before
   -- the data products are saved to the store. This allows the component to be
   -- connected to a rate group running at a speed appropriate for command
   -- responsiveness (i.e. 1 Hz), while saving to the store at a slower rate (i.e.
   -- every 600 ticks for a 10 minute save cadence).
   -- Commands_Dispatched_Per_Tick : Positive - The number of commands executed per
   -- tick, if any are in the queue.
   --
   overriding procedure Init (Self : in out Instance; Bytes_A : in not null Basic_Types.Byte_Array_Access; Bytes_B : in not null Basic_Types.Byte_Array_Access; Restore_On_Set_Up : in Boolean := False; Ticks_Per_Save : in Positive := 1; Commands_Dispatched_Per_Tick : in Positive := 3) is
      Expected_Size : Natural := Crc_Length + Counter_Length + Time_Length;
   begin
      -- Compute the expected size of one copy of the store from the description,
      -- and check the configuration of each entry:
      for Item of Self.Store_Description.Entries.all loop
         Expected_Size := @ + Stored_Length_Length + Item.Size;
         if Item.Store_Timestamp then
            Expected_Size := @ + Time_Length;
         end if;
         -- An entry stores its data product's timestamp if and only if that
         -- timestamp is what the entry is restored with:
         pragma Assert (Item.Store_Timestamp = (Item.Restore_Time = Use_Stored_Dp_Time));
      end loop;
      -- The store size found in the description must match the size computed from
      -- the entries:
      pragma Assert (Expected_Size = Self.Store_Description.Store_Size);
      -- Each provided byte array must be large enough to hold one copy of the
      -- store:
      pragma Assert (Bytes_A.all'Length >= Self.Store_Description.Store_Size);
      pragma Assert (Bytes_B.all'Length >= Self.Store_Description.Store_Size);
      -- The two copies must be held in distinct allocations. Note that this check
      -- cannot detect two distinct allocations that overlap in memory - that
      -- remains a requirement on the caller:
      pragma Assert (Bytes_A /= Bytes_B);
      -- Each copy of the store always fits within a single packet, so that it can
      -- be dumped. This is guaranteed by the Store_Size_Type subtype (see
      -- product_store_types.ads), and is restated here for auditability:
      pragma Assert (Self.Store_Description.Store_Size <= Packet_Types.Packet_Buffer_Type'Length);

      -- Store the configuration:
      Self.Bytes_A := Bytes_A;
      Self.Bytes_B := Bytes_B;
      Self.Restore_On_Set_Up := Restore_On_Set_Up;
      Self.Ticks_Per_Save := Ticks_Per_Save;
      Self.Commands_Dispatched_Per_Tick := Commands_Dispatched_Per_Tick;
   end Init;

   ---------------------------------------
   -- Private helper subprograms:
   ---------------------------------------

   -- Return the byte array allocation holding the given copy of the store:
   function Copy_Bytes (Self : in Instance; Copy : in Store_Copy_Type) return Basic_Types.Byte_Array_Access is
      (case Copy is
         when Copy_A => Self.Bytes_A,
         when Copy_B => Self.Bytes_B)
      with Inline => True;

   -- Return the other copy of the store from the one provided:
   function Other_Copy (Copy : in Store_Copy_Type) return Store_Copy_Type is
      (case Copy is
         when Copy_A => Copy_B,
         when Copy_B => Copy_A)
      with Inline => True;

   -- Return the index of the first byte of a copy's data region, which is the
   -- region covered by the CRC (the save counter, save time, and entries):
   function Data_First (Self : in Instance; Copy : in Store_Copy_Type) return Natural is
      (Self.Copy_Bytes (Copy).all'First + Crc_Length)
      with Inline => True;

   -- Return the index of the last byte of a copy of the store:
   function Store_Last (Self : in Instance; Copy : in Store_Copy_Type) return Natural is
      (Self.Copy_Bytes (Copy).all'First + Self.Store_Description.Store_Size - 1)
      with Inline => True;

   -- Compute the CRC over a copy's data region:
   function Compute_Store_Crc (Self : in Instance; Copy : in Store_Copy_Type) return Crc_16.Crc_16_Type is
      (Crc_16.Compute_Crc_16 (Self.Copy_Bytes (Copy).all (Self.Data_First (Copy) .. Self.Store_Last (Copy))))
      with Inline => True;

   -- Read the CRC currently held in a copy's header:
   function Read_Stored_Crc (Self : in Instance; Copy : in Store_Copy_Type) return Crc_16.Crc_16_Type is
      (Self.Copy_Bytes (Copy).all (Self.Copy_Bytes (Copy).all'First .. Self.Copy_Bytes (Copy).all'First + Crc_Length - 1))
      with Inline => True;

   -- A copy is valid if the CRC computed over its data region matches the CRC
   -- held in its header:
   function Copy_Valid (Self : in Instance; Copy : in Store_Copy_Type) return Boolean is
      (Self.Compute_Store_Crc (Copy) = Self.Read_Stored_Crc (Copy))
      with Inline => True;

   -- Read the save counter currently held in a copy's header:
   function Read_Save_Counter (Self : in Instance; Copy : in Store_Copy_Type) return Unsigned_32 is
      (Packed_U32.Serialization.From_Byte_Array (Self.Copy_Bytes (Copy).all (Self.Data_First (Copy) .. Self.Data_First (Copy) + Counter_Length - 1)).Value)
      with Inline => True;

   -- Return True if the first save counter is newer than the second. The
   -- comparison is wraparound-aware, so it remains correct if the counter ever
   -- rolls over:
   function Is_Newer (Counter : in Unsigned_32; Than : in Unsigned_32) return Boolean is
      (Counter /= Than and then Counter - Than < 2 ** 31)
      with Inline => True;

   -- Determine the valid copy of the store holding the newest save counter. If
   -- no copy holds a valid CRC (i.e. the store was never written or both copies
   -- were corrupted), Valid_Found is set to False and Newest is meaningless:
   procedure Find_Newest_Valid_Copy (Self : in Instance; Valid_Found : out Boolean; Newest : out Store_Copy_Type) is
      A_Valid : constant Boolean := Self.Copy_Valid (Copy_A);
      B_Valid : constant Boolean := Self.Copy_Valid (Copy_B);
   begin
      Valid_Found := A_Valid or else B_Valid;
      if A_Valid and then B_Valid then
         -- Both copies are valid, so the one holding the newer save counter is
         -- selected. Copy A wins a tie, which cannot occur through this
         -- component's own writes, since a save always stamps the written copy
         -- with a counter newer than the other copy's:
         Newest := (if Is_Newer (Self.Read_Save_Counter (Copy_B), Than => Self.Read_Save_Counter (Copy_A)) then Copy_B else Copy_A);
      elsif B_Valid then
         Newest := Copy_B;
      else
         Newest := Copy_A;
      end if;
   end Find_Newest_Valid_Copy;

   -- Save the data products into the store, stamping the store with the provided
   -- save time. The copy NOT holding the most recent valid save is written, so
   -- that a reboot in the middle of the save can never corrupt the only good
   -- copy - at worst, one save interval of freshness is lost. The written copy
   -- is stamped with a save counter one newer than the newest valid copy's, and
   -- its CRC is written last, so that a reboot at any point during the save
   -- leaves the store restorable. The slot of any data product that cannot be
   -- fetched (or that is returned with an unexpected length) holds the value
   -- from the most recent valid save, or the never-saved marker (a stored length
   -- of zero) if no value was ever saved. The copy written and the counter it
   -- was stamped with are returned:
   function Do_Save (Self : in out Instance; Save_Time : in Sys_Time.T) return Store_Copy_Info.T is
      use Basic_Types;
      use Data_Product_Enums.Fetch_Status;
      Valid_Found : Boolean;
      Newest : Store_Copy_Type;
      Saved : Store_Copy_Info.T;
   begin
      Self.Find_Newest_Valid_Copy (Valid_Found, Newest);
      declare
         -- Select the copy to write and the counter to stamp it with. If no
         -- valid copy exists (i.e. the store has never been written), copy A is
         -- written first with a counter of one:
         Target : constant Store_Copy_Type := (if Valid_Found then Other_Copy (Newest) else Copy_A);
         Target_Bytes : constant Basic_Types.Byte_Array_Access := Self.Copy_Bytes (Target);
         New_Counter : constant Unsigned_32 := (if Valid_Found then Self.Read_Save_Counter (Newest) + 1 else 1);
         Idx : Natural := Self.Data_First (Target);
      begin
         -- Seed the target copy's data region from the newest valid copy, so
         -- that the slots of data products that cannot be fetched below keep the
         -- most recently saved values. If no valid copy exists, zero the region
         -- instead, so that stale garbage can never carry a nonzero stored
         -- length and later be mistaken for a saved value by a restore:
         if Valid_Found then
            Target_Bytes.all (Idx .. Self.Store_Last (Target)) :=
               Self.Copy_Bytes (Newest).all (Self.Data_First (Newest) .. Self.Store_Last (Newest));
         else
            Target_Bytes.all (Idx .. Self.Store_Last (Target)) := [others => 0];
         end if;

         -- Write the save counter:
         Target_Bytes.all (Idx .. Idx + Counter_Length - 1) := Packed_U32.Serialization.To_Byte_Array ((Value => New_Counter));
         Idx := @ + Counter_Length;

         -- Write the save time:
         Target_Bytes.all (Idx .. Idx + Time_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Save_Time);
         Idx := @ + Time_Length;

         -- Save each data product entry:
         for Item of Self.Store_Description.Entries.all loop
            declare
               Slot_Length : constant Natural := Stored_Length_Length + Item.Size + (if Item.Store_Timestamp then Time_Length else 0);
               -- Request the data product from the database:
               Fetch_Return : constant Data_Product_Return.T := Self.Data_Product_Fetch_T_Request ((Id => Item.Data_Product_Id));
               Save_Slot : Boolean := False;
            begin
               -- Check the fetch status and throw appropriate events:
               case Fetch_Return.The_Status is
                  when Success =>
                     -- Check the length of the data product to make sure it is what we expect:
                     if Fetch_Return.The_Data_Product.Header.Buffer_Length /= Item.Size then
                        Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Length_Mismatch (Self.Sys_Time_T_Get, (
                           Header => Fetch_Return.The_Data_Product.Header,
                           Expected_Length => Item.Size)
                        ));
                     else
                        Save_Slot := True;
                     end if;
                  when Not_Available =>
                     -- Throw event if configured to do so:
                     if Item.Event_On_Missing then
                        Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Missing_On_Save (Self.Sys_Time_T_Get, (
                           Id => Item.Data_Product_Id)
                        ));
                     end if;
                  when Id_Out_Of_Range =>
                     -- This indicates a misconfiguration between the stored products
                     -- model and the database, so the event is not maskable:
                     Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Id_Out_Of_Range (Self.Sys_Time_T_Get, (
                        Id => Item.Data_Product_Id)
                     ));
               end case;

               if Save_Slot then
                  -- Write the stored length, then the data product (and its timestamp,
                  -- if configured) into the slot:
                  declare
                     Write_Idx : Natural := Idx;
                  begin
                     Target_Bytes.all (Write_Idx) := Basic_Types.Byte (Item.Size);
                     Write_Idx := @ + Stored_Length_Length;
                     if Item.Store_Timestamp then
                        Target_Bytes.all (Write_Idx .. Write_Idx + Time_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Fetch_Return.The_Data_Product.Header.Time);
                        Write_Idx := @ + Time_Length;
                     end if;
                     Target_Bytes.all (Write_Idx .. Write_Idx + Item.Size - 1) :=
                        Fetch_Return.The_Data_Product.Buffer (Fetch_Return.The_Data_Product.Buffer'First .. Fetch_Return.The_Data_Product.Buffer'First + Item.Size - 1);
                  end;
               end if;
               -- If the data product could not be saved, the slot is left as seeded
               -- above. This preserves the value from the most recent valid save
               -- (and its stored length), or the never-saved marker if no save ever
               -- succeeded.

               -- Increment the index by the size of the slot:
               Idx := @ + Slot_Length;
            end;
         end loop;

         -- Compute the CRC over the copy's contents and write it to the header.
         -- The CRC is written last, so that a reboot at any earlier point leaves
         -- this copy invalid and the other copy untouched:
         Target_Bytes.all (Target_Bytes.all'First .. Target_Bytes.all'First + Crc_Length - 1) := Self.Compute_Store_Crc (Target);

         Saved := (Copy => Target, Save_Counter => New_Counter);
      end;

      -- Update the save counter data product:
      Self.Save_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Save_Count (Self.Sys_Time_T_Get, (Value => Self.Save_Count)));
      return Saved;
   end Do_Save;

   -- Restore the data products held in the store into the data product database,
   -- reading from the valid copy holding the newest save counter. Returns True
   -- if the restore was performed, or False if neither copy's CRC validated.
   function Do_Restore (Self : in out Instance) return Boolean is
      use Basic_Types;
      Valid_Found : Boolean;
      Newest : Store_Copy_Type;
   begin
      -- Check that a valid copy exists prior to restoring. This protects against
      -- restoring the contents of memory that was never written or has been
      -- corrupted. An event is produced reporting the CRC mismatch of each copy:
      Self.Find_Newest_Valid_Copy (Valid_Found, Newest);
      if not Valid_Found then
         Self.Event_T_Send_If_Connected (Self.Events.Store_Crc_Invalid (Self.Sys_Time_T_Get, (
            Copy => Copy_A,
            Computed_Crc => Self.Compute_Store_Crc (Copy_A),
            Expected_Crc => Self.Read_Stored_Crc (Copy_A))
         ));
         Self.Event_T_Send_If_Connected (Self.Events.Store_Crc_Invalid (Self.Sys_Time_T_Get, (
            Copy => Copy_B,
            Computed_Crc => Self.Compute_Store_Crc (Copy_B),
            Expected_Crc => Self.Read_Stored_Crc (Copy_B))
         ));
         -- Update the CRC invalid counter data product:
         Self.Crc_Invalid_Count := @ + 1;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Crc_Invalid_Count (Self.Sys_Time_T_Get, (Value => Self.Crc_Invalid_Count)));
         return False;
      end if;

      declare
         Bytes : constant Basic_Types.Byte_Array_Access := Self.Copy_Bytes (Newest);
         -- Skip over the save counter, which was already read to select the copy:
         Idx : Natural := Self.Data_First (Newest) + Counter_Length;
         Save_Time_Stamp : Sys_Time.T;
      begin
         -- Read the save time:
         Save_Time_Stamp := Sys_Time.Serialization.From_Byte_Array (Bytes.all (Idx .. Idx + Time_Length - 1));
         Idx := @ + Time_Length;

         -- Restore each data product entry:
         for Item of Self.Store_Description.Entries.all loop
            declare
               Slot_Length : constant Natural := Stored_Length_Length + Item.Size + (if Item.Store_Timestamp then Time_Length else 0);
               Stored_Length : constant Basic_Types.Byte := Bytes.all (Idx);
               Slot_Idx : Natural := Idx + Stored_Length_Length;
               Stored_Dp_Time : Sys_Time.T := Sys_Time.Arithmetic.Sys_Time_Zero;
            begin
               if Natural (Stored_Length) = Item.Size then
                  -- Read the stored data product timestamp if configured:
                  if Item.Store_Timestamp then
                     Stored_Dp_Time := Sys_Time.Serialization.From_Byte_Array (Bytes.all (Slot_Idx .. Slot_Idx + Time_Length - 1));
                     Slot_Idx := @ + Time_Length;
                  end if;

                  declare
                     -- Select the timestamp to restore the data product with:
                     Restore_Stamp : constant Sys_Time.T :=
                        (case Item.Restore_Time is
                           when Use_Zeros => Sys_Time.Arithmetic.Sys_Time_Zero,
                           when Use_Save_Time => Save_Time_Stamp,
                           when Use_Stored_Dp_Time => Stored_Dp_Time);
                     The_Data_Product : Data_Product.T := (
                        Header => (Time => Restore_Stamp, Id => Item.Data_Product_Id, Buffer_Length => Item.Size),
                        Buffer => [others => 0]
                     );
                  begin
                     -- Copy the stored value into the data product and send it:
                     The_Data_Product.Buffer (The_Data_Product.Buffer'First .. The_Data_Product.Buffer'First + Item.Size - 1) :=
                        Bytes.all (Slot_Idx .. Slot_Idx + Item.Size - 1);
                     Self.Data_Product_T_Send_If_Connected (The_Data_Product);
                  end;
               elsif Natural (Stored_Length) /= 0 then
                  -- The stored length is neither the expected size nor zero. Since the
                  -- CRC over the store validated, this is not corruption - it means the
                  -- stored products model has changed since the store was written. Do
                  -- not restore this entry, and alert:
                  Self.Event_T_Send_If_Connected (Self.Events.Stored_Length_Mismatch (Self.Sys_Time_T_Get, (
                     Id => Item.Data_Product_Id,
                     Stored_Length => Stored_Length,
                     Expected_Length => Item.Size)
                  ));
               end if;
               -- A stored length of zero means this entry has never been saved. It is
               -- skipped silently, leaving the data product unavailable in the
               -- database rather than restoring a meaningless value.

               -- Advance to the next slot:
               Idx := @ + Slot_Length;
            end;
         end loop;
      end;

      -- Send info event reporting the copy restored from and its save counter:
      Self.Event_T_Send_If_Connected (Self.Events.Products_Restored (Self.Sys_Time_T_Get, (
         Copy => Newest,
         Save_Counter => Self.Read_Save_Counter (Newest))
      ));

      -- Update the restore counter data product:
      Self.Restore_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Restore_Count (Self.Sys_Time_T_Get, (Value => Self.Restore_Count)));
      return True;
   end Do_Restore;

   -- Build and send packets containing the contents of both copies of the store:
   procedure Do_Dump (Self : in out Instance) is
      use Serializer_Types;
      Pkt : Packet.T;
      Stat : Serialization_Status := Self.Packets.Stored_Products_A (Self.Sys_Time_T_Get, Self.Bytes_A.all (Self.Bytes_A.all'First .. Self.Store_Last (Copy_A)), Pkt);
   begin
      -- This should never fail since the autocoder and the Store_Size_Type
      -- constraint guarantee that each copy of the store fits within a single
      -- packet:
      pragma Assert (Stat = Success);
      -- Send the packet holding copy A:
      Self.Packet_T_Send_If_Connected (Pkt);

      -- Build and send the packet holding copy B:
      Stat := Self.Packets.Stored_Products_B (Self.Sys_Time_T_Get, Self.Bytes_B.all (Self.Bytes_B.all'First .. Self.Store_Last (Copy_B)), Pkt);
      pragma Assert (Stat = Success);
      Self.Packet_T_Send_If_Connected (Pkt);

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Store_Dumped (Self.Sys_Time_T_Get));
   end Do_Dump;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- The counter data products are seeded with zero here. Then, if the component
   -- is configured with Restore_On_Set_Up, the store contents are restored into
   -- the data product database, seeding the database with the values saved before
   -- the last reboot. If neither store copy holds a valid CRC, the restore is
   -- skipped and error events are produced.
   overriding procedure Set_Up (Self : in out Instance) is
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Seed the counter data products:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Save_Count (Timestamp, (Value => Self.Save_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Restore_Count (Timestamp, (Value => Self.Restore_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Crc_Invalid_Count (Timestamp, (Value => Self.Crc_Invalid_Count)));

      -- Restore the store contents if configured to do so. The restore status is
      -- not needed here - a CRC failure is reported by event and counter, and is
      -- expected on the first boot before the store has ever been written:
      if Self.Restore_On_Set_Up then
         declare
            Ignore : constant Boolean := Self.Do_Restore;
            pragma Unreferenced (Ignore);
         begin
            null;
         end;
      end if;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Commands are dispatched from the
   -- queue on every tick, and the data products are saved to the store every
   -- Ticks_Per_Save ticks (when saving on tick is enabled).
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Messages_Dispatched : Natural;
   begin
      -- Save the data products to the store if saving on tick is enabled and the
      -- tick divider has elapsed. The divider only advances while saving on tick
      -- is enabled - it is frozen while disabled and reset upon re-enable:
      if Self.Save_On_Tick then
         if Self.Tick_Count = 0 then
            declare
               -- The copy written and its counter are reported by event only for
               -- commanded saves, so they are not needed here:
               Ignore : constant Store_Copy_Info.T := Self.Do_Save (Save_Time =>
                  (case Self.Store_Description.Save_Time is
                     when Tick_Time => Arg.Time,
                     when Current_Time => Self.Sys_Time_T_Get));
               pragma Unreferenced (Ignore);
            begin
               null;
            end;
         end if;

         -- Increment the tick counter, rolling over at Ticks_Per_Save:
         Self.Tick_Count := (@ + 1) mod Self.Ticks_Per_Save;
      end if;

      -- Handle any commands in the queue after the business logic is complete.
      -- Service up to N commands per tick:
      Messages_Dispatched := Self.Dispatch_N (Self.Commands_Dispatched_Per_Tick);
      pragma Assert (Messages_Dispatched <= Self.Commands_Dispatched_Per_Tick);
   end Tick_T_Recv_Sync;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Throw info event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Command (Self.Sys_Time_T_Get, Arg.Header));
   end Command_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Product Store component.
   -- Save the configured data products from the database into the store. This
   -- works regardless of whether saving on tick is enabled or disabled. The
   -- current time is used as the save time, even if the store is configured for
   -- Tick_Time, since no tick is available.
   overriding function Save_Products (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Saved : constant Store_Copy_Info.T := Self.Do_Save (Save_Time => Self.Sys_Time_T_Get);
   begin
      -- Send info event reporting the copy written and its save counter:
      Self.Event_T_Send_If_Connected (Self.Events.Products_Saved (Self.Sys_Time_T_Get, Saved));
      return Success;
   end Save_Products;

   -- Restore the data product values held in the store back into the data product
   -- database. The CRC of each store copy is checked prior to the restore, and the
   -- values are restored from the valid copy holding the newest save counter. The
   -- command fails if neither copy's CRC validates.
   overriding function Restore_Products (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Do_Restore then
         return Success;
      else
         return Failure;
      end if;
   end Restore_Products;

   -- Dump the current contents of both store copies, each into its own packet. The
   -- store contents are dumped as-is, without validating the CRCs, so that
   -- corrupted store contents can be inspected on the ground.
   overriding function Dump_Store (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Do_Dump;
      return Success;
   end Dump_Store;

   -- Enable the automatic saving of the data products to the store upon receipt of
   -- a tick.
   overriding function Enable_Save_On_Tick (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Save_On_Tick := True;
      -- Reset the tick divider so that a save deterministically occurs on the
      -- next tick after enabling:
      Self.Tick_Count := 0;
      Self.Event_T_Send_If_Connected (Self.Events.Save_On_Tick_Enabled (Self.Sys_Time_T_Get));
      return Success;
   end Enable_Save_On_Tick;

   -- Disable the automatic saving of the data products to the store upon receipt of
   -- a tick.
   overriding function Disable_Save_On_Tick (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Save_On_Tick := False;
      Self.Event_T_Send_If_Connected (Self.Events.Save_On_Tick_Disabled (Self.Sys_Time_T_Get));
      return Success;
   end Disable_Save_On_Tick;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Product_Store.Implementation;
