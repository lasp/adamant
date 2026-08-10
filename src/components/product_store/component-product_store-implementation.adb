--------------------------------------------------------------------------------
-- Product_Store Component Implementation Body
--------------------------------------------------------------------------------

with Basic_Types;
with Crc_16;
with Data_Product_Enums;
with Packet_Types;
with Serializer_Types;
with Sys_Time.Arithmetic;

package body Component.Product_Store.Implementation is

   use Product_Store_Types;

   -- Constants describing the layout of the store. The store holds a header of
   -- the CRC followed by the save time. Each entry then holds a one byte stored
   -- length (zero means the entry has never been saved), followed by the data
   -- product's timestamp (if configured), followed by the data product's value:
   Crc_Length : constant Natural := Crc_16.Crc_16_Type'Length;
   Time_Length : constant Natural := Sys_Time.Serialization.Serialized_Length;
   Stored_Length_Length : constant Natural := 1;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the memory region it is to manage,
   -- which holds the data product store.
   --
   -- Init Parameters:
   -- Bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to
   -- be used for storing the data products. The size of this byte array MUST be at
   -- least Store_Description.Store_Size bytes in length (which includes the CRC and
   -- save time header). Only the first Store_Description.Store_Size bytes will be
   -- used by this component.
   -- Restore_On_Set_Up : Boolean - If set to True, the component will attempt to
   -- restore the stored data products into the data product database during Set_Up,
   -- seeding the database with the values saved before the last reboot. If the store
   -- CRC does not validate (i.e. the store was never written or was corrupted) the
   -- restore is skipped and an error event is produced.
   -- Ticks_Per_Save : Positive - The number of ticks that must be received before
   -- the data products are saved to the store. This allows the component to be
   -- connected to a rate group running at a speed appropriate for command
   -- responsiveness (i.e. 1 Hz), while saving to the store at a slower rate (i.e.
   -- every 600 ticks for a 10 minute save cadence).
   -- Commands_Dispatched_Per_Tick : Positive - The number of commands executed per
   -- tick, if any are in the queue.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in not null Basic_Types.Byte_Array_Access; Restore_On_Set_Up : in Boolean := False; Ticks_Per_Save : in Positive := 1; Commands_Dispatched_Per_Tick : in Positive := 3) is
      Expected_Size : Natural := Crc_Length + Time_Length;
   begin
      -- Compute the expected size of the store from the description, and check the
      -- configuration of each entry:
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
      -- The provided byte array must be large enough to hold the store:
      pragma Assert (Bytes.all'Length >= Self.Store_Description.Store_Size);
      -- The store always fits within a single packet, so that it can be dumped.
      -- This is guaranteed by the Store_Size_Type subtype (see
      -- product_store_types.ads), and is restated here for auditability:
      pragma Assert (Self.Store_Description.Store_Size <= Packet_Types.Packet_Buffer_Type'Length);

      -- Store the configuration:
      Self.Bytes := Bytes;
      Self.Restore_On_Set_Up := Restore_On_Set_Up;
      Self.Ticks_Per_Save := Ticks_Per_Save;
      Self.Commands_Dispatched_Per_Tick := Commands_Dispatched_Per_Tick;
   end Init;

   ---------------------------------------
   -- Private helper subprograms:
   ---------------------------------------

   -- Return the index of the first byte of the store data region, which is the
   -- region covered by the CRC:
   function Data_First (Self : in Instance) return Natural is
      (Self.Bytes.all'First + Crc_Length)
      with Inline => True;

   -- Return the index of the last byte of the store:
   function Store_Last (Self : in Instance) return Natural is
      (Self.Bytes.all'First + Self.Store_Description.Store_Size - 1)
      with Inline => True;

   -- Compute the CRC over the store data region:
   function Compute_Store_Crc (Self : in Instance) return Crc_16.Crc_16_Type is
      (Crc_16.Compute_Crc_16 (Self.Bytes.all (Self.Data_First .. Self.Store_Last)))
      with Inline => True;

   -- Read the CRC currently held in the store header:
   function Read_Stored_Crc (Self : in Instance) return Crc_16.Crc_16_Type is
      (Self.Bytes.all (Self.Bytes.all'First .. Self.Bytes.all'First + Crc_Length - 1))
      with Inline => True;

   -- Save the data products into the store, stamping the store with the provided
   -- save time. The slot of any data product that cannot be fetched (or that is
   -- returned with an unexpected length) is left unchanged, preserving the last
   -- saved value, or the never-saved marker (a stored length of zero) if no value
   -- was ever saved:
   procedure Do_Save (Self : in out Instance; Save_Time : in Sys_Time.T) is
      use Basic_Types;
      use Data_Product_Enums.Fetch_Status;
      Idx : Natural := Self.Data_First;
   begin
      -- If the current store contents do not pass the CRC check, then the store
      -- holds memory that was never written (or was corrupted), and no byte of it
      -- can be trusted. Zero the entire data region once, so that stale garbage
      -- can never carry a nonzero stored length and later be mistaken for a saved
      -- value by a restore:
      if Self.Compute_Store_Crc /= Self.Read_Stored_Crc then
         Self.Bytes.all (Self.Data_First .. Self.Store_Last) := [others => 0];
      end if;

      -- Write the save time:
      Self.Bytes.all (Idx .. Idx + Time_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Save_Time);
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
                  Self.Bytes.all (Write_Idx) := Basic_Types.Byte (Item.Size);
                  Write_Idx := @ + Stored_Length_Length;
                  if Item.Store_Timestamp then
                     Self.Bytes.all (Write_Idx .. Write_Idx + Time_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Fetch_Return.The_Data_Product.Header.Time);
                     Write_Idx := @ + Time_Length;
                  end if;
                  Self.Bytes.all (Write_Idx .. Write_Idx + Item.Size - 1) :=
                     Fetch_Return.The_Data_Product.Buffer (Fetch_Return.The_Data_Product.Buffer'First .. Fetch_Return.The_Data_Product.Buffer'First + Item.Size - 1);
               end;
            end if;
            -- If the data product could not be saved, the slot is left unchanged.
            -- This preserves the last saved value (and its stored length), or the
            -- never-saved marker left by the zeroing above if no save ever
            -- succeeded.

            -- Increment the index by the size of the slot:
            Idx := @ + Slot_Length;
         end;
      end loop;

      -- Compute the CRC over the store contents and write it to the header:
      Self.Bytes.all (Self.Bytes.all'First .. Self.Bytes.all'First + Crc_Length - 1) := Self.Compute_Store_Crc;

      -- Update the save counter data product:
      Self.Save_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Save_Count (Self.Sys_Time_T_Get, (Value => Self.Save_Count)));
   end Do_Save;

   -- Restore the data products held in the store into the data product database.
   -- Returns True if the restore was performed, or False if the store CRC did not
   -- validate.
   function Do_Restore (Self : in out Instance) return Boolean is
      use Basic_Types;
      Computed_Crc : constant Crc_16.Crc_16_Type := Self.Compute_Store_Crc;
      Store_Crc : constant Crc_16.Crc_16_Type := Self.Read_Stored_Crc;
      Idx : Natural := Self.Data_First;
      Save_Time_Stamp : Sys_Time.T;
   begin
      -- Check the CRC prior to restoring. This protects against restoring the
      -- contents of memory that was never written or has been corrupted:
      if Computed_Crc /= Store_Crc then
         Self.Event_T_Send_If_Connected (Self.Events.Store_Crc_Invalid (Self.Sys_Time_T_Get, (
            Computed_Crc => Computed_Crc,
            Expected_Crc => Store_Crc)
         ));
         -- Update the CRC invalid counter data product:
         Self.Crc_Invalid_Count := @ + 1;
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Crc_Invalid_Count (Self.Sys_Time_T_Get, (Value => Self.Crc_Invalid_Count)));
         return False;
      end if;

      -- Read the save time:
      Save_Time_Stamp := Sys_Time.Serialization.From_Byte_Array (Self.Bytes.all (Idx .. Idx + Time_Length - 1));
      Idx := @ + Time_Length;

      -- Restore each data product entry:
      for Item of Self.Store_Description.Entries.all loop
         declare
            Slot_Length : constant Natural := Stored_Length_Length + Item.Size + (if Item.Store_Timestamp then Time_Length else 0);
            Stored_Length : constant Basic_Types.Byte := Self.Bytes.all (Idx);
            Slot_Idx : Natural := Idx + Stored_Length_Length;
            Stored_Dp_Time : Sys_Time.T := Sys_Time.Arithmetic.Sys_Time_Zero;
         begin
            if Natural (Stored_Length) = Item.Size then
               -- Read the stored data product timestamp if configured:
               if Item.Store_Timestamp then
                  Stored_Dp_Time := Sys_Time.Serialization.From_Byte_Array (Self.Bytes.all (Slot_Idx .. Slot_Idx + Time_Length - 1));
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
                     Self.Bytes.all (Slot_Idx .. Slot_Idx + Item.Size - 1);
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

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Products_Restored (Self.Sys_Time_T_Get));

      -- Update the restore counter data product:
      Self.Restore_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Restore_Count (Self.Sys_Time_T_Get, (Value => Self.Restore_Count)));
      return True;
   end Do_Restore;

   -- Build and send a packet containing the contents of the store:
   procedure Do_Dump (Self : in out Instance) is
      use Serializer_Types;
      Pkt : Packet.T;
      Stat : constant Serialization_Status := Self.Packets.Stored_Products (Self.Sys_Time_T_Get, Self.Bytes.all (Self.Bytes.all'First .. Self.Store_Last), Pkt);
   begin
      -- This should never fail since the autocoder and the Store_Size_Type
      -- constraint guarantee that the store fits within a single packet:
      pragma Assert (Stat = Success);
      -- Send the packet:
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
   -- the last reboot. If the store CRC does not validate, the restore is skipped
   -- and an error event is produced.
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
            Self.Do_Save (Save_Time =>
               (case Self.Store_Description.Save_Time is
                  when Tick_Time => Arg.Time,
                  when Current_Time => Self.Sys_Time_T_Get));
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
   begin
      Self.Do_Save (Save_Time => Self.Sys_Time_T_Get);
      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Products_Saved (Self.Sys_Time_T_Get));
      return Success;
   end Save_Products;

   -- Restore the data product values held in the store back into the data product
   -- database. The store CRC is checked prior to the restore, and the command fails
   -- if the CRC does not validate.
   overriding function Restore_Products (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Self.Do_Restore then
         return Success;
      else
         return Failure;
      end if;
   end Restore_Products;

   -- Dump the current contents of the store into a packet. The store contents are
   -- dumped as-is, without validating the CRC, so that corrupted store contents can
   -- be inspected on the ground.
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
