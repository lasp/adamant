--------------------------------------------------------------------------------
-- Product_Store Component Implementation Body
--------------------------------------------------------------------------------

with Crc_16;
with Data_Product_Enums;
with Serializer_Types;
with Sys_Time.Arithmetic;

package body Component.Product_Store.Implementation is

   use Product_Store_Types;

   -- Constants describing the layout of the store header:
   Crc_Length : constant Natural := Crc_16.Crc_16_Type'Length;
   Time_Length : constant Natural := Sys_Time.Serialization.Serialized_Length;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the memory region it is to manage,
   -- which holds the data product store.
   --
   -- Init Parameters:
   -- Bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to
   -- be used for storing the data products. The size of this byte array MUST be at
   -- least Store_Description.Store_Size bytes in length. Only the first
   -- Store_Description.Store_Size bytes will be used by this component.
   -- Restore_On_Set_Up : Boolean - If set to True, the component will attempt to
   -- restore the stored data products into the data product database during Set_Up,
   -- seeding the database with the values saved before the last reboot. If the store
   -- CRC does not validate (i.e. the store was never written or was corrupted) the
   -- restore is skipped and an error event is produced.
   -- Commands_Dispatched_Per_Tick : Positive - The number of commands executed per
   -- tick, if any are in the queue.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in not null Basic_Types.Byte_Array_Access; Restore_On_Set_Up : in Boolean := False; Commands_Dispatched_Per_Tick : in Positive := 3) is
      Expected_Size : Natural := Crc_Length;
   begin
      -- Compute the expected size of the store from the description, and check the
      -- configuration of each entry:
      if Self.Store_Description.Save_Time /= No_Time then
         Expected_Size := @ + Time_Length;
      end if;
      for Item of Self.Store_Description.Entries.all loop
         Expected_Size := @ + Item.Size;
         if Item.Store_Timestamp then
            Expected_Size := @ + Time_Length;
         end if;
         -- An entry may only be restored with the stored data product time if that
         -- time is actually stored:
         pragma Assert (Item.Restore_Time /= Use_Stored_Dp_Time or else Item.Store_Timestamp);
         -- An entry may only be restored with the save time if a save time is
         -- actually stored:
         pragma Assert (Item.Restore_Time /= Use_Save_Time or else Self.Store_Description.Save_Time /= No_Time);
      end loop;
      -- The store size found in the description must match the size computed from
      -- the entries:
      pragma Assert (Expected_Size = Self.Store_Description.Store_Size);
      -- The provided byte array must be large enough to hold the store:
      pragma Assert (Bytes.all'Length >= Self.Store_Description.Store_Size);

      -- Store the configuration:
      Self.Bytes := Bytes;
      Self.Restore_On_Set_Up := Restore_On_Set_Up;
      Self.Commands_Dispatched_Per_Tick := Commands_Dispatched_Per_Tick;
   end Init;

   ---------------------------------------
   -- Private helper subprograms:
   ---------------------------------------

   -- Return the index of the first byte of the store data region, which is the
   -- region covered by the CRC:
   function Data_First (Self : in Instance) return Natural is
      (Self.Bytes.all'First + Crc_Length);

   -- Return the index of the last byte of the store:
   function Store_Last (Self : in Instance) return Natural is
      (Self.Bytes.all'First + Self.Store_Description.Store_Size - 1);

   -- Compute the CRC over the store data region:
   function Compute_Store_Crc (Self : in Instance) return Crc_16.Crc_16_Type is
      (Crc_16.Compute_Crc_16 (Self.Bytes.all (Self.Data_First .. Self.Store_Last)));

   -- Read the CRC currently held in the store header:
   function Read_Stored_Crc (Self : in Instance) return Crc_16.Crc_16_Type is
      (Self.Bytes.all (Self.Bytes.all'First .. Self.Bytes.all'First + Crc_Length - 1));

   -- Save the data products into the store, stamping the store with the provided
   -- save time:
   procedure Do_Save (Self : in out Instance; Save_Time : in Sys_Time.T) is
      use Basic_Types;
      use Data_Product_Enums.Fetch_Status;
      -- If the store currently holds valid contents, then slots for data products
      -- that cannot be fetched are left untouched, preserving the last saved
      -- values. Otherwise the slots are zeroed, so that stale memory contents can
      -- never be presented as valid data.
      Store_Was_Valid : constant Boolean := Self.Compute_Store_Crc = Self.Read_Stored_Crc;
      Idx : Natural := Self.Data_First;
   begin
      -- Write the save time if configured:
      if Self.Store_Description.Save_Time /= No_Time then
         Self.Bytes.all (Idx .. Idx + Time_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Save_Time);
         Idx := @ + Time_Length;
      end if;

      -- Save each data product entry:
      for Item of Self.Store_Description.Entries.all loop
         declare
            Slot_Length : constant Natural := Item.Size + (if Item.Store_Timestamp then Time_Length else 0);
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
               when Not_Available | Id_Out_Of_Range =>
                  -- Throw event if configured to do so:
                  if Item.Event_On_Missing then
                     Self.Event_T_Send_If_Connected (Self.Events.Data_Product_Missing_On_Save (Self.Sys_Time_T_Get, (
                        Id => Item.Data_Product_Id)
                     ));
                  end if;
            end case;

            if Save_Slot then
               -- Write the data product (and its timestamp, if configured) into the slot:
               declare
                  Write_Idx : Natural := Idx;
               begin
                  if Item.Store_Timestamp then
                     Self.Bytes.all (Write_Idx .. Write_Idx + Time_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Fetch_Return.The_Data_Product.Header.Time);
                     Write_Idx := @ + Time_Length;
                  end if;
                  Self.Bytes.all (Write_Idx .. Write_Idx + Item.Size - 1) :=
                     Fetch_Return.The_Data_Product.Buffer (Fetch_Return.The_Data_Product.Buffer'First .. Fetch_Return.The_Data_Product.Buffer'First + Item.Size - 1);
               end;
            elsif not Store_Was_Valid then
               -- The data product could not be saved and the existing slot contents
               -- are not trustworthy, so zero the slot:
               Self.Bytes.all (Idx .. Idx + Slot_Length - 1) := [others => 0];
            end if;

            -- Increment the index by the size of the slot:
            Idx := @ + Slot_Length;
         end;
      end loop;

      -- Compute the CRC over the store contents and write it to the header:
      Self.Bytes.all (Self.Bytes.all'First .. Self.Bytes.all'First + Crc_Length - 1) := Self.Compute_Store_Crc;
   end Do_Save;

   -- Restore the data products held in the store into the data product database.
   -- Status is set to True if the restore was performed, or False if the store CRC
   -- did not validate.
   procedure Do_Restore (Self : in out Instance; Status : out Boolean) is
      use Basic_Types;
      Computed_Crc : constant Crc_16.Crc_16_Type := Self.Compute_Store_Crc;
      Store_Crc : constant Crc_16.Crc_16_Type := Self.Read_Stored_Crc;
      Idx : Natural := Self.Data_First;
      Save_Time_Stamp : Sys_Time.T := Sys_Time.Arithmetic.Sys_Time_Zero;
   begin
      Status := False;

      -- Check the CRC prior to restoring. This protects against restoring the
      -- contents of memory that was never written or has been corrupted:
      if Computed_Crc /= Store_Crc then
         Self.Event_T_Send_If_Connected (Self.Events.Store_Crc_Invalid (Self.Sys_Time_T_Get, (
            Computed_Crc => Computed_Crc,
            Stored_Crc => Store_Crc)
         ));
         return;
      end if;

      -- Read the save time if configured:
      if Self.Store_Description.Save_Time /= No_Time then
         Save_Time_Stamp := Sys_Time.Serialization.From_Byte_Array (Self.Bytes.all (Idx .. Idx + Time_Length - 1));
         Idx := @ + Time_Length;
      end if;

      -- Restore each data product entry:
      for Item of Self.Store_Description.Entries.all loop
         declare
            Stored_Dp_Time : Sys_Time.T := Sys_Time.Arithmetic.Sys_Time_Zero;
         begin
            -- Read the stored data product timestamp if configured:
            if Item.Store_Timestamp then
               Stored_Dp_Time := Sys_Time.Serialization.From_Byte_Array (Self.Bytes.all (Idx .. Idx + Time_Length - 1));
               Idx := @ + Time_Length;
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
                  Self.Bytes.all (Idx .. Idx + Item.Size - 1);
               Self.Data_Product_T_Send_If_Connected (The_Data_Product);
               Idx := @ + Item.Size;
            end;
         end;
      end loop;

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Products_Restored (Self.Sys_Time_T_Get));
      Status := True;
   end Do_Restore;

   -- Build and send a packet containing the contents of the store:
   procedure Do_Dump (Self : in out Instance) is
      use Serializer_Types;
      Pkt : Packet.T;
      Stat : constant Serialization_Status := Self.Packets.Stored_Products (Self.Sys_Time_T_Get, Self.Bytes.all (Self.Bytes.all'First .. Self.Store_Last), Pkt);
   begin
      -- This should never fail since both the autocoder and an assertion at Init
      -- guarantee that the store fits within a single packet:
      pragma Assert (Stat = Success);
      -- Send the packet:
      Self.Packet_T_Send_If_Connected (Pkt);
      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Store_Dumped (Self.Sys_Time_T_Get));
   end Do_Dump;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- If the component is configured with Restore_On_Set_Up, then the store
   -- contents are restored into the data product database here, seeding the
   -- database with the values saved before the last reboot. If the store CRC
   -- does not validate, the restore is skipped and an error event is produced.
   overriding procedure Set_Up (Self : in out Instance) is
      Ignore : Boolean;
   begin
      if Self.Restore_On_Set_Up then
         Self.Do_Restore (Ignore);
      end if;
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Each tick received saves the data
   -- products to the store.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Messages_Dispatched : Natural;
   begin
      -- Handle any commands in the queue. Service up to N commands per tick:
      Messages_Dispatched := Self.Dispatch_N (Self.Commands_Dispatched_Per_Tick);
      pragma Assert (Messages_Dispatched <= Self.Commands_Dispatched_Per_Tick);

      -- Save the data products to the store, stamping the store with the
      -- configured save time:
      case Self.Store_Description.Save_Time is
         when Tick_Time =>
            Self.Do_Save (Save_Time => Arg.Time);
         when Current_Time =>
            Self.Do_Save (Save_Time => Self.Sys_Time_T_Get);
         when No_Time =>
            Self.Do_Save (Save_Time => Sys_Time.Arithmetic.Sys_Time_Zero);
      end case;
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
   -- performs the same operation as the receipt of a tick. If the store's save time
   -- is configured as Tick_Time, the current time is used instead, since no tick is
   -- available.
   overriding function Save_Products (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Save using the current time as the save time. Note that if the store is
      -- configured for Tick_Time, the current time is used for a commanded save,
      -- since no tick is available:
      case Self.Store_Description.Save_Time is
         when Tick_Time | Current_Time =>
            Self.Do_Save (Save_Time => Self.Sys_Time_T_Get);
         when No_Time =>
            Self.Do_Save (Save_Time => Sys_Time.Arithmetic.Sys_Time_Zero);
      end case;
      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Products_Saved (Self.Sys_Time_T_Get));
      return Success;
   end Save_Products;

   -- Restore the data product values held in the store back into the data product
   -- database. The store CRC is checked prior to the restore, and the command fails
   -- if the CRC does not validate.
   overriding function Restore_Products (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Restore_Ok : Boolean;
   begin
      Self.Do_Restore (Restore_Ok);
      if Restore_Ok then
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
