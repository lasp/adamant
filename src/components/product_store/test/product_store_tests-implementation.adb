--------------------------------------------------------------------------------
-- Product_Store Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Basic_Types;
with Crc_16;
with Sys_Time.Assertion; use Sys_Time.Assertion;
with Tick;
with Packet;
with Command;
with Command_Types;
with Interfaces;
with Data_Product_Types;
with Packed_U32;
with Packed_U16;
with Data_Product.Assertion; use Data_Product.Assertion;
with Data_Product_Fetch.Assertion; use Data_Product_Fetch.Assertion;
with Data_Product_Id.Assertion; use Data_Product_Id.Assertion;
with Invalid_Data_Product_Length.Assertion; use Invalid_Data_Product_Length.Assertion;
with Store_Crc_Error.Assertion; use Store_Crc_Error.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Data_Product_Enums; use Data_Product_Enums;
with Test_Assembly_Stored_Products;
with Test_Store_Memory;

package body Product_Store_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Zero out the store memory so that each test starts fresh:
      Test_Store_Memory.Store_Bytes := [others => 0];

      -- Initialize the component:
      Self.Tester.Component_Instance.Init (Bytes => Test_Store_Memory.Store_Bytes'Access);

      -- Call the component set up method that the assembly would normally call.
      -- This does nothing unless Restore_On_Set_Up is set to True:
      Self.Tester.Component_Instance.Set_Up;

      -- Set the desired time for the tests:
      Self.Tester.System_Time := (3, 17);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helper functions:
   -------------------------------------------------------------------------

   -- Build the expected contents of the store given the values that should have
   -- been saved. The layout matches the test assembly stored products model:
   -- CRC [0 .. 1], save time [2 .. 9], data product A timestamp [10 .. 17],
   -- data product A [18 .. 21], data product B [22 .. 33], data product C [34 .. 35].
   function Expected_Store (
      Save_Time : in Sys_Time.T;
      A_Time : in Sys_Time.T;
      A_Value : in Interfaces.Unsigned_32;
      B_Value : in Tick.T;
      C_Value : in Interfaces.Unsigned_16
   ) return Basic_Types.Byte_Array is
      Bytes : Basic_Types.Byte_Array (0 .. Test_Assembly_Stored_Products.Store_Size_In_Bytes - 1) := [others => 0];
   begin
      Bytes (2 .. 9) := Sys_Time.Serialization.To_Byte_Array (Save_Time);
      Bytes (10 .. 17) := Sys_Time.Serialization.To_Byte_Array (A_Time);
      Bytes (18 .. 21) := Packed_U32.Serialization.To_Byte_Array ((Value => A_Value));
      Bytes (22 .. 33) := Tick.Serialization.To_Byte_Array (B_Value);
      Bytes (34 .. 35) := Packed_U16.Serialization.To_Byte_Array ((Value => C_Value));
      Bytes (0 .. 1) := Crc_16.Compute_Crc_16 (Bytes (2 .. Bytes'Last));
      return Bytes;
   end Expected_Store;

   -- Build the expected data product produced by a restore:
   function Expected_Data_Product (
      Id : in Data_Product_Types.Data_Product_Id;
      Timestamp : in Sys_Time.T;
      Value : in Basic_Types.Byte_Array
   ) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Time => Timestamp, Id => Id, Buffer_Length => Value'Length), Buffer => [others => 0]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Value'Length - 1) := Value;
      return Dp;
   end Expected_Data_Product;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   -- This unit test tests saving the data products to the store upon receipt of a
   -- tick.
   overriding procedure Test_Nominal_Save (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Tick_Time : constant Sys_Time.T := (7, 88);
   begin
      -- Send a tick to the component to save the data products:
      T.Tick_T_Send ((Time => Tick_Time, Count => 1));

      -- All three data products should have been fetched:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      Data_Product_Fetch_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get (1), (Id => 1));
      Data_Product_Fetch_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get (2), (Id => 2));
      Data_Product_Fetch_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get (3), (Id => 3));

      -- The store should contain the data products stamped with the tick time:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => Tick_Time, A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));

      -- No events should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send another tick with a new time and make sure the save time is updated:
      T.Tick_T_Send ((Time => (8, 99), Count => 2));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 6);
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => (8, 99), A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Nominal_Save;

   -- This unit test tests restoring the data products from the store by command,
   -- including all of the restore timestamp modes.
   overriding procedure Test_Nominal_Restore (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Tick_Time : constant Sys_Time.T := (7, 88);
   begin
      -- Send a tick to the component to save the data products:
      T.Tick_T_Send ((Time => Tick_Time, Count => 1));

      -- Send the restore command:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));

      -- Three data products should have been sent to the database, each with the
      -- timestamp determined by its restore_time configuration:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      -- A is restored with its own stored timestamp:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (1), Expected_Data_Product (
         Id => 1, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      -- B is restored with the save time:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Expected_Data_Product (
         Id => 2, Timestamp => Tick_Time, Value => Tick.Serialization.To_Byte_Array (((5, 11), 13))));
      -- C is restored with a timestamp of zero:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Expected_Data_Product (
         Id => 3, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));

      -- A single Products_Restored event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
   end Test_Nominal_Restore;

   -- This unit test tests restoring the data products from the store at Set_Up, both
   -- with a valid and an invalid store.
   overriding procedure Test_Restore_On_Set_Up (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Tick_Time : constant Sys_Time.T := (7, 88);
   begin
      -- Re-initialize the component to restore the store contents at Set_Up:
      T.Component_Instance.Init (Bytes => Test_Store_Memory.Store_Bytes'Access, Restore_On_Set_Up => True);

      -- The store is zeroed out, so the CRC should not validate and the restore
      -- should be skipped with an error event:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Store_Crc_Invalid_History.Get_Count, 1);
      Store_Crc_Error_Assert.Eq (T.Store_Crc_Invalid_History.Get (1), (
         Computed_Crc => Crc_16.Compute_Crc_16 (Test_Store_Memory.Store_Bytes (2 .. Test_Store_Memory.Store_Bytes'Last)),
         Stored_Crc => [0, 0]));

      -- Save valid contents into the store via a tick:
      T.Tick_T_Send ((Time => Tick_Time, Count => 1));

      -- Now the restore at Set_Up should succeed:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (1), Expected_Data_Product (
         Id => 1, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Expected_Data_Product (
         Id => 2, Timestamp => Tick_Time, Value => Tick.Serialization.To_Byte_Array (((5, 11), 13))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Expected_Data_Product (
         Id => 3, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
   end Test_Restore_On_Set_Up;

   -- This unit test tests the component's response to a restore command when the
   -- store contents are corrupted.
   overriding procedure Test_Crc_Invalid_On_Restore (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Stored_Crc_Before : Crc_16.Crc_16_Type;
   begin
      -- Send a tick to the component to save the data products:
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Stored_Crc_Before := Test_Store_Memory.Store_Bytes (0 .. 1);

      -- Corrupt a byte within the store:
      Test_Store_Memory.Store_Bytes (20) := 16#FF#;

      -- Send the restore command, which should fail:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Failure));

      -- No data products should have been restored:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- A CRC error event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Store_Crc_Invalid_History.Get_Count, 1);
      Store_Crc_Error_Assert.Eq (T.Store_Crc_Invalid_History.Get (1), (
         Computed_Crc => Crc_16.Compute_Crc_16 (Test_Store_Memory.Store_Bytes (2 .. Test_Store_Memory.Store_Bytes'Last)),
         Stored_Crc => Stored_Crc_Before));
   end Test_Crc_Invalid_On_Restore;

   -- This unit test tests saving the data products to the store by command.
   overriding procedure Test_Save_Command (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send the save command:
      T.Command_T_Send (T.Commands.Save_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Save_Products_Id, Status => Success));

      -- All three data products should have been fetched:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);

      -- The store is configured for Tick_Time, but a commanded save has no tick, so
      -- the current system time should have been used as the save time instead:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => (3, 17), A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));

      -- A single Products_Saved event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Saved_History.Get_Count, 1);
   end Test_Save_Command;

   -- This unit test tests the component's response to a data product that is missing
   -- from the database on save, both when the existing store contents are valid and
   -- when they are not.
   overriding procedure Test_Missing_Data_Product (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Make all data product fetches return Not_Available. The store has never
      -- been written (invalid CRC), so the slots should be zeroed out:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Not_Available;
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);

      -- Only A and C are configured to produce an event on missing:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get_Count, 2);
      Data_Product_Id_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get (1), (Id => 1));
      Data_Product_Id_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get (2), (Id => 3));

      -- The store should contain zeroed slots with a valid save time and CRC:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => (7, 88), A_Time => (0, 0), A_Value => 0, B_Value => ((0, 0), 0), C_Value => 0));

      -- Now perform a nominal save to fill the store with valid contents:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Success;
      T.Tick_T_Send ((Time => (8, 0), Count => 2));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => (8, 0), A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));

      -- Make the fetches fail again. This time the store contents are valid, so the
      -- previously saved values should be preserved, with an updated save time:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Not_Available;
      T.Tick_T_Send ((Time => (9, 0), Count => 3));
      Natural_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get_Count, 4);
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => (9, 0), A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));

      -- An Id_Out_Of_Range fetch status should behave the same way:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Id_Out_Of_Range;
      T.Tick_T_Send ((Time => (10, 0), Count => 4));
      Natural_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get_Count, 6);
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => (10, 0), A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));

      -- Check the total event count:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
   end Test_Missing_Data_Product;

   -- This unit test tests the component's response to a fetched data product with an
   -- unexpected length.
   overriding procedure Test_Length_Mismatch (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Make all data product fetches return a length of 1, which does not match
      -- any of the configured entries:
      T.Data_Product_Length_Override := 1;
      T.Tick_T_Send ((Time => (7, 88), Count => 1));

      -- A length mismatch event should have been produced for each entry:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get_Count, 3);
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (1), (
         Header => (Time => (5, 11), Id => 1, Buffer_Length => 1), Expected_Length => 4));
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (2), (
         Header => (Time => (5, 11), Id => 2, Buffer_Length => 1), Expected_Length => 12));
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (3), (
         Header => (Time => (5, 11), Id => 3, Buffer_Length => 1), Expected_Length => 2));

      -- The store was never valid, so the slots should have been zeroed:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes, Expected_Store (
         Save_Time => (7, 88), A_Time => (0, 0), A_Value => 0, B_Value => ((0, 0), 0), C_Value => 0));
   end Test_Length_Mismatch;

   -- This unit test tests dumping the contents of the store into a packet by
   -- command.
   overriding procedure Test_Dump_Store (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
   begin
      -- Send a tick to the component to save the data products:
      T.Tick_T_Send ((Time => (7, 88), Count => 1));

      -- Send the dump command:
      T.Command_T_Send (T.Commands.Dump_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Store_Id, Status => Success));

      -- Make sure a packet is produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Stored_Products_History.Get_Count, 1);

      -- Check the packet header and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Sys_Time_Assert.Eq (Pkt.Header.Time, (3, 17));
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Assembly_Stored_Products.Store_Size_In_Bytes);
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Test_Store_Memory.Store_Bytes);
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Expected_Store (
         Save_Time => (7, 88), A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));

      -- A single Store_Dumped event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Store_Dumped_History.Get_Count, 1);

      -- Dump again and make sure the sequence count increments:
      T.Command_T_Send (T.Commands.Dump_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Packet_T_Recv_Sync_History.Get (2).Header.Sequence_Count), 1);
      Natural_Assert.Eq (T.Store_Dumped_History.Get_Count, 2);
   end Test_Dump_Store;

   -- This unit test tests the component's response to an invalid command.
   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Save_Products;
   begin
      -- Corrupt the command by giving it an unexpected argument length:
      Cmd.Header.Arg_Buffer_Length := 22;

      -- Send the corrupted command:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Save_Products_Id, Status => Length_Error));

      -- An invalid command event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (
         Id => T.Commands.Get_Save_Products_Id,
         Errant_Field_Number => Interfaces.Unsigned_32'Last,
         Errant_Field => [0, 0, 0, 0, 0, 0, 0, 22]));

      -- No save should have been performed:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 0);
   end Test_Invalid_Command;

   -- This unit test tests a command being dropped due to a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Define full sized command, which fills an entire queue element:
      Buffer : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 92];
      A_Command : constant Command.T := ((Source_Id => 0, Id => 15, Arg_Buffer_Length => Buffer'Length), Arg_Buffer => Buffer);
   begin
      -- Fill the queue. The queue was sized to hold exactly three full sized
      -- commands in Set_Up_Test:
      for Idx in 1 .. 3 loop
         T.Command_T_Send (A_Command);
      end loop;

      -- Send another command and expect it to be dropped with an event:
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (A_Command);
      Natural_Assert.Eq (T.Command_T_Send_Dropped_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Dropped_Command_History.Get (1), A_Command.Header);

      -- Send another command and expect it to be dropped with an event:
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (A_Command);
      Natural_Assert.Eq (T.Command_T_Send_Dropped_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dropped_Command_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Dropped_Command_History.Get (2), A_Command.Header);
   end Test_Full_Queue;

end Product_Store_Tests.Implementation;
