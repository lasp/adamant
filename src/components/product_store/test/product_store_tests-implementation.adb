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
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Data_Product.Assertion; use Data_Product.Assertion;
with Data_Product_Fetch.Assertion; use Data_Product_Fetch.Assertion;
with Data_Product_Id.Assertion; use Data_Product_Id.Assertion;
with Invalid_Data_Product_Length.Assertion; use Invalid_Data_Product_Length.Assertion;
with Invalid_Stored_Length.Assertion; use Invalid_Stored_Length.Assertion;
with Copy_Crc_Mismatch_Info.Assertion; use Copy_Crc_Mismatch_Info.Assertion;
with Store_Copy_Info.Assertion; use Store_Copy_Info.Assertion;
with Product_Store_Enums; use Product_Store_Enums.Store_Copy;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Data_Product_Enums; use Data_Product_Enums;
with Test_Assembly_Stored_Products_Backup;
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
      Test_Store_Memory.Store_Bytes_A := [others => 0];
      Test_Store_Memory.Store_Bytes_B := [others => 0];

      -- Initialize the component:
      Self.Tester.Component_Instance.Init (
         Bytes_A => Test_Store_Memory.Store_Bytes_A'Access,
         Bytes_B => Test_Store_Memory.Store_Bytes_B'Access);

      -- Call the component set up method that the assembly would normally call.
      -- This seeds the counter data products (and would restore the store contents
      -- if Restore_On_Set_Up were True):
      Self.Tester.Component_Instance.Set_Up;

      -- Clear the histories populated by the Set_Up seeding so that each test
      -- starts from a clean slate. The seeding itself is verified in
      -- Test_Set_Up_Seeding:
      Self.Tester.Data_Product_T_Recv_Sync_History.Clear;
      Self.Tester.Save_Count_History.Clear;
      Self.Tester.Restore_Count_History.Clear;
      Self.Tester.Crc_Invalid_Count_History.Clear;

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

   -- The contents of a store copy that has never been written (all zeros):
   Zeroed_Store : constant Basic_Types.Byte_Array (0 .. Test_Assembly_Stored_Products_Backup.Store_Size_In_Bytes - 1) := [others => 0];

   -- Build the expected contents of one copy of the store given the values that
   -- should have been saved. The layout matches the test assembly stored products
   -- model: CRC [0 .. 1], save counter [2 .. 5], save time [6 .. 13], then per
   -- entry a stored length byte followed by the timestamp (if configured) and the
   -- value: A length [14], A timestamp [15 .. 22], A value [23 .. 26],
   -- B length [27], B value [28 .. 39], C length [40], C value [41 .. 42].
   -- Entries marked as not written hold all zeros (a zero stored length marks a
   -- never-saved entry):
   function Expected_Store (
      Save_Time : in Sys_Time.T;
      Save_Counter : in Interfaces.Unsigned_32;
      A_Time : in Sys_Time.T := (0, 0);
      A_Value : in Interfaces.Unsigned_32 := 0;
      B_Value : in Tick.T := ((0, 0), 0);
      C_Value : in Interfaces.Unsigned_16 := 0;
      A_Written : in Boolean := True;
      B_Written : in Boolean := True;
      C_Written : in Boolean := True
   ) return Basic_Types.Byte_Array is
      Bytes : Basic_Types.Byte_Array (0 .. Test_Assembly_Stored_Products_Backup.Store_Size_In_Bytes - 1) := [others => 0];
   begin
      Bytes (2 .. 5) := Packed_U32.Serialization.To_Byte_Array ((Value => Save_Counter));
      Bytes (6 .. 13) := Sys_Time.Serialization.To_Byte_Array (Save_Time);
      if A_Written then
         Bytes (14) := Packed_U32.Serialization.Byte_Array'Length;
         Bytes (15 .. 22) := Sys_Time.Serialization.To_Byte_Array (A_Time);
         Bytes (23 .. 26) := Packed_U32.Serialization.To_Byte_Array ((Value => A_Value));
      end if;
      if B_Written then
         Bytes (27) := Tick.Serialization.Byte_Array'Length;
         Bytes (28 .. 39) := Tick.Serialization.To_Byte_Array (B_Value);
      end if;
      if C_Written then
         Bytes (40) := Packed_U16.Serialization.Byte_Array'Length;
         Bytes (41 .. 42) := Packed_U16.Serialization.To_Byte_Array ((Value => C_Value));
      end if;
      Bytes (0 .. 1) := Crc_16.Compute_Crc_16 (Bytes (2 .. Bytes'Last));
      return Bytes;
   end Expected_Store;

   -- Build the expected contents of a store copy holding the nominal data product
   -- values returned by the tester's mocked fetch handler:
   function Expected_Nominal_Store (
      Save_Time : in Sys_Time.T;
      Save_Counter : in Interfaces.Unsigned_32
   ) return Basic_Types.Byte_Array is
      (Expected_Store (Save_Time => Save_Time, Save_Counter => Save_Counter, A_Time => (5, 11), A_Value => 23, B_Value => ((5, 11), 13), C_Value => 33));

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

   -- This unit test tests that the counter data products are seeded with zero at
   -- Set_Up.
   overriding procedure Test_Set_Up_Seeding (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Run Set_Up again (the fixture's seeding was cleared). All three counters
      -- should be seeded with their current values, which are zero:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Save_Count_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Restore_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Restore_Count_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Crc_Invalid_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Crc_Invalid_Count_History.Get (1), (Value => 0));

      -- No restore was configured, so no other data products or events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Set_Up_Seeding;

   -- This unit test tests saving the data products to the store upon receipt of a
   -- tick, verifying that consecutive saves alternate between the two copies.
   overriding procedure Test_Nominal_Save (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Tick_Time : constant Sys_Time.T := (7, 88);
   begin
      -- Send a tick to the component to save the data products:
      T.Tick_T_Send ((Time => Tick_Time, Count => 1));

      -- All three data products should have been fetched:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      Data_Product_Fetch_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get (1), (Id => 100));
      Data_Product_Fetch_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get (2), (Id => 101));
      Data_Product_Fetch_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get (3), (Id => 102));

      -- The store was never written, so the first save lands in copy A with a
      -- save counter of one, stamped with the tick time. Copy B is untouched:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Nominal_Store (Save_Time => Tick_Time, Save_Counter => 1));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Zeroed_Store);

      -- The save counter data product should have been updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Save_Count_History.Get (1), (Value => 1));

      -- No events should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send another tick with a new time. The save lands in copy B with a save
      -- counter of two, and copy A is untouched:
      T.Tick_T_Send ((Time => (8, 99), Count => 2));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 6);
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Expected_Nominal_Store (Save_Time => (8, 99), Save_Counter => 2));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Nominal_Store (Save_Time => Tick_Time, Save_Counter => 1));
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Save_Count_History.Get (2), (Value => 2));

      -- A third tick wraps back around to copy A with a save counter of three:
      T.Tick_T_Send ((Time => (9, 0), Count => 3));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 9);
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Nominal_Store (Save_Time => (9, 0), Save_Counter => 3));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Expected_Nominal_Store (Save_Time => (8, 99), Save_Counter => 2));
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Save_Count_History.Get (3), (Value => 3));
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

      -- The data product history should contain, in order: the Save_Count from the
      -- tick's save, the three restored data products, and the Restore_Count:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      -- A is restored with its own stored timestamp:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Expected_Data_Product (
         Id => 100, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      -- B is restored with the save time:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Expected_Data_Product (
         Id => 101, Timestamp => Tick_Time, Value => Tick.Serialization.To_Byte_Array (((5, 11), 13))));
      -- C is restored with a timestamp of zero:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Expected_Data_Product (
         Id => 102, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));

      -- The restore counter data product should have been updated:
      Natural_Assert.Eq (T.Restore_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Restore_Count_History.Get (1), (Value => 1));

      -- A single Products_Restored event should have been produced, reporting the
      -- restore was performed from copy A with a save counter of one:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_A, Save_Counter => 1));
   end Test_Nominal_Restore;

   -- This unit test tests restoring the data products from the store at Set_Up, both
   -- with a valid and an invalid store.
   overriding procedure Test_Restore_On_Set_Up (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Tick_Time : constant Sys_Time.T := (7, 88);
   begin
      -- Re-initialize the component to restore the store contents at Set_Up:
      T.Component_Instance.Init (
         Bytes_A => Test_Store_Memory.Store_Bytes_A'Access,
         Bytes_B => Test_Store_Memory.Store_Bytes_B'Access,
         Restore_On_Set_Up => True);

      -- The store is zeroed out, so neither copy's CRC should validate and the
      -- restore should be skipped with an error event per copy. The data product
      -- history should contain the three counter seeds and the incremented
      -- Crc_Invalid_Count:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Crc_Invalid_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Crc_Invalid_Count_History.Get (2), (Value => 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Store_Crc_Invalid_History.Get_Count, 2);
      Copy_Crc_Mismatch_Info_Assert.Eq (T.Store_Crc_Invalid_History.Get (1), (
         Copy => Copy_A,
         Computed_Crc => Crc_16.Compute_Crc_16 (Test_Store_Memory.Store_Bytes_A (2 .. Test_Store_Memory.Store_Bytes_A'Last)),
         Expected_Crc => [0, 0]));
      Copy_Crc_Mismatch_Info_Assert.Eq (T.Store_Crc_Invalid_History.Get (2), (
         Copy => Copy_B,
         Computed_Crc => Crc_16.Compute_Crc_16 (Test_Store_Memory.Store_Bytes_B (2 .. Test_Store_Memory.Store_Bytes_B'Last)),
         Expected_Crc => [0, 0]));

      -- Save valid contents into the store via a tick:
      T.Tick_T_Send ((Time => Tick_Time, Count => 1));

      -- Now the restore at Set_Up should succeed. The history gains three more
      -- counter seeds, the three restored products, and the Restore_Count:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 12);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (9), Expected_Data_Product (
         Id => 100, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (10), Expected_Data_Product (
         Id => 101, Timestamp => Tick_Time, Value => Tick.Serialization.To_Byte_Array (((5, 11), 13))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (11), Expected_Data_Product (
         Id => 102, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));
      -- The restore counter history holds the seeds from both Set_Up calls plus
      -- the increment from the successful restore:
      Natural_Assert.Eq (T.Restore_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Restore_Count_History.Get (3), (Value => 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_A, Save_Counter => 1));
   end Test_Restore_On_Set_Up;

   -- This unit test tests the component's response to a restore command when the
   -- store contents are corrupted.
   overriding procedure Test_Crc_Invalid_On_Restore (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Stored_Crc_Before : Crc_16.Crc_16_Type;
   begin
      -- Send a tick to the component to save the data products (into copy A):
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Stored_Crc_Before := Test_Store_Memory.Store_Bytes_A (0 .. 1);

      -- Corrupt a byte within copy A. Copy B has never been written, so no valid
      -- copy remains:
      Test_Store_Memory.Store_Bytes_A (24) := 16#FF#;

      -- Send the restore command, which should fail:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Failure));

      -- No data products should have been restored. The history contains the
      -- Save_Count from the tick and the incremented Crc_Invalid_Count:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Crc_Invalid_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Crc_Invalid_Count_History.Get (1), (Value => 1));

      -- A CRC error event should have been produced for each copy:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Store_Crc_Invalid_History.Get_Count, 2);
      Copy_Crc_Mismatch_Info_Assert.Eq (T.Store_Crc_Invalid_History.Get (1), (
         Copy => Copy_A,
         Computed_Crc => Crc_16.Compute_Crc_16 (Test_Store_Memory.Store_Bytes_A (2 .. Test_Store_Memory.Store_Bytes_A'Last)),
         Expected_Crc => Stored_Crc_Before));
      Copy_Crc_Mismatch_Info_Assert.Eq (T.Store_Crc_Invalid_History.Get (2), (
         Copy => Copy_B,
         Computed_Crc => Crc_16.Compute_Crc_16 (Test_Store_Memory.Store_Bytes_B (2 .. Test_Store_Memory.Store_Bytes_B'Last)),
         Expected_Crc => [0, 0]));
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
      -- the current system time should have been used as the save time instead. The
      -- first save lands in copy A:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Nominal_Store (Save_Time => (3, 17), Save_Counter => 1));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Zeroed_Store);

      -- The save counter data product should have been updated:
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Save_Count_History.Get (1), (Value => 1));

      -- A Products_Saved event should have been produced, reporting the copy
      -- written and its save counter:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Saved_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Saved_History.Get (1), (Copy => Copy_A, Save_Counter => 1));
   end Test_Save_Command;

   -- This unit test tests disabling and enabling the automatic saving of data
   -- products on tick by command.
   overriding procedure Test_Save_On_Tick_Enable_Disable (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Disable saving on tick:
      T.Command_T_Send (T.Commands.Disable_Save_On_Tick);
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Save_On_Tick_Id, Status => Success));
      Natural_Assert.Eq (T.Save_On_Tick_Disabled_History.Get_Count, 1);

      -- Note that the first tick above performed a save, since the disable command
      -- was dispatched after the tick's business logic ran. Subsequent ticks must
      -- not save:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      T.Tick_T_Send ((Time => (8, 0), Count => 2));
      T.Tick_T_Send ((Time => (9, 0), Count => 3));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 1);

      -- A commanded save must still work while saving on tick is disabled. The
      -- tick's save went to copy A, so the commanded save goes to copy B:
      T.Command_T_Send (T.Commands.Save_Products);
      T.Tick_T_Send ((Time => (10, 0), Count => 4));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 6);
      Natural_Assert.Eq (T.Products_Saved_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Saved_History.Get (1), (Copy => Copy_B, Save_Counter => 2));
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 2);

      -- Re-enable saving on tick. The enable command is dispatched by the next
      -- tick (after its business logic, which does not save), so the tick after
      -- that saves:
      T.Command_T_Send (T.Commands.Enable_Save_On_Tick);
      T.Tick_T_Send ((Time => (11, 0), Count => 5));
      Natural_Assert.Eq (T.Save_On_Tick_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 6);
      T.Tick_T_Send ((Time => (12, 0), Count => 6));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 9);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 3);

      -- Check total events: disabled, saved, enabled:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
   end Test_Save_On_Tick_Enable_Disable;

   -- This unit test tests the tick divider, which saves the data products only
   -- every Ticks_Per_Save ticks.
   overriding procedure Test_Ticks_Per_Save (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Re-initialize the component to save every 3 ticks:
      T.Component_Instance.Init (
         Bytes_A => Test_Store_Memory.Store_Bytes_A'Access,
         Bytes_B => Test_Store_Memory.Store_Bytes_B'Access,
         Ticks_Per_Save => 3);

      -- The first tick saves (the divider counter starts elapsed), the next two
      -- do not:
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      T.Tick_T_Send ((Time => (8, 0), Count => 2));
      T.Tick_T_Send ((Time => (9, 0), Count => 3));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 1);

      -- The fourth tick saves again:
      T.Tick_T_Send ((Time => (10, 0), Count => 4));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 6);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 2);

      -- The first save went to copy A and the second to copy B, each holding the
      -- save time of its own save:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Nominal_Store (Save_Time => (7, 88), Save_Counter => 1));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Expected_Nominal_Store (Save_Time => (10, 0), Save_Counter => 2));

      -- Disable saving on tick. The divider is frozen while disabled, so ticks
      -- do not advance it:
      T.Command_T_Send (T.Commands.Disable_Save_On_Tick);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send ((Time => (11, 0), Count => 5));
      T.Tick_T_Send ((Time => (12, 0), Count => 6));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 6);

      -- Re-enable saving on tick. The divider is reset, so a save deterministically
      -- occurs on the very next tick:
      T.Command_T_Send (T.Commands.Enable_Save_On_Tick);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send ((Time => (13, 0), Count => 7));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 9);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 3);

      -- The only events should be the disable and enable events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Save_On_Tick_Disabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Save_On_Tick_Enabled_History.Get_Count, 1);
   end Test_Ticks_Per_Save;

   -- This unit test tests the component's response to a data product that is missing
   -- from the database on save, verifying that slots keep the values from the most
   -- recent valid save and that never-saved slots restore nothing.
   overriding procedure Test_Missing_Data_Product (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Make all data product fetches return Not_Available. The store has never
      -- been written, so all slots of the written copy (A) should be zeroed, with
      -- a valid save counter, save time, and CRC:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Not_Available;
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);

      -- Only A and C are configured to produce an event on missing (B masks it):
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get_Count, 2);
      Data_Product_Id_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get (1), (Id => 100));
      Data_Product_Id_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get (2), (Id => 102));
      -- No copy was ever valid, so all slots hold the never-saved marker:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Store (
         Save_Time => (7, 88), Save_Counter => 1, A_Written => False, B_Written => False, C_Written => False));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Zeroed_Store);

      -- Restore the store. Copy A's CRC is valid, so the restore succeeds, but
      -- every slot holds the never-saved marker, so no data products flow back
      -- into the database:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));
      -- The data product history holds only the Save_Count from the tick and the
      -- Restore_Count - no restored products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Restore_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Restore_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_A, Save_Counter => 1));

      -- Now perform a nominal save to fill copy B with valid contents:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Success;
      T.Tick_T_Send ((Time => (8, 0), Count => 2));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Expected_Nominal_Store (Save_Time => (8, 0), Save_Counter => 2));

      -- Make the fetches fail again. The next save writes copy A, seeding it from
      -- copy B (the most recent valid save), so the slots keep those values with
      -- an updated save counter and save time:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Not_Available;
      T.Tick_T_Send ((Time => (9, 0), Count => 3));
      Natural_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get_Count, 4);
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Nominal_Store (Save_Time => (9, 0), Save_Counter => 3));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Expected_Nominal_Store (Save_Time => (8, 0), Save_Counter => 2));

      -- Check the total event count and save counter:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Save_Count_History.Get (3), (Value => 3));
   end Test_Missing_Data_Product;

   -- This unit test tests the component's response to a data product id reported as
   -- out of range by the database on save, which produces an unmaskable event.
   overriding procedure Test_Id_Out_Of_Range (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Make all data product fetches return Id_Out_Of_Range. Unlike the missing
      -- data product event, this event cannot be masked, so all three entries
      -- produce it - including B, which masks the missing event:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Id_Out_Of_Range;
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_Id_Out_Of_Range_History.Get_Count, 3);
      Data_Product_Id_Assert.Eq (T.Data_Product_Id_Out_Of_Range_History.Get (1), (Id => 100));
      Data_Product_Id_Assert.Eq (T.Data_Product_Id_Out_Of_Range_History.Get (2), (Id => 101));
      Data_Product_Id_Assert.Eq (T.Data_Product_Id_Out_Of_Range_History.Get (3), (Id => 102));

      -- No missing events should have been produced:
      Natural_Assert.Eq (T.Data_Product_Missing_On_Save_History.Get_Count, 0);

      -- All slots of copy A should be marked never-saved, with a valid save
      -- counter, save time, and CRC. Copy B is untouched:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Store (
         Save_Time => (7, 88), Save_Counter => 1, A_Written => False, B_Written => False, C_Written => False));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Zeroed_Store);
   end Test_Id_Out_Of_Range;

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
         Header => (Time => (5, 11), Id => 100, Buffer_Length => 1), Expected_Length => 4));
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (2), (
         Header => (Time => (5, 11), Id => 101, Buffer_Length => 1), Expected_Length => 12));
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (3), (
         Header => (Time => (5, 11), Id => 102, Buffer_Length => 1), Expected_Length => 2));

      -- No copy was ever valid, so all slots of copy A hold the never-saved
      -- marker. Copy B is untouched:
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Store (
         Save_Time => (7, 88), Save_Counter => 1, A_Written => False, B_Written => False, C_Written => False));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Zeroed_Store);
   end Test_Length_Mismatch;

   -- This unit test tests dumping the contents of both store copies into packets by
   -- command.
   overriding procedure Test_Dump_Store (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
   begin
      -- Send a tick to the component to save the data products (into copy A):
      T.Tick_T_Send ((Time => (7, 88), Count => 1));

      -- Send the dump command:
      T.Command_T_Send (T.Commands.Dump_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Store_Id, Status => Success));

      -- Make sure a packet is produced for each copy:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Stored_Products_A_History.Get_Count, 1);
      Natural_Assert.Eq (T.Stored_Products_B_History.Get_Count, 1);

      -- Check the copy A packet header and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Sys_Time_Assert.Eq (Pkt.Header.Time, (3, 17));
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Assembly_Stored_Products_Backup.Store_Size_In_Bytes);
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Test_Store_Memory.Store_Bytes_A);
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Expected_Nominal_Store (
         Save_Time => (7, 88), Save_Counter => 1));

      -- Check the copy B packet header and contents. Copy B has never been
      -- written, so the packet holds zeros:
      Pkt := T.Packet_T_Recv_Sync_History.Get (2);
      Sys_Time_Assert.Eq (Pkt.Header.Time, (3, 17));
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 1);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Assembly_Stored_Products_Backup.Store_Size_In_Bytes);
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Test_Store_Memory.Store_Bytes_B);
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Zeroed_Store);

      -- A single Store_Dumped event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Store_Dumped_History.Get_Count, 1);

      -- Dump again and make sure the sequence counts increment (each packet keeps
      -- its own sequence count):
      T.Command_T_Send (T.Commands.Dump_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (Natural (T.Packet_T_Recv_Sync_History.Get (3).Header.Sequence_Count), 1);
      Natural_Assert.Eq (Natural (T.Packet_T_Recv_Sync_History.Get (4).Header.Sequence_Count), 1);
      Natural_Assert.Eq (T.Store_Dumped_History.Get_Count, 2);
   end Test_Dump_Store;

   -- This unit test tests that a reboot in the middle of a save costs only one save
   -- of freshness, with the restore falling back to the intact copy and subsequent
   -- saves recovering the corrupted copy.
   overriding procedure Test_Mid_Save_Reboot_Recovery (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Save twice, filling copy A (counter 1) and then copy B (counter 2):
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      T.Tick_T_Send ((Time => (8, 0), Count => 2));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 6);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 2);

      -- Simulate a reboot in the middle of the next save: that save would write
      -- copy A (the older copy), so corrupt copy A as a partial write would,
      -- leaving its CRC invalid. Copy B - the only good copy - is untouched, which
      -- is exactly the property the double buffering guarantees:
      Test_Store_Memory.Store_Bytes_A (20) := 16#FF#;

      -- Restore. The restore falls back to copy B, losing only the freshness of
      -- the interrupted save:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));

      -- The data product history holds the two Save_Counts, the three products
      -- restored from copy B (with copy B's save time), and the Restore_Count:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Expected_Data_Product (
         Id => 100, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Expected_Data_Product (
         Id => 101, Timestamp => (8, 0), Value => Tick.Serialization.To_Byte_Array (((5, 11), 13))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Expected_Data_Product (
         Id => 102, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));
      Natural_Assert.Eq (T.Restore_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Restore_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_B, Save_Counter => 2));

      -- The next save recovers the corrupted copy: it writes copy A (the copy not
      -- holding the most recent valid save) with a counter one newer than copy
      -- B's, leaving copy B untouched:
      T.Tick_T_Send ((Time => (9, 0), Count => 3));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Nominal_Store (Save_Time => (9, 0), Save_Counter => 3));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_B, Expected_Nominal_Store (Save_Time => (8, 0), Save_Counter => 2));

      -- A restore now reads from the recovered copy A, which holds the newest
      -- save counter:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 2);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (2), (Copy => Copy_A, Save_Counter => 3));

      -- Check the total event count: two Products_Restored events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
   end Test_Mid_Save_Reboot_Recovery;

   -- This unit test tests that the component operates correctly when given byte
   -- arrays that are larger than the store size and that have nonzero first
   -- indices, verifying that only the first Store_Size bytes of each allocation
   -- are used.
   overriding procedure Test_Offset_Memory_Regions (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Store_Size : constant Natural := Test_Assembly_Stored_Products_Backup.Store_Size_In_Bytes;
      A_First : constant Natural := Test_Store_Memory.Store_Bytes_Offset_A'First;
      B_First : constant Natural := Test_Store_Memory.Store_Bytes_Offset_B'First;
      Zero_Tail_A : constant Basic_Types.Byte_Array (A_First + Store_Size .. Test_Store_Memory.Store_Bytes_Offset_A'Last) := [others => 0];
      Zero_Tail_B : constant Basic_Types.Byte_Array (B_First + Store_Size .. Test_Store_Memory.Store_Bytes_Offset_B'Last) := [others => 0];
   begin
      -- Re-initialize the component with byte arrays that are larger than the
      -- store size and have nonzero first indices:
      Test_Store_Memory.Store_Bytes_Offset_A := [others => 0];
      Test_Store_Memory.Store_Bytes_Offset_B := [others => 0];
      T.Component_Instance.Init (
         Bytes_A => Test_Store_Memory.Store_Bytes_Offset_A'Access,
         Bytes_B => Test_Store_Memory.Store_Bytes_Offset_B'Access);

      -- Save twice, filling copy A and then copy B. Each save must land in the
      -- first Store_Size bytes of its allocation, leaving the rest untouched:
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Byte_Array_Assert.Eq (
         Test_Store_Memory.Store_Bytes_Offset_A (A_First .. A_First + Store_Size - 1),
         Expected_Nominal_Store (Save_Time => (7, 88), Save_Counter => 1));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_Offset_A (Zero_Tail_A'Range), Zero_Tail_A);
      T.Tick_T_Send ((Time => (8, 0), Count => 2));
      Byte_Array_Assert.Eq (
         Test_Store_Memory.Store_Bytes_Offset_B (B_First .. B_First + Store_Size - 1),
         Expected_Nominal_Store (Save_Time => (8, 0), Save_Counter => 2));
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_Offset_B (Zero_Tail_B'Range), Zero_Tail_B);

      -- Restore. The newest valid copy is B, and the products must restore with
      -- the correct values despite the offset array bounds:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Expected_Data_Product (
         Id => 100, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Expected_Data_Product (
         Id => 101, Timestamp => (8, 0), Value => Tick.Serialization.To_Byte_Array (((5, 11), 13))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Expected_Data_Product (
         Id => 102, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_B, Save_Counter => 2));

      -- Dump the store and make sure each packet holds the first Store_Size bytes
      -- of its allocation:
      T.Command_T_Send (T.Commands.Dump_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Byte_Array_Assert.Eq (
         T.Packet_T_Recv_Sync_History.Get (1).Buffer (0 .. Store_Size - 1),
         Test_Store_Memory.Store_Bytes_Offset_A (A_First .. A_First + Store_Size - 1));
      Byte_Array_Assert.Eq (
         T.Packet_T_Recv_Sync_History.Get (2).Buffer (0 .. Store_Size - 1),
         Test_Store_Memory.Store_Bytes_Offset_B (B_First .. B_First + Store_Size - 1));

      -- Check the total event count: Products_Restored and Store_Dumped:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Store_Dumped_History.Get_Count, 1);
   end Test_Offset_Memory_Regions;

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

   -- This unit test tests that a restore silently skips store entries that have
   -- never been saved, leaving those data products unavailable in the database.
   overriding procedure Test_Restore_Skips_Unwritten (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Make fetches of data product B alone fail, and save on a fresh store. A
      -- and C are saved, while B's slot keeps its never-saved marker. B masks the
      -- missing event, so no events are produced:
      T.Fetch_Fail_Id := 101;
      T.Tick_T_Send ((Time => (7, 88), Count => 1));
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Byte_Array_Assert.Eq (Test_Store_Memory.Store_Bytes_A, Expected_Store (
         Save_Time => (7, 88), Save_Counter => 1, A_Time => (5, 11), A_Value => 23, C_Value => 33, B_Written => False));

      -- Restore. A and C are restored; B is skipped silently since it has never
      -- been saved:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));

      -- The data product history holds the Save_Count, restored A and C (no B),
      -- and the Restore_Count:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Expected_Data_Product (
         Id => 100, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Expected_Data_Product (
         Id => 102, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));

      -- Only the Products_Restored event should have been produced - the skipped
      -- entry is silent:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_A, Save_Counter => 1));
      Natural_Assert.Eq (T.Restore_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Restore_Count_History.Get (1), (Value => 1));
   end Test_Restore_Skips_Unwritten;

   -- This unit test tests that a restore refuses a store entry whose stored length
   -- does not match the expected length, which indicates the stored products model
   -- has changed since the store was written.
   overriding procedure Test_Stored_Length_Mismatch (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Save the data products nominally (into copy A):
      T.Tick_T_Send ((Time => (7, 88), Count => 1));

      -- Simulate a stored products model change across boots by hand-writing a
      -- different (nonzero) stored length for B, and recomputing a valid CRC over
      -- the modified contents:
      Test_Store_Memory.Store_Bytes_A (27) := 5;
      Test_Store_Memory.Store_Bytes_A (0 .. 1) := Crc_16.Compute_Crc_16 (
         Test_Store_Memory.Store_Bytes_A (2 .. Test_Store_Memory.Store_Bytes_A'Last));

      -- Restore. The CRC validates, so A and C are restored, but B's stored
      -- length does not match and it is refused with an event:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));

      -- The data product history holds the Save_Count, restored A and C (no B),
      -- and the Restore_Count:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Expected_Data_Product (
         Id => 100, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Expected_Data_Product (
         Id => 102, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));

      -- A Stored_Length_Mismatch event should have been produced for B, along
      -- with the Products_Restored event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Stored_Length_Mismatch_History.Get_Count, 1);
      Invalid_Stored_Length_Assert.Eq (T.Stored_Length_Mismatch_History.Get (1), (
         Id => 101, Stored_Length => 5, Expected_Length => 12));
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_A, Save_Counter => 1));
   end Test_Stored_Length_Mismatch;

end Product_Store_Tests.Implementation;
