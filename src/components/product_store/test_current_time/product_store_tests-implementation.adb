--------------------------------------------------------------------------------
-- Product_Store Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Basic_Types;
with Crc_16;
with Sys_Time;
with Interfaces;
with Data_Product_Types;
with Packed_U32;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Data_Product.Assertion; use Data_Product.Assertion;
with Store_Copy_Info.Assertion; use Store_Copy_Info.Assertion;
with Product_Store_Enums; use Product_Store_Enums.Store_Copy;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Test_Assembly_Ct_Stored_Products;
with Test_Store_Memory_Ct;

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
      Test_Store_Memory_Ct.Store_Bytes_A := [others => 0];
      Test_Store_Memory_Ct.Store_Bytes_B := [others => 0];

      -- Initialize the component:
      Self.Tester.Component_Instance.Init (
         Bytes_A => Test_Store_Memory_Ct.Store_Bytes_A'Access,
         Bytes_B => Test_Store_Memory_Ct.Store_Bytes_B'Access);

      -- Call the component set up method that the assembly would normally call.
      -- This seeds the counter data products:
      Self.Tester.Component_Instance.Set_Up;

      -- Clear the histories populated by the Set_Up seeding so that each test
      -- starts from a clean slate:
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

   -- Build the expected contents of one copy of the store given the values that
   -- should have been saved. The layout matches the test assembly stored products
   -- model: CRC [0 .. 1], save counter [2 .. 5], save time [6 .. 13], data
   -- product A stored length [14], data product A value [15 .. 18].
   function Expected_Store (
      Save_Time : in Sys_Time.T;
      Save_Counter : in Interfaces.Unsigned_32;
      A_Value : in Interfaces.Unsigned_32
   ) return Basic_Types.Byte_Array is
      Bytes : Basic_Types.Byte_Array (0 .. Test_Assembly_Ct_Stored_Products.Store_Size_In_Bytes - 1) := [others => 0];
   begin
      Bytes (2 .. 5) := Packed_U32.Serialization.To_Byte_Array ((Value => Save_Counter));
      Bytes (6 .. 13) := Sys_Time.Serialization.To_Byte_Array (Save_Time);
      Bytes (14) := Packed_U32.Serialization.Byte_Array'Length;
      Bytes (15 .. 18) := Packed_U32.Serialization.To_Byte_Array ((Value => A_Value));
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

   -- This unit test tests saving the data products to a store configured to save
   -- the current time upon receipt of a tick, and restoring them with the save
   -- time.
   overriding procedure Test_Save_Current_Time (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send a tick to the component. The store is configured for Current_Time, so
      -- the save time should come from the system time connector, not the tick:
      T.Tick_T_Send ((Time => (7, 88), Count => 1));

      -- The data product should have been fetched:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 1);

      -- The first save lands in copy A, which should contain the data product
      -- stamped with the current system time rather than the tick time. Copy B
      -- is untouched:
      Byte_Array_Assert.Eq (Test_Store_Memory_Ct.Store_Bytes_A, Expected_Store (
         Save_Time => (3, 17), Save_Counter => 1, A_Value => 23));
      Byte_Array_Assert.Eq (Test_Store_Memory_Ct.Store_Bytes_B,
         [0 .. Test_Assembly_Ct_Stored_Products.Store_Size_In_Bytes - 1 => 0]);

      -- The save counter data product should have been updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Save_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Save_Count_History.Get (1), (Value => 1));

      -- No events should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Restore the data products and make sure A is restored with the save time:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));

      -- The data product history holds the Save_Count, the restored product, and
      -- the Restore_Count:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Expected_Data_Product (
         Id => 100, Timestamp => (3, 17), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      Natural_Assert.Eq (T.Restore_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Restore_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
      Store_Copy_Info_Assert.Eq (T.Products_Restored_History.Get (1), (Copy => Copy_A, Save_Counter => 1));
   end Test_Save_Current_Time;

end Product_Store_Tests.Implementation;
