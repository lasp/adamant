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
with Packed_U16;
with Data_Product.Assertion; use Data_Product.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Test_Assembly_Nt_Stored_Products;
with Test_Store_Memory_Nt;

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
      Test_Store_Memory_Nt.Store_Bytes := [others => 0];

      -- Initialize the component:
      Self.Tester.Component_Instance.Init (Bytes => Test_Store_Memory_Nt.Store_Bytes'Access);

      -- Call the component set up method that the assembly would normally call:
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
   -- been saved. The layout matches the test assembly stored products model, which
   -- is configured without a save time: CRC [0 .. 1], data product A timestamp
   -- [2 .. 9], data product A [10 .. 13], data product C [14 .. 15].
   function Expected_Store (
      A_Time : in Sys_Time.T;
      A_Value : in Interfaces.Unsigned_32;
      C_Value : in Interfaces.Unsigned_16
   ) return Basic_Types.Byte_Array is
      Bytes : Basic_Types.Byte_Array (0 .. Test_Assembly_Nt_Stored_Products.Store_Size_In_Bytes - 1) := [others => 0];
   begin
      Bytes (2 .. 9) := Sys_Time.Serialization.To_Byte_Array (A_Time);
      Bytes (10 .. 13) := Packed_U32.Serialization.To_Byte_Array ((Value => A_Value));
      Bytes (14 .. 15) := Packed_U16.Serialization.To_Byte_Array ((Value => C_Value));
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

   -- This unit test tests saving the data products to a store configured without a
   -- save time upon receipt of a tick.
   overriding procedure Test_Save_No_Time (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send a tick to the component to save the data products:
      T.Tick_T_Send ((Time => (7, 88), Count => 1));

      -- Both data products should have been fetched:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 2);

      -- The store should contain the data products with no save time in the header:
      Byte_Array_Assert.Eq (Test_Store_Memory_Nt.Store_Bytes, Expected_Store (
         A_Time => (5, 11), A_Value => 23, C_Value => 33));

      -- No events should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Save_No_Time;

   -- This unit test tests restoring the data products from a store configured
   -- without a save time.
   overriding procedure Test_Restore_No_Time (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send a tick to the component to save the data products:
      T.Tick_T_Send ((Time => (7, 88), Count => 1));

      -- Send the restore command:
      T.Command_T_Send (T.Commands.Restore_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Restore_Products_Id, Status => Success));

      -- Both data products should have been sent to the database:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      -- A is restored with its own stored timestamp:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (1), Expected_Data_Product (
         Id => 1, Timestamp => (5, 11), Value => Packed_U32.Serialization.To_Byte_Array ((Value => 23))));
      -- C is restored with a timestamp of zero:
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Expected_Data_Product (
         Id => 3, Timestamp => (0, 0), Value => Packed_U16.Serialization.To_Byte_Array ((Value => 33))));

      -- A single Products_Restored event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Restored_History.Get_Count, 1);
   end Test_Restore_No_Time;

   -- This unit test tests saving the data products to a store configured without a
   -- save time by command.
   overriding procedure Test_Save_Command_No_Time (Self : in out Instance) is
      T : Component.Product_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send the save command:
      T.Command_T_Send (T.Commands.Save_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Save_Products_Id, Status => Success));

      -- Both data products should have been fetched and saved:
      Natural_Assert.Eq (T.Data_Product_Fetch_T_Service_History.Get_Count, 2);
      Byte_Array_Assert.Eq (Test_Store_Memory_Nt.Store_Bytes, Expected_Store (
         A_Time => (5, 11), A_Value => 23, C_Value => 33));

      -- A single Products_Saved event should have been produced:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Products_Saved_History.Get_Count, 1);
   end Test_Save_Command_No_Time;

end Product_Store_Tests.Implementation;
