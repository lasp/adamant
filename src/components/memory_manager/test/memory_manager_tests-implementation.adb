--------------------------------------------------------------------------------
-- Memory_Manager Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces;
with Memory_Region_Request.Assertion; use Memory_Region_Request.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Memory_Manager_Enums; use Memory_Manager_Enums.Memory_Request_Status;
use Memory_Manager_Enums.Memory_State;
with Virtual_Memory_Region_Crc.Assertion; use Virtual_Memory_Region_Crc.Assertion;
with Virtual_Memory_Region.Assertion; use Virtual_Memory_Region.Assertion;
with Virtual_Memory_Region_Positive.Assertion; use Virtual_Memory_Region_Positive.Assertion;
with Virtual_Memory_Region_Write;
with Invalid_Virtual_Memory_Region.Assertion; use Invalid_Virtual_Memory_Region.Assertion;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Memory_Manager_State.Assertion; use Memory_Manager_State.Assertion;
with Ided_Memory_Region.Assertion; use Ided_Memory_Region.Assertion;
with Byte_Array_Pointer.Packed;
with System.Storage_Elements;
with Memory_Packetizer_Types;
with Byte_Array_Pointer;
with Crc_16;
with Serializer_Types;

package body Memory_Manager_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals to aid in testing:
   -------------------------------------------------------------------------
   -- 100 byte region. By declaring this outside the component we can
   -- easily view the internal memory region.
   Memory : aliased Basic_Types.Byte_Array := [0 .. 99 => 0];

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Reset memory to zeros:
      Memory := [others => 0];

      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Bytes => Memory'Unchecked_Access, Size => -1);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Init (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal_Heap is
      begin
         T.Component_Instance.Init (Size => 50);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init heap failed!");
      end Init_Nominal_Heap;

      procedure Init_Nominal_Static is
      begin
         T.Component_Instance.Init (Bytes => Memory'Unchecked_Access);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init static failed!");
      end Init_Nominal_Static;

      procedure Init_Everything is
      begin
         T.Component_Instance.Init (Bytes => Memory'Unchecked_Access, Size => 50);
         -- Should never get here:
         Assert (False, "Init everything did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Everything;

      procedure Init_Nothing is
      begin
         T.Component_Instance.Init;
         -- Should never get here:
         Assert (False, "Init nothing did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Nothing;
   begin
      Init_Nominal_Heap;
      Init_Nominal_Static;
      Init_Everything;
      Init_Nothing;
   end Test_Init;

   overriding procedure Test_Nominal_Request_Release (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Request : Memory_Region_Request.T;
   begin
      -- Check initial data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Location_History.Get_Count, 1);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (1), (Region => (Address => 0, Length => 0), Crc => [0, 0]));
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (1), (State => Available));
      Memory_Region_Assert.Eq (T.Memory_Location_History.Get (1), (Memory'Address, Memory'Length));

      -- Request the memory region:
      Request := T.Memory_Region_Request_T_Get;
      Memory_Region_Request_Assert.Eq (Request, ((Id => 0, Region => (Memory'Address, Memory'Length)), Status => Success));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 2);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (2), (State => In_Use));

      -- Check the memory region
      Byte_Array_Assert.Eq (Memory, [0 .. Memory'Length - 1 => 0]);

      -- Fill the pointer:
      declare
         use Byte_Array_Pointer.Packed;
         Ptr : constant Byte_Array_Pointer.Instance := Unpack (Request.Ided_Region.Region);
      begin
         Byte_Array_Pointer.Copy_To (Ptr, [0 .. Memory'Length - 1 => 55]);
      end;

      -- Check the memory region:
      Byte_Array_Assert.Eq (Memory, [0 .. Memory'Length - 1 => 55]);

      -- Release the memory region:
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Check the data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 3);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (3), (State => Available));

      -- Request the memory region:
      Request := T.Memory_Region_Request_T_Get;
      Memory_Region_Request_Assert.Eq (Request, ((Id => 1, Region => (Memory'Address, Memory'Length)), Status => Success));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 4);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (4), (State => In_Use));

      -- Check the memory region
      Byte_Array_Assert.Eq (Memory, [0 .. Memory'Length - 1 => 55]);

      -- Fill the pointer:
      declare
         use Byte_Array_Pointer.Packed;
         Ptr : constant Byte_Array_Pointer.Instance := Unpack (Request.Ided_Region.Region);
      begin
         Byte_Array_Pointer.Copy_To (Ptr, [0 .. Memory'Length - 1 => 22]);
      end;

      -- Check the memory region:
      Byte_Array_Assert.Eq (Memory, [0 .. Memory'Length - 1 => 22]);

      -- Release the memory region:
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Check the data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 5);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (5), (State => Available));

      -- Expect no error events to have been thrown during this test:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Nominal_Request_Release;

   overriding procedure Test_Off_Nominal_Request_Release (Self : in out Instance) is
      use System.Storage_Elements;
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Request : Memory_Region_Request.T := ((Id => 1, Region => (Memory'Address, Memory'Length)), Status => Success);
      Bad_Request : Memory_Region_Request.T;
   begin
      -- Check initial data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Location_History.Get_Count, 1);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (1), (Region => (Address => 0, Length => 0), Crc => [0, 0]));
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (1), (State => Available));
      Memory_Region_Assert.Eq (T.Memory_Location_History.Get (1), (Memory'Address, Memory'Length));

      --
      -- Release the memory region before anything is allocated.
      --
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Expect an error event.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Already_Released_History.Get_Count, 1);
      Ided_Memory_Region_Assert.Eq (T.Memory_Already_Released_History.Get (1), (Id => 1, Region => (Memory'Address, Memory'Length)));

      --
      -- Request the memory region:
      --
      Request := T.Memory_Region_Request_T_Get;
      Memory_Region_Request_Assert.Eq (Request, ((Id => 0, Region => (Memory'Address, Memory'Length)), Status => Success));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 2);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (2), (State => In_Use));

      -- Check the memory region
      Byte_Array_Assert.Eq (Memory, [0 .. Memory'Length - 1 => 0]);

      -- Fill the pointer:
      declare
         use Byte_Array_Pointer.Packed;
         Ptr : constant Byte_Array_Pointer.Instance := Unpack (Request.Ided_Region.Region);
      begin
         Byte_Array_Pointer.Copy_To (Ptr, [0 .. Memory'Length - 1 => 22]);
      end;

      -- Check the memory region:
      Byte_Array_Assert.Eq (Memory, [0 .. Memory'Length - 1 => 22]);

      --
      -- Request the memory region again:
      --
      Bad_Request := T.Memory_Region_Request_T_Get;
      Memory_Region_Request_Assert.Eq (Bad_Request, ((Id => 0, Region => (To_Address (Integer_Address (0)), 0)), Status => Failure));

      -- Expect an error event.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Unavailable_History.Get_Count, 1);

      --
      -- Release bad ID
      --
      Request.Ided_Region.Id := 99;
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Expect an error event.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unexpected_Memory_Id_History.Get_Count, 1);
      Ided_Memory_Region_Assert.Eq (T.Unexpected_Memory_Id_History.Get (1), Request.Ided_Region);

      --
      -- Release good memory region:
      --
      Request.Ided_Region.Id := 0;
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Check the data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 3);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (3), (State => Available));

      --
      -- Release the memory region before anything is allocated.
      --
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Expect an error event.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Already_Released_History.Get_Count, 2);
      Ided_Memory_Region_Assert.Eq (T.Memory_Already_Released_History.Get (2), Request.Ided_Region);
   end Test_Off_Nominal_Request_Release;

   overriding procedure Test_Nominal_Memory_Dump (Self : in out Instance) is
      use Byte_Array_Pointer;
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Dump : Memory_Packetizer_Types.Memory_Dump;
   begin
      -- Set bytes to pattern to make testing easier:
      Memory := [0 => 0, 1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 6, 7 => 7, 8 => 8, 9 => 9, 10 => 10, 11 => 11, 12 => 12, 13 => 13, 14 => 14, 15 => 15, 16 .. 50 => 33, 51 .. 99 => 55];

      -- Send command to dump region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 1);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (1), (Address => 0, Length => Memory'Length));

      -- Make sure region is dumped:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 1);
      Dump := T.Memory_Dump_Recv_Sync_History.Get (1);

      -- Check dump id and data:
      Natural_Assert.Eq (Natural (Dump.Id), Natural (T.Packets.Get_Memory_Region_Packet_Id));
      Byte_Array_Assert.Eq (To_Byte_Array (Dump.Memory_Pointer), Memory);

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region_Bytes ((Address => 0, Length => 5)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Bytes_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 2);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (2), (Address => 0, Length => 5));

      -- Make sure region is dumped:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 2);
      Dump := T.Memory_Dump_Recv_Sync_History.Get (2);

      -- Check dump id and data:
      Natural_Assert.Eq (Natural (Dump.Id), Natural (T.Packets.Get_Memory_Region_Packet_Id));
      Byte_Array_Assert.Eq (To_Byte_Array (Dump.Memory_Pointer), [0 => 0, 1 => 1, 2 => 2, 3 => 3, 4 => 4]);

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region_Bytes ((Address => 13, Length => 8)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Bytes_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 3);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (3), (Address => 13, Length => 8));

      -- Make sure region is dumped:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 3);
      Dump := T.Memory_Dump_Recv_Sync_History.Get (3);

      -- Check dump id and data:
      Natural_Assert.Eq (Natural (Dump.Id), Natural (T.Packets.Get_Memory_Region_Packet_Id));
      Byte_Array_Assert.Eq (To_Byte_Array (Dump.Memory_Pointer), [0 => 13, 1 => 14, 2 => 15, 3 .. 7 => 33]);

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region_Bytes ((Address => 98, Length => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Bytes_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 4);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (4), (Address => 98, Length => 2));

      -- Make sure region is dumped:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 4);
      Dump := T.Memory_Dump_Recv_Sync_History.Get (4);

      -- Check dump id and data:
      Natural_Assert.Eq (Natural (Dump.Id), Natural (T.Packets.Get_Memory_Region_Packet_Id));
      Byte_Array_Assert.Eq (To_Byte_Array (Dump.Memory_Pointer), [0 => 55, 1 => 55]);

      -- No data products except the ones at initialization.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
   end Test_Nominal_Memory_Dump;

   overriding procedure Test_Nominal_Memory_Crc (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Crc : Crc_16.Crc_16_Type;
   begin
      -- No data products except the ones at initialization.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      -- Set bytes to pattern to make testing easier:
      Memory := [0 => 0, 1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 6, 7 => 7, 8 => 8, 9 => 9, 10 => 10, 11 => 11, 12 => 12, 13 => 13, 14 => 14, 15 => 15, 16 .. 50 => 33, 51 .. 99 => 55];

      -- Send command to dump region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 0, Length => Memory'Length)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 1);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (1), (Address => 0, Length => Memory'Length));
      Crc := Crc_16.Compute_Crc_16 (Memory);
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 1);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (1), (Region => (Address => 0, Length => Memory'Length), Crc => Crc));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 2);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (2), (Region => (Address => 0, Length => Memory'Length), Crc => Crc));

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 0, Length => 5)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 2);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (2), (Address => 0, Length => 5));
      Crc := Crc_16.Compute_Crc_16 (Memory (0 .. 4));
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 2);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (2), (Region => (Address => 0, Length => 5), Crc => Crc));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 3);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (3), (Region => (Address => 0, Length => 5), Crc => Crc));

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 13, Length => 8)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 3);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (3), (Address => 13, Length => 8));
      Crc := Crc_16.Compute_Crc_16 (Memory (13 .. 20));
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 3);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (3), (Region => (Address => 13, Length => 8), Crc => Crc));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 4);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (4), (Region => (Address => 13, Length => 8), Crc => Crc));

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 98, Length => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 4);
      Virtual_Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (4), (Address => 98, Length => 2));
      Crc := Crc_16.Compute_Crc_16 (Memory (98 .. 99));
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 4);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (4), (Region => (Address => 98, Length => 2), Crc => Crc));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 5);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (5), (Region => (Address => 98, Length => 2), Crc => Crc));
   end Test_Nominal_Memory_Crc;

   overriding procedure Test_Nominal_Memory_Write (Self : in out Instance) is
      use Serializer_Types;
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Send command to write start of region:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 0, Length => 5, Data => [others => 15]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 1);
      Virtual_Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (1), (Address => 0, Length => 5));
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 1);
      Virtual_Memory_Region_Assert.Eq (T.Memory_Written_History.Get (1), (Address => 0, Length => 5));

      -- Check the memory region to make sure it was written to:
      Byte_Array_Assert.Eq (Memory (0 .. 4), [0 .. 4 => 15]);
      Byte_Array_Assert.Eq (Memory (5 .. Memory'Last), [5 .. Memory'Last => 0]);

      -- Send command to write middle of region:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 40, Length => 11, Data => [others => 12]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 2);
      Virtual_Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (2), (Address => 40, Length => 11));
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 2);
      Virtual_Memory_Region_Assert.Eq (T.Memory_Written_History.Get (2), (Address => 40, Length => 11));

      -- Check the memory region to make sure it was written to:
      Byte_Array_Assert.Eq (Memory (0 .. 4), [0 .. 4 => 15]);
      Byte_Array_Assert.Eq (Memory (40 .. 50), [40 .. 50 => 12]);
      Byte_Array_Assert.Eq (Memory (51 .. Memory'Last), [51 .. Memory'Last => 0]);

      -- Send command to write end of region:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 98, Length => 2, Data => [others => 44]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 3);
      Virtual_Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (3), (Address => 98, Length => 2));
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 3);
      Virtual_Memory_Region_Assert.Eq (T.Memory_Written_History.Get (3), (Address => 98, Length => 2));

      -- Check the memory region to make sure it was written to:
      Byte_Array_Assert.Eq (Memory (0 .. 4), [0 .. 4 => 15]);
      Byte_Array_Assert.Eq (Memory (40 .. 50), [40 .. 50 => 12]);
      Byte_Array_Assert.Eq (Memory (51 .. Memory'Last - 2), [51 .. Memory'Last - 2 => 0]);
      Byte_Array_Assert.Eq (Memory (98 .. Memory'Last), [98 .. Memory'Last => 44]);

      -- Send command to write end of region:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 98, Length => 1, Data => [others => 18]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 4);
      Virtual_Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (4), (Address => 98, Length => 1));
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 4);
      Virtual_Memory_Region_Assert.Eq (T.Memory_Written_History.Get (4), (Address => 98, Length => 1));

      -- Check the memory region to make sure it was written to:
      Byte_Array_Assert.Eq (Memory (0 .. 4), [0 .. 4 => 15]);
      Byte_Array_Assert.Eq (Memory (40 .. 50), [40 .. 50 => 12]);
      Byte_Array_Assert.Eq (Memory (51 .. Memory'Last - 2), [51 .. Memory'Last - 2 => 0]);
      Byte_Array_Assert.Eq (Memory (98 .. 98), [98 .. 98 => 18]);
      Byte_Array_Assert.Eq (Memory (99 .. Memory'Last), [99 .. Memory'Last => 44]);
   end Test_Nominal_Memory_Write;

   overriding procedure Test_Write_Unreleased_Region (Self : in out Instance) is
      use Serializer_Types;
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Request : Memory_Region_Request.T;
   begin
      -- First request the region so it is not available.
      Request := T.Memory_Region_Request_T_Get;
      Memory_Region_Request_Assert.Eq (Request, ((Id => 0, Region => (Memory'Address, Memory'Length)), Status => Success));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 2);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (2), (State => In_Use));

      -- Send command to write start of region, expect failure:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 0, Length => 5, Data => [others => 15]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 1);
      Virtual_Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (1), (Address => 0, Length => 5));
      Natural_Assert.Eq (T.Memory_Unavailable_History.Get_Count, 1);

      -- Check the memory region to make sure it was written to:
      Byte_Array_Assert.Eq (Memory (0 .. Memory'Last), [0 .. Memory'Last => 0]);

      -- Now release the region so it becomes available.
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Check the data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 3);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (3), (State => Available));

      -- Send command to write start of region, expect success:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 0, Length => 5, Data => [others => 15]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 2);
      Virtual_Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (2), (Address => 0, Length => 5));
      Natural_Assert.Eq (T.Memory_Unavailable_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 1);
      Virtual_Memory_Region_Assert.Eq (T.Memory_Written_History.Get (1), (Address => 0, Length => 5));

      -- Check the memory region to make sure it was written to:
      Byte_Array_Assert.Eq (Memory (0 .. 4), [0 .. 4 => 15]);
      Byte_Array_Assert.Eq (Memory (5 .. Memory'Last), [5 .. Memory'Last => 0]);
   end Test_Write_Unreleased_Region;

   overriding procedure Test_Dump_Invalid_Region (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region_Bytes ((Address => 0, Length => 101)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 1);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (1), (Invalid_Region => (Address => 0, Length => 101), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region_Bytes ((Address => 100, Length => 1000)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 2);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (2), (Invalid_Region => (Address => 100, Length => 1000), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region_Bytes ((Address => 50, Length => 51)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 3);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (3), (Invalid_Region => (Address => 50, Length => 51), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to dump part of region:
      T.Command_T_Send (T.Commands.Dump_Memory_Region_Bytes ((Address => 0, Length => 500)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 4);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (4), (Invalid_Region => (Address => 0, Length => 500), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Make sure region is never dumped:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
   end Test_Dump_Invalid_Region;

   overriding procedure Test_Crc_Invalid_Region (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command to crc part of region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 0, Length => 101)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 1);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (1), (Invalid_Region => (Address => 0, Length => 101), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to crc part of region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 100, Length => 1000)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 2);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (2), (Invalid_Region => (Address => 100, Length => 1000), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to crc part of region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 50, Length => 51)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 3);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (3), (Invalid_Region => (Address => 50, Length => 51), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to crc part of region:
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => 0, Length => 500)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 4);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (4), (Invalid_Region => (Address => 0, Length => 500), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Make sure no data products sent out besides initial ones:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      -- Send command to dump region with everything maxed to try to induce overflow
      T.Command_T_Send (T.Commands.Crc_Memory_Region_Bytes ((Address => Natural'Last, Length => Natural'Last)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Region_Bytes_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 5);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (5), (Invalid_Region => (Address => Natural'Last, Length => Natural'Last), Managed_Region => (Address => 0, Length => Memory'Length)));
   end Test_Crc_Invalid_Region;

   overriding procedure Test_Write_Invalid_Region (Self : in out Instance) is
      use Serializer_Types;
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Send command to write part too big of region:
      declare
         Bytes : constant Virtual_Memory_Region_Write.Serialization.Byte_Array := [0, 0, 0, 0, 100, 100, others => 255];
         Temp : constant Virtual_Memory_Region_Write.T := (0, 0, [others => 0]);
      begin
         pragma Assert (T.Commands.Write_Memory_Region (Temp, Cmd) = Success);
         -- Overwrite with invalid bytes.
         Cmd.Arg_Buffer := Bytes;
      end;
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Write_Memory_Region_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 6]));

      -- Send command to write part of region:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 100, Length => 0, Data => [others => 255]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 1);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (1), (Invalid_Region => (Address => 100, Length => 0), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to write part of region:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 50, Length => 51, Data => [others => 255]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 2);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (2), (Invalid_Region => (Address => 50, Length => 51), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Send command to write part of region:
      pragma Assert (T.Commands.Write_Memory_Region ((Address => 0, Length => 0, Data => [others => 255]), Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Region_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 3);
      Invalid_Virtual_Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (3), (Invalid_Region => (Address => 0, Length => 0), Managed_Region => (Address => 0, Length => Memory'Length)));

      -- Make sure no data products sent out besides initial ones:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      -- Check the memory region to make sure it was never written to:
      Byte_Array_Assert.Eq (Memory, [0 .. Memory'Length - 1 => 0]);
   end Test_Write_Invalid_Region;

   overriding procedure Test_Force_Release_Command (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Request : Memory_Region_Request.T;
   begin
      -- Check initial data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 1);
      Virtual_Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (1), (Region => (Address => 0, Length => 0), Crc => [0, 0]));
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (1), (State => Available));

      -- Send command to force release region:
      T.Command_T_Send (T.Commands.Force_Release);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Force_Release_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Force_Released_History.Get_Count, 1);

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 2);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (2), (State => Available));

      -- Request the memory region:
      Request := T.Memory_Region_Request_T_Get;
      Memory_Region_Request_Assert.Eq (Request, ((Id => 0, Region => (Memory'Address, Memory'Length)), Status => Success));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 3);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (3), (State => In_Use));

      -- Send command to force release region:
      T.Command_T_Send (T.Commands.Force_Release);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Force_Release_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Force_Released_History.Get_Count, 2);

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 4);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (4), (State => Available));

      -- Release the memory region:
      T.Ided_Memory_Region_T_Release_Reciprocal (Request.Ided_Region);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Memory_Already_Released_History.Get_Count, 1);
      Ided_Memory_Region_Assert.Eq (T.Memory_Already_Released_History.Get (1), (Id => 0, Region => (Memory'Address, Memory'Length)));

      -- Send command to force release region:
      T.Command_T_Send (T.Commands.Force_Release);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Force_Release_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Force_Released_History.Get_Count, 3);

      -- Check the data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Memory_Region_Status_History.Get_Count, 5);
      Memory_Manager_State_Assert.Eq (T.Memory_Region_Status_History.Get (5), (State => Available));
   end Test_Force_Release_Command;

   overriding procedure Test_Command_Dropped (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Force_Release;
   begin
      -- Make the command maximum size:
      Cmd.Header.Arg_Buffer_Length := Cmd.Arg_Buffer'Length;

      -- Four ticks should fill the queue.
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);

      -- OK the next command should overflow the queue.
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (Cmd);

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Dropped_Command_History.Get (1), Cmd.Header);
   end Test_Command_Dropped;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Memory_Manager.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Force_Release;
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 13;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Force_Release_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Force_Release_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 13]));
   end Test_Invalid_Command;

end Memory_Manager_Tests.Implementation;
