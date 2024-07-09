--------------------------------------------------------------------------------
-- Memory_Stuffer Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Basic_Types.Representation;
with Basic_Assertions; use Basic_Assertions;
with System;
with Memory_Manager_Types;
with Memory_Region;
with Serializer_Types;
with Command;
with Smart_Assert;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Memory_Region_Release.Assertion; use Memory_Region_Release.Assertion;
with System.Storage_Elements;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Interfaces; use Interfaces;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Memory_Region_Copy.Assertion; use Memory_Region_Copy.Assertion;
with Memory_Enums;
with Packed_Arm_State.Assertion; use Packed_Arm_State.Assertion;
with Packed_Arm_Timeout.Assertion; use Packed_Arm_Timeout.Assertion;
with Command_Protector_Enums;

package body Memory_Stuffer_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   Region_1 : aliased Basic_Types.Byte_Array := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
   Region_1_Address : constant System.Address := Region_1'Address;
   Region_2 : aliased Basic_Types.Byte_Array := [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79];
   Region_2_Address : constant System.Address := Region_2'Address;
   Regions : aliased Memory_Manager_Types.Memory_Region_Array := [(Region_1_Address, Region_1'Length), (Region_2_Address, Region_2'Length)];
   Protection_List : aliased Memory_Manager_Types.Memory_Protection_Array := [Memory_Manager_Types.Unprotected_Region, Memory_Manager_Types.Protected_Region];
   -- Another region for copying from.
   Region_3 : aliased Basic_Types.Byte_Array := [66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77];
   Region_3_Address : constant System.Address := Region_3'Address;

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 5);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   package Ser_Status_Assert is new Smart_Assert.Discrete (Serializer_Types.Serialization_Status, Serializer_Types.Serialization_Status'Image);

   overriding procedure Test_Invalid_Initialization (Self : in out Instance) is
      use Command_Protector_Enums.Armed_State;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal is
      begin
         T.Component_Instance.Init (Regions'Access, Protection_List'Access);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init failed!");
      end Init_Nominal;

      procedure Init_Nominal_No_Protection is
      begin
         T.Component_Instance.Init (Regions'Access, null);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init no protection failed!");
      end Init_Nominal_No_Protection;

      procedure Init_Size_Mismatch is
         Protection_List_Bad : aliased Memory_Manager_Types.Memory_Protection_Array := [0 => Memory_Manager_Types.Unprotected_Region];
      begin
         T.Component_Instance.Init (Regions'Access, Protection_List_Bad'Unchecked_Access);
         -- Should never get here:
         Assert (False, "Init size mismatch did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Size_Mismatch;

      procedure Init_Size_Mismatch_2 is
         Protection_List_Bad : aliased Memory_Manager_Types.Memory_Protection_Array := [Memory_Manager_Types.Unprotected_Region, Memory_Manager_Types.Protected_Region, Memory_Manager_Types.Protected_Region];
      begin
         T.Component_Instance.Init (Regions'Access, Protection_List_Bad'Unchecked_Access);
         -- Should never get here:
         Assert (False, "Init size mismatch 2 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Size_Mismatch_2;
   begin
      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure that memory is not written by default on valid startup:
      Byte_Array_Assert.Eq (Region_1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Byte_Array_Assert.Eq (Region_2, [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79]);

      -- Test different start-up scenarios:
      Init_Nominal;
      Init_Nominal_No_Protection;
      Init_Size_Mismatch;
      Init_Size_Mismatch_2;

      -- Make sure no memory releases were sent.
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);

      -- Call setup:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (1), (Timeout => 0));
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 1);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (1), (State => Unarmed));
   end Test_Invalid_Initialization;

   overriding procedure Test_Unprotected_Stuffing (Self : in out Instance) is
      use Serializer_Types;
      use System.Storage_Elements;
      use System;
      Region : Memory_Region.T;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Init both regions with no protection:
      T.Component_Instance.Init (Regions'Access, null);

      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure that memory is not written by default on valid startup:
      Byte_Array_Assert.Eq (Region_1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Byte_Array_Assert.Eq (Region_2, [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79]);

      -- Send a command to stuff a memory region:
      Put_Line ("Writing region 1:");
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 9]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Make sure no events are thrown yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (1), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (1), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Byte_Array_Assert.Eq (Region_1, [9, 9, 9, 9, 9, 9, 9, 9, 9, 9]);

      -- Send a command to stuff a memory region:
      Region := (Address => Region_1_Address, Length => Region_1'Length - 1);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 8]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (2), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (2), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Byte_Array_Assert.Eq (Region_1, [8, 8, 8, 8, 8, 8, 8, 8, 8, 9]);

      -- Send a command to stuff a memory region:
      Region := (Address => Region_1_Address + Storage_Offset (2), Length => 3);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 7]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 3);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (3), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 3);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (3), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Byte_Array_Assert.Eq (Region_1, [8, 8, 7, 7, 7, 8, 8, 8, 8, 9]);

      -- Send a command to stuff a memory region:
      Put_Line ("Writing region 2:");
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 255]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 4);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (4), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 4);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (4), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_1, [8, 8, 7, 7, 7, 8, 8, 8, 8, 9]); -- Make sure region 1 is unchanged.
      Byte_Array_Assert.Eq (Region_2, [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255]);

      -- Send a command to stuff a memory region:
      Region := (Address => Region_2_Address, Length => 5);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 254]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 5);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (5), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 5);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (5), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_2, [254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255]);

      -- Send a command to stuff a memory region:
      Region := (Address => Region_2_Address + Storage_Offset (15), Length => 3);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 253]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 6);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (6), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 6);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (6), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_2, [254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 253, 253, 253, 255, 255]);

      -- Make sure no memory releases were sent.
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);
   end Test_Unprotected_Stuffing;

   overriding procedure Test_Protected_Stuffing (Self : in out Instance) is
      use Serializer_Types;
      use System.Storage_Elements;
      use System;
      Region : Memory_Region.T;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Init both regions with no protection:
      T.Component_Instance.Init (Regions'Access, Protection_List'Access);

      -- Send a command to stuff a memory region:
      Put_Line ("Writing unprotected region 1:");
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 9]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Make sure no events are thrown yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (1), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (1), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Byte_Array_Assert.Eq (Region_1, [9, 9, 9, 9, 9, 9, 9, 9, 9, 9]);

      -- Send a command to stuff a memory region:
      Region := (Address => Region_1_Address, Length => Region_1'Length - 1);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 8]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (2), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (2), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Byte_Array_Assert.Eq (Region_1, [8, 8, 8, 8, 8, 8, 8, 8, 8, 9]);

      -- Send a command to stuff a memory region:
      Region := (Address => Region_1_Address + Storage_Offset (2), Length => 3);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 7]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 3);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (3), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 3);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (3), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Byte_Array_Assert.Eq (Region_1, [8, 8, 7, 7, 7, 8, 8, 8, 8, 9]);

      -- Send a command to stuff a memory region:
      Put_Line ("Writing protected region 2:");
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 255]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));

      -- Expect one error event to be thrown because we are not armed.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Protected_Write_Denied_History.Get (1), Region);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      -- Make sure region 1 is unchanged.
      Byte_Array_Assert.Eq (Region_1, [8, 8, 7, 7, 7, 8, 8, 8, 8, 9]);
      -- Make sure region 2 is unchanged.
      Byte_Array_Assert.Eq (Region_2, [254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 253, 253, 253, 255, 255]);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));

      -- Make sure no additional event thrown yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Verify we are armed:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 1);

      -- Send a command to stuff a memory region:
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 255]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_1, [8, 8, 7, 7, 7, 8, 8, 8, 8, 9]); -- Make sure region 1 is unchanged.
      Byte_Array_Assert.Eq (Region_2, [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255]);

      -- See if event thrown to disarm:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 4);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (4), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 4);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (4), Region);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 1);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));

      -- Send a command to stuff a memory region:
      Region := (Address => Region_2_Address, Length => 5);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 254]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 8);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (8), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 15);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 5);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (5), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 5);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (5), Region);
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 2);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 2);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_2, [254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255]);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));

      -- Send a command to stuff a memory region:
      Region := (Address => Region_2_Address + Storage_Offset (15), Length => 3);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 253]), Cmd), Success);
      T.Command_T_Send (Cmd);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 10);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (9), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (10), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 19);
      Natural_Assert.Eq (T.Writing_Memory_History.Get_Count, 6);
      Memory_Region_Assert.Eq (T.Writing_Memory_History.Get (6), Region);
      Natural_Assert.Eq (T.Memory_Written_History.Get_Count, 6);
      Memory_Region_Assert.Eq (T.Memory_Written_History.Get (6), Region);
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 3);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 3);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_2, [254, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 253, 253, 253, 255, 255]);

      -- Make sure no memory releases were sent.
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);
   end Test_Protected_Stuffing;

   overriding procedure Test_Arm_Unarm (Self : in out Instance) is
      use Serializer_Types;
      use System.Storage_Elements;
      use System;
      Region : Memory_Region.T;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Init both regions with no protection:
      T.Component_Instance.Init (Regions'Access, Protection_List'Access);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 0);

      -- Write region 2 and expect disarmed:
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect system to be disarmed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 1);
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Protected_Write_Denied_History.Get (1), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 2);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 1);

      -- Write region 1 and expect disarmed:
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect system to be disarmed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 2);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 2);
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Protected_Write_Denied_History.Get (2), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 7);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 3);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 2);

      -- Write invalid command expect disarmed:
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 2]), Cmd), Success);
      Cmd.Header.Arg_Buffer_Length := 0; -- Cause deserialization error
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 8);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (8), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Length_Error));
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Write_Memory_Id, Errant_Field_Number => Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));

      -- Expect system to be disarmed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 3);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 3);
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 9);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (9), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 3);
      Memory_Region_Assert.Eq (T.Protected_Write_Denied_History.Get (3), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 10);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (10), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 4);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 3);

      -- Write invalid address expect disarmed:
      Region := (Address => Region_1_Address - Storage_Offset (1), Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 2]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 11);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (11), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (1), Region);

      -- Expect system to be disarmed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 4);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 4);
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 12);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (12), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 4);
      Memory_Region_Assert.Eq (T.Protected_Write_Denied_History.Get (4), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 13);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (13), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 5);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 4);

      -- Send another arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 14);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (14), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 6);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 4);

      -- Write region 2 expect disarmed:
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 2]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 15);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (15), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));

      -- Expect system to be disarmed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 6);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 5);
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 16);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (16), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 5);
      Memory_Region_Assert.Eq (T.Protected_Write_Denied_History.Get (5), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Make sure no memory releases were sent.
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);
   end Test_Arm_Unarm;

   overriding procedure Test_Arm_Timeout (Self : in out Instance) is
      use Command_Protector_Enums.Armed_State;
      use Serializer_Types;
      Region : Memory_Region.T;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Init both regions with no protection:
      T.Component_Instance.Init (Regions'Access, Protection_List'Access);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 0);

      -- Delay 2 seconds, by sending ticks
      T.Tick_T_Send (((90, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (2), (Timeout => 1));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (((90, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 3);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (3), (Timeout => 0));
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 2);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (2), (State => Unarmed));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Protected_Write_Disabled_Timeout_History.Get_Count, 1);

      -- Expect system to be disarmed when trying to write:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 0);
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 1]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Protected_Write_Denied_History.Get (1), Region);
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 1);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Send arm command:
      T.Command_T_Send (T.Commands.Arm_Protected_Write ((Timeout => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Protected_Write_Id, Status => Success));

      -- Expect system to be armed:
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 2);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 0);

      -- Delay 0 seconds:

      -- Expect system to be disarmed when trying to write:
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 3]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));
      Natural_Assert.Eq (T.Protected_Write_Denied_History.Get_Count, 1);
      Natural_Assert.Eq (T.Protected_Write_Enabled_History.Get_Count, 2);
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 1);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Make sure no memory releases were sent.
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);
   end Test_Arm_Timeout;

   overriding procedure Test_Invalid_Address (Self : in out Instance) is
      use Serializer_Types;
      use System.Storage_Elements;
      use System;
      Region : Memory_Region.T;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Init both regions with no protection:
      T.Component_Instance.Init (Regions'Access, null);

      -- Write invalid address:
      Region := (Address => Region_1_Address - Storage_Offset (1), Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 2]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (1), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Write invalid address:
      Region := (Address => Region_1_Address + Storage_Offset (1), Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 2]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (2), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Write invalid address:
      Region := (Address => Region_2_Address - Storage_Offset (1), Length => 0);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 2]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 3);
      Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (3), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Write valid address, zero length:
      Region := (Address => Region_1_Address, Length => 0);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 2]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Failure));
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 4);
      Memory_Region_Assert.Eq (T.Invalid_Memory_Region_History.Get (4), Region);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]);
      Byte_Array_Assert.Eq (Region_1, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]);

      -- Write valid address, zero length:
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 4]), Cmd), Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Success));
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 4);

      -- Check memory:
      Byte_Array_Assert.Eq (Region_2, [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]);
      Byte_Array_Assert.Eq (Region_1, [4, 4, 4, 4, 4, 4, 4, 4, 4, 4]);

      -- Make sure no memory releases were sent.
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);
   end Test_Invalid_Address;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      use Serializer_Types;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
      Region : constant Memory_Region.T := (Address => Region_1_Address, Length => Region_1'Length);
      Cmd : Command.T;
   begin
      -- Make the command invalid by modifying its length.
      Ser_Status_Assert.Eq (T.Commands.Write_Memory ((Region.Address, Region.Length, [others => 4]), Cmd), Success);
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Write_Memory_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Write_Memory_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
      Natural_Assert.Eq (T.Protected_Write_Disabled_History.Get_Count, 1);

      -- Make sure no memory releases were sent.
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);
   end Test_Invalid_Command;

   overriding procedure Test_Memory_Region_Copy (Self : in out Instance) is
      use Memory_Enums.Memory_Copy_Status;
      Region : Memory_Region.T;
      Region_Copy : Memory_Region_Copy.T;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Reset memory regions:
      Region_1 := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      Region_2 := [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79];

      -- Init both regions with no protection:
      T.Component_Instance.Init (Regions'Access, null);

      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);

      -- Send a command to stuff a memory region:
      Put_Line ("Copying to region 1 from region 3:");
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Region := (Address => Region_3_Address, Length => Region_1'Length);
      Region_Copy := (Source_Region => Region, Destination_Address => Region_1_Address);
      T.Memory_Region_Copy_T_Send (Region_Copy);

      -- Make sure no events are thrown yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Memory_Region_Release_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region => Region, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Copying_Memory_History.Get_Count, 1);
      Memory_Region_Copy_Assert.Eq (T.Copying_Memory_History.Get (1), Region_Copy);
      Natural_Assert.Eq (T.Memory_Copied_History.Get_Count, 1);
      Memory_Region_Copy_Assert.Eq (T.Memory_Copied_History.Get (1), Region_Copy);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_1));
      Byte_Array_Assert.Eq (Region_1, [66, 67, 68, 69, 70, 71, 72, 73, 74, 75]);

      -- Send a command to stuff a memory region:
      Put_Line ("Copying to region 1 from region 2:");
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Region := (Address => Region_3_Address, Length => Region_3'Length);
      Region_Copy := (Source_Region => Region, Destination_Address => Region_2_Address);
      T.Memory_Region_Copy_T_Send (Region_Copy);

      -- Make sure no events are thrown yet:
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Memory_Region_Release_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get (2), (Region => Region, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Copying_Memory_History.Get_Count, 2);
      Memory_Region_Copy_Assert.Eq (T.Copying_Memory_History.Get (2), Region_Copy);
      Natural_Assert.Eq (T.Memory_Copied_History.Get_Count, 2);
      Memory_Region_Copy_Assert.Eq (T.Memory_Copied_History.Get (2), Region_Copy);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_2, [66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 86, 85, 84, 83, 82, 81, 80, 79]);

      -- Send a command to stuff a memory region:
      Region_1 := [1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
      Put_Line ("Copying to region 2 from region 1:");
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Region_Copy := (Source_Region => Region, Destination_Address => Region_2_Address);
      T.Memory_Region_Copy_T_Send (Region_Copy);

      -- Make sure no events are thrown yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 3);
      Memory_Region_Release_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get (3), (Region => Region, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Copying_Memory_History.Get_Count, 3);
      Memory_Region_Copy_Assert.Eq (T.Copying_Memory_History.Get (3), Region_Copy);
      Natural_Assert.Eq (T.Memory_Copied_History.Get_Count, 3);
      Memory_Region_Copy_Assert.Eq (T.Memory_Copied_History.Get (3), Region_Copy);

      -- Check memory:
      Put_Line (Basic_Types.Representation.Image (Region_2));
      Byte_Array_Assert.Eq (Region_2, [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 76, 77, 86, 85, 84, 83, 82, 81, 80, 79]);
   end Test_Memory_Region_Copy;

   overriding procedure Test_Memory_Region_Copy_Invalid_Address (Self : in out Instance) is
      use Memory_Enums.Memory_Copy_Status;
      Region : Memory_Region.T;
      Region_Copy : Memory_Region_Copy.T;
      T : Component.Memory_Stuffer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Reset memory regions:
      Region_1 := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      Region_2 := [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79];

      -- Init both regions with no protection:
      T.Component_Instance.Init (Regions'Access, null);

      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);

      -- Send a command to stuff a memory region, invalid:
      Put_Line ("Copying to region 3 from region 3:");
      Region := (Address => Region_3_Address, Length => Region_1'Length);
      Region_Copy := (Source_Region => Region, Destination_Address => Region_3_Address);
      T.Memory_Region_Copy_T_Send (Region_Copy);

      -- Make sure no events are thrown yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Memory_Region_Release_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region => Region, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Copy_Destination_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Invalid_Copy_Destination_History.Get (1), (Region_3_Address, Region_1'Length));

      -- Check memory, make sure its the same:
      Byte_Array_Assert.Eq (Region_1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Byte_Array_Assert.Eq (Region_2, [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79]);

      -- Send a command to stuff a memory region, invalid:
      Put_Line ("Copying to region 1 from region 3:");
      Region := (Address => Region_3_Address, Length => Region_3'Length);
      Region_Copy := (Source_Region => Region, Destination_Address => Region_1_Address);
      T.Memory_Region_Copy_T_Send (Region_Copy);

      -- Make sure no events are thrown yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect two events to be thrown
      Natural_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Memory_Region_Release_Assert.Eq (T.Memory_Region_Release_T_Recv_Sync_History.Get (2), (Region => Region, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Copy_Destination_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Invalid_Copy_Destination_History.Get (2), (Region_1_Address, Region_3'Length));

      -- Check memory, make sure its the same:
      Byte_Array_Assert.Eq (Region_1, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Byte_Array_Assert.Eq (Region_2, [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79]);
   end Test_Memory_Region_Copy_Invalid_Address;

end Memory_Stuffer_Tests.Implementation;
