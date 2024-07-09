--------------------------------------------------------------------------------
-- Memory_Dumper Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Types; use Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Memory_Manager_Types;
with System.Storage_Elements;
with Memory_Region_Positive.Assertion; use Memory_Region_Positive.Assertion;
with Memory_Region_Positive.Representation;
with Memory_Region_Crc.Assertion; use Memory_Region_Crc.Assertion;
with Memory_Region_Crc.Representation;
with Memory_Dumper_Packets;
with Byte_Array_Pointer.Assertion; use Byte_Array_Pointer.Assertion;
with Packet_Types;
with Smart_Assert;
with Crc_16;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Interfaces;

package body Memory_Dumper_Tests.Implementation is

   Region_1 : aliased Basic_Types.Byte_Array := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
   Region_1_Address : constant System.Address := Region_1'Address;
   Region_2 : aliased Basic_Types.Byte_Array := [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79];
   Region_2_Address : constant System.Address := Region_2'Address;
   Regions : aliased Memory_Manager_Types.Memory_Region_Array := [(Region_1_Address, Region_1'Length), (Region_2_Address, Region_2'Length)];

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Memory_Regions => Regions'Access);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   package Packet_Id_Assert is new Smart_Assert.Discrete (Packet_Types.Packet_Id, Packet_Types.Packet_Id'Image);

   overriding procedure Test_Nominal_Dumping (Self : in out Instance) is
      use System.Storage_Elements;
      use System;
      T : Component.Memory_Dumper.Implementation.Tester.Instance_Access renames Self.Tester;
      Region : Memory_Region_Positive.T;
      Packets : Memory_Dumper_Packets.Instance;
   begin
      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send command to dump entire first region:
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Put_Line ("Dumping region 1:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Make sure no events were sent or pointers sent yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check event:
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 1);
      Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (1), Region);

      -- Check memory dump:
      Packet_Id_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (1).Id, Packets.Get_Memory_Dump_Packet_Id);
      Byte_Array_Pointer_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (1).Memory_Pointer, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

      -- Send command to dump entire second region:
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Put_Line ("Dumping region 2:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Make sure no events were sent or pointers sent yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Check event:
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 2);
      Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (2), Region);

      -- Check memory dump:
      Packet_Id_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (2).Id, Packets.Get_Memory_Dump_Packet_Id);
      Byte_Array_Pointer_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (2).Memory_Pointer, [98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79]);

      -- Send command to dump partial first region:
      Region := (Address => Region_1_Address, Length => 5);
      Put_Line ("Dumping part of region 1:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 3);

      -- Check event:
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 3);
      Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (3), Region);

      -- Check memory dump:
      Packet_Id_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (3).Id, Packets.Get_Memory_Dump_Packet_Id);
      Byte_Array_Pointer_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (3).Memory_Pointer, [1, 2, 3, 4, 5]);

      -- Send command to dump partial second region:
      Region := (Address => Region_2_Address + Storage_Offset (12), Length => 6);
      Put_Line ("Dumping part of region 2:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 4);

      -- Check event:
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 4);
      Memory_Region_Positive_Assert.Eq (T.Dumping_Memory_History.Get (4), Region);

      -- Check memory dump:
      Packet_Id_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (4).Id, Packets.Get_Memory_Dump_Packet_Id);
      Byte_Array_Pointer_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get (4).Memory_Pointer, [86, 85, 84, 83, 82, 81]);
   end Test_Nominal_Dumping;

   overriding procedure Test_Memory_Crc (Self : in out Instance) is
      use System.Storage_Elements;
      use System;
      T : Component.Memory_Dumper.Implementation.Tester.Instance_Access renames Self.Tester;
      Region : Memory_Region_Positive.T;
   begin
      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send command to crc entire first region:
      Region := (Address => Region_1_Address, Length => Region_1'Length);
      Put_Line ("CRCing region 1:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));

      -- Make sure no events were sent or pointers sent yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      -- Check events:
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 1);
      Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (1), Region);
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 1);
      Put_Line ("CRC:");
      Put_Line (Memory_Region_Crc.Representation.Image (T.Memory_Crc_History.Get (1)));
      Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (1), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 (Region_1)));

      -- Check data product:
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 1);
      Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (1), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 (Region_1)));

      -- Send command to crc entire second region:
      Region := (Address => Region_2_Address, Length => Region_2'Length);
      Put_Line ("CRCing region 2:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));

      -- Make sure no events were sent or pointers sent yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);

      -- Check events:
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 2);
      Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (2), Region);
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 2);
      Put_Line ("CRC:");
      Put_Line (Memory_Region_Crc.Representation.Image (T.Memory_Crc_History.Get (2)));
      Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (2), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 (Region_2)));

      -- Check data product:
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 2);
      Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (2), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 (Region_2)));

      -- Send command to crc partial first region:
      Region := (Address => Region_1_Address + Storage_Offset (2), Length => 4);
      Put_Line ("CRCing partial region 1:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      -- Check events:
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 3);
      Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (3), Region);
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 3);
      Put_Line ("CRC:");
      Put_Line (Memory_Region_Crc.Representation.Image (T.Memory_Crc_History.Get (3)));
      Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (3), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 ([3, 4, 5, 6])));

      -- Check data product:
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 3);
      Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (3), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 ([3, 4, 5, 6])));

      -- Send command to crc partial second region:
      Region := (Address => Region_2_Address + Storage_Offset (12), Length => 8);
      Put_Line ("CRCing partial region 2:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Success));

      -- One event and one memory dump should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);

      -- Check events:
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 4);
      Memory_Region_Positive_Assert.Eq (T.Crcing_Memory_History.Get (4), Region);
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 4);
      Put_Line ("CRC:");
      Put_Line (Memory_Region_Crc.Representation.Image (T.Memory_Crc_History.Get (4)));
      Memory_Region_Crc_Assert.Eq (T.Memory_Crc_History.Get (4), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 ([86, 85, 84, 83, 82, 81, 80, 79])));

      -- Check data product:
      Natural_Assert.Eq (T.Crc_Report_History.Get_Count, 4);
      Memory_Region_Crc_Assert.Eq (T.Crc_Report_History.Get (4), (Region => (Region.Address, Region.Length), Crc => Crc_16.Compute_Crc_16 ([86, 85, 84, 83, 82, 81, 80, 79])));
   end Test_Memory_Crc;

   overriding procedure Test_Invalid_Address (Self : in out Instance) is
      use System.Storage_Elements;
      use System;
      T : Component.Memory_Dumper.Implementation.Tester.Instance_Access renames Self.Tester;
      Region : Memory_Region_Positive.T;
   begin
      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send command to crc entire first region:
      Region := (Address => Region_1_Address, Length => Region_1'Length + 1);
      Put_Line ("Sending region with length too long:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));

      -- Make sure no events were sent or pointers sent yet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Failure));

      -- One error event should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 1);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (1), Region);

      -- Make sure the same thing happens with a dump command:
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Failure));

      -- One error event should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 2);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (2), Region);

      -- Send command to crc entire first region:
      Region := (Address => Region_1_Address - Storage_Offset (1), Length => Region_1'Length);
      Put_Line ("Sending region with address out of range:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Failure));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Failure));

      -- One error event should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 4);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (3), Region);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (4), Region);

      -- Send command to crc entire first region:
      Region := (Address => Region_2_Address - Storage_Offset (1), Length => Region_1'Length + 2);
      Put_Line ("Sending region with everything out of range:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Failure));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Failure));

      -- One error event should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 6);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (5), Region);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (6), Region);

      -- Send command to crc entire first region:
      Region := (Address => Region_2_Address + Storage_Offset (5), Length => 16);
      Put_Line ("Sending region with everything out of range:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 8);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Failure));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (8), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Failure));

      -- One error event should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 8);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (7), Region);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (8), Region);

      -- Send command to dump 1 length second region:
      Region := (Address => Region_2_Address + Storage_Offset (20), Length => 1);
      Put_Line ("Dumping invalid 1 length part of region 2:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 10);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (9), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Failure));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (10), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Failure));

      -- One error event should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 10);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (9), Region);
      Memory_Region_Positive_Assert.Eq (T.Invalid_Memory_Region_History.Get (10), Region);

      -- Send command to crc entire first region:
      Region := (Address => Region_2_Address + Storage_Offset (5), Length => 15);
      Put_Line ("Sending valid region:");
      Put_Line (Memory_Region_Positive.Representation.Image (Region));
      T.Command_T_Send (T.Commands.Crc_Memory (Region));
      T.Command_T_Send (T.Commands.Dump_Memory (Region));

      -- Drain the queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 12);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (11), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (12), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Memory_Id, Status => Success));

      -- One error event should have been returned.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Memory_Region_History.Get_Count, 10);
      Natural_Assert.Eq (T.Dumping_Memory_History.Get_Count, 1);
      Natural_Assert.Eq (T.Crcing_Memory_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Crc_History.Get_Count, 1);
   end Test_Invalid_Address;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Memory_Dumper.Implementation.Tester.Instance_Access renames Self.Tester;
      Region : constant Memory_Region_Positive.T := (Address => Region_1_Address, Length => Region_1'Length);
      Cmd : Command.T := T.Commands.Crc_Memory (Region);
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Crc_Memory_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Crc_Memory_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Memory_Dumper_Tests.Implementation;
