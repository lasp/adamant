--------------------------------------------------------------------------------
-- Stack_Monitor Tests Body
--------------------------------------------------------------------------------

with System;
with Task_Types;
with Basic_Types;
with Ada.Task_Identification;
with Basic_Assertions; use Basic_Assertions;
with Tick;
with Packet.Assertion; use Packet.Assertion;
with System.Storage_Elements; use System.Storage_Elements;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces;

package body Stack_Monitor_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   Task_1_Stack : aliased Basic_Types.Byte_Array (0 .. 999) := [others => 16#CC#];
   Task_1_Info : aliased Task_Types.Task_Info := (
      Number => 1,
      Id => Ada.Task_Identification.Null_Task_Id,
      Priority => 0,
      Stack_Address => Task_1_Stack (Task_1_Stack'Last)'Address + Storage_Offset (1),
      Stack_Size => Task_1_Stack'Length, Secondary_Stack_Address => System.Null_Address,
      Secondary_Stack_Size => 1_000,
      Secondary_Stack_Max_Usage => 0
   );

   Task_2_Stack : aliased Basic_Types.Byte_Array (0 .. 1_999) := [others => 16#CC#];
   Task_2_Info : aliased Task_Types.Task_Info := (
      Number => 2,
      Id => Ada.Task_Identification.Null_Task_Id,
      Priority => 0,
      Stack_Address => Task_2_Stack (Task_2_Stack'Last)'Address + Storage_Offset (1),
      Stack_Size => Task_2_Stack'Length,
      Secondary_Stack_Address => System.Null_Address,
      Secondary_Stack_Size => 2_000,
      Secondary_Stack_Max_Usage => 0
   );

   Task_List : aliased Task_Types.Task_Info_List := [0 => Task_1_Info'Access, 1 => Task_2_Info'Access];

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Make sure the stacks are pristine:
      Task_1_Stack := [others => 16#CC#];
      Task_2_Stack := [others => 16#CC#];
      Task_1_Info.Secondary_Stack_Max_Usage := 0;
      Task_2_Info.Secondary_Stack_Max_Usage := 0;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Task_List => Task_List'Access, Packet_Period => 1);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Insert custom cleanup code here.
      Self.Tester.Component_Instance.Final;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Stack_Monitoring (Self : in out Instance) is
      T : Component.Stack_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((0, 0), 0);
      Pkt : Packet.T := (Header => (Time => (0, 0), Id => T.Packets.Get_Stack_Usage_Packet_Id, Sequence_Count => 0, Buffer_Length => 4), Buffer => [others => 0]);
   begin
      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet, make sure all zeros:
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1), Pkt);

      -- Test the secondary stack:
      Task_1_Info.Secondary_Stack_Max_Usage := 639;
      Task_2_Info.Secondary_Stack_Max_Usage := 639;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 1;
      Pkt.Buffer (1) := 63;
      Pkt.Buffer (3) := 31;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (2), Pkt);

      -- See what happens if we set the secondary stack sizes to zero:
      Task_1_Info.Secondary_Stack_Size := 0;
      Task_2_Info.Secondary_Stack_Size := 0;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 2;
      Pkt.Buffer (1) := 0;
      Pkt.Buffer (3) := 0;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (3), Pkt);

      -- Reset secondary stack sizes:
      Task_1_Info.Secondary_Stack_Size := 1_000;
      Task_2_Info.Secondary_Stack_Size := 2_000;

      -- See what happens if the usage is larger than the stack size:
      Task_1_Info.Secondary_Stack_Max_Usage := Task_1_Info.Secondary_Stack_Size * 2;
      Task_2_Info.Secondary_Stack_Max_Usage := Task_2_Info.Secondary_Stack_Size + 2;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 3;
      Pkt.Buffer (1) := 100;
      Pkt.Buffer (3) := 100;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (4), Pkt);

      -- Test the secondary stack:
      Task_1_Info.Secondary_Stack_Max_Usage := 500;
      Task_2_Info.Secondary_Stack_Max_Usage := 1_000;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 4;
      Pkt.Buffer (1) := 50;
      Pkt.Buffer (3) := 50;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (5), Pkt);

      -- Test the primary stack. Let's fill up the stacks 639 bytes and see what happens.
      for Idx in Natural range 0 .. 638 loop
         Task_1_Stack (Task_1_Stack'Last - Idx) := 12; -- Something other than 0xCC
         Task_2_Stack (Task_2_Stack'Last - Idx) := 12; -- Something other than 0xCC
      end loop;

      -- A little white box testing. The internal index in the component for
      -- caching should be the last possible index.
      T.Check_Stack_Indexes ([Task_1_Stack'Last, Task_2_Stack'Last]);

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 5;
      Pkt.Buffer (0) := 63;
      Pkt.Buffer (2) := 31;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (6), Pkt);

      -- Test the primary stack. Let's fill up the first stack to 90 percent and
      -- the second stack to 50 percent.
      for Idx in Natural range 639 .. 902 loop
         Task_1_Stack (Task_1_Stack'Last - Idx) := 12; -- Something other than 0xCC
      end loop;
      for Idx in Natural range 639 .. 1_002 loop
         Task_2_Stack (Task_2_Stack'Last - Idx) := 12; -- Something other than 0xCC
      end loop;

      -- A little white box testing. The internal index in the component for
      -- caching should be at the last location where the stack ended.
      T.Check_Stack_Indexes ([Task_1_Stack'Last - 638, Task_2_Stack'Last - 638]);

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 7);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 6;
      Pkt.Buffer (0) := 90;
      Pkt.Buffer (2) := 50;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (7), Pkt);

      -- Test the primary stack. Let's fill up the stacks to 100 percent.
      for Idx in Natural range 902 .. 999 loop
         Task_1_Stack (Task_1_Stack'Last - Idx) := 12; -- Something other than 0xCC
      end loop;
      for Idx in Natural range 1_002 .. 1_999 loop
         Task_2_Stack (Task_2_Stack'Last - Idx) := 12; -- Something other than 0xCC
      end loop;

      -- A little white box testing. The internal index in the component for
      -- caching should be at the last location where the stack ended.
      T.Check_Stack_Indexes ([Task_1_Stack'Last - 902, Task_2_Stack'Last - 1_002]);

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 8);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 7;
      Pkt.Buffer (0) := 100;
      Pkt.Buffer (2) := 100;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (8), Pkt);

      -- A little white box testing. The internal index in the component for
      -- caching should be at the last location where the stack ended.
      T.Check_Stack_Indexes ([0, 0]);

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 9);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 8;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (9), Pkt);

      -- A little white box testing. The internal index in the component for
      -- caching should be at the last location where the stack ended.
      T.Check_Stack_Indexes ([0, 0]);

      -- OK let's test some special cases.
      Task_1_Info.Stack_Size := 99;
      Task_2_Info.Stack_Size := 0;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 10);

      -- Check the packet, make sure all zeros:
      Pkt.Header.Sequence_Count := 9;
      Pkt.Buffer (0) := 100;
      Pkt.Buffer (2) := 0;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (10), Pkt);

      -- A little white box testing. The internal index in the component for
      -- caching should be at the last location where the stack ended.
      T.Check_Stack_Indexes ([0, 0]);

      -- No events should have been sent unless a command was sent. No commands were sent.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Stack_Monitoring;

   overriding procedure Test_Packet_Period (Self : in out Instance) is
      T : Component.Stack_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((0, 0), 0);
   begin
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check data product at start:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Packet_Period_History.Get (1), (Value => 1));
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Packet_Period_History.Clear;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Send a command to disable packet sending.
      T.Command_T_Send (T.Commands.Set_Packet_Period ((Value => 0)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Packet_Period_Set_History.Get (1), (Value => 0));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Packet_Period_History.Get (1), (Value => 0));

      -- Send a tick and expect no packets.
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Send a command to set packet sending to every 3.
      T.Command_T_Send (T.Commands.Set_Packet_Period ((Value => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Packet_Period_Set_History.Get (2), (Value => 3));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Period_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Packet_Period_History.Get (2), (Value => 3));

      -- Send a tick and expect no packets.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 7);
   end Test_Packet_Period;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Stack_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Set_Packet_Period ((Value => 3));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Set_Packet_Period_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Stack_Monitor_Tests.Implementation;
