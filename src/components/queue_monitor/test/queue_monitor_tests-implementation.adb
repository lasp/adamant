--------------------------------------------------------------------------------
-- Queue_Monitor Tests Body
--------------------------------------------------------------------------------

with Tick;
with Basic_Assertions; use Basic_Assertions;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces;
with Component.Queued_Component.Implementation;
with Packet.Assertion; use Packet.Assertion;

package body Queue_Monitor_Tests.Implementation is

   Queued_Component_1 : aliased Component.Queued_Component.Implementation.Instance;
   Queued_Component_2 : aliased Component.Queued_Component.Implementation.Instance;
   Queued_Component_3 : aliased Component.Queued_Component.Implementation.Instance;

   Queued_Component_List : aliased Component.Component_List := [Queued_Component_1'Access, Queued_Component_2'Access, Queued_Component_3'Access];

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Allocate queues for the queued components:
      Queued_Component_1.Init_Base (Component.Queued_Component.Max_Queue_Element_Size * 10);
      Queued_Component_2.Init_Base (Component.Queued_Component.Max_Queue_Element_Size * 10);
      Queued_Component_3.Init_Base (Component.Queued_Component.Max_Queue_Element_Size * 10);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Queued_Component_List => Queued_Component_List'Access, Packet_Period => 1);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Empty the queues in the queued components:
      Queued_Component_1.Drain_Queue;
      Queued_Component_2.Drain_Queue;
      Queued_Component_3.Drain_Queue;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Queue_Monitoring (Self : in out Instance) is
      T : Component.Queue_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((0, 0), 0);
      Pkt : Packet.T := (Header => (Time => (0, 0), Id => T.Packets.Get_Queue_Usage_Packet_Id, Sequence_Count => 0, Buffer_Length => 6), Buffer => [others => 0]);
   begin
      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet, make sure all zeros:
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1), Pkt);

      -- Enqueue some items:
      Queued_Component_1.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet:
      Pkt.Header.Sequence_Count := 1;
      Pkt.Buffer (0) := 10;
      Pkt.Buffer (1) := 10;
      Pkt.Buffer (2) := 20;
      Pkt.Buffer (3) := 20;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (2), Pkt);

      -- Enqueue some more items:
      Queued_Component_1.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Check the packet:
      Pkt.Header.Sequence_Count := 2;
      Pkt.Buffer (0) := 20;
      Pkt.Buffer (1) := 20;
      Pkt.Buffer (2) := 40;
      Pkt.Buffer (3) := 40;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (3), Pkt);

      -- Enqueue some more items:
      Queued_Component_1.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Check the packet:
      Pkt.Header.Sequence_Count := 3;
      Pkt.Buffer (0) := 30;
      Pkt.Buffer (1) := 30;
      Pkt.Buffer (2) := 100;
      Pkt.Buffer (3) := 100;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (4), Pkt);

      -- Enqueue some more items:
      Queued_Component_1.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;
      Queued_Component_2.Enqueue_13_Bytes;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);

      -- Check the packet:
      Pkt.Header.Sequence_Count := 4;
      Pkt.Buffer (0) := 40;
      Pkt.Buffer (1) := 40;
      Pkt.Buffer (2) := 100;
      Pkt.Buffer (3) := 100;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (5), Pkt);

      -- OK, clear all the queues and check. Make sure max queue values
      -- remain, but current ones get set to zero:
      Queued_Component_1.Drain_Queue;
      Queued_Component_2.Drain_Queue;
      Queued_Component_3.Drain_Queue;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);

      -- Check the packet:
      Pkt.Header.Sequence_Count := 5;
      Pkt.Buffer (0) := 0;
      Pkt.Buffer (1) := 40;
      Pkt.Buffer (2) := 0;
      Pkt.Buffer (3) := 100;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (6), Pkt);

      -- Enqueue some more items:
      Queued_Component_3.Enqueue_13_Bytes;
      Queued_Component_3.Enqueue_13_Bytes;
      Queued_Component_3.Enqueue_13_Bytes;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 7);

      -- Check the packet:
      Pkt.Header.Sequence_Count := 6;
      Pkt.Buffer (0) := 0;
      Pkt.Buffer (1) := 40;
      Pkt.Buffer (2) := 0;
      Pkt.Buffer (3) := 100;
      Pkt.Buffer (4) := 30;
      Pkt.Buffer (5) := 30;
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (7), Pkt);
   end Test_Queue_Monitoring;

   overriding procedure Test_Packet_Period (Self : in out Instance) is
      T : Component.Queue_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
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

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Packet_Period_Set_History.Get (2), (Value => 3));

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
      T : Component.Queue_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
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

end Queue_Monitor_Tests.Implementation;
