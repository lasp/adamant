--------------------------------------------------------------------------------
-- Example_Component Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Packet.Assertion; use Packet.Assertion;

package body Example_Component_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Setup flags to log only packets
      Self.Tester.Log_Sys_Time_T_Get := False;
      Self.Tester.Log_Tick_T_Recv_Sync := False;
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Packet_T_Send_Count => 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

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

   overriding procedure Test_That_Should_Pass (Self : in out Instance) is
      -- Define the packet we are going to compare against.
      A_Packet : Packet.T := (
         Header => (
            Time => (0, 0),
            Id => 0,
            Sequence_Count => 0,
            Buffer_Length => 1
         ),
         Buffer => [others => 0]
      );
   begin
      -- Send the example component a tick:
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Check the tester component histories. The example component should
      -- have requested Time one time through the Sys_Time.T connector.
      Natural_Assert.Eq (Self.Tester.Sys_Time_T_Return_History.Get_Count, 1);

      -- The component should have sent out 3 packets, one to each of its
      -- 3 Packet.T connections. All three of these connectors are connected
      -- to the tester Packet_T_Recv_Sync connector, so we should have gotten
      -- 3 packets in that history.
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- OK now check the contents of the packets.
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (1), A_Packet);
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (2), A_Packet);
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (3), A_Packet);

      -- Send another tick:
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Check the tester component histories again.
      Natural_Assert.Eq (Self.Tester.Sys_Time_T_Return_History.Get_Count, 2);
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 6);

      -- OK now check the contents of the packets. Expect the data to have
      -- incremented this time.
      A_Packet.Buffer (0) := 1;
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (4), A_Packet);
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (5), A_Packet);
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (6), A_Packet);
   end Test_That_Should_Pass;

   overriding procedure Test_That_Should_Fail (Self : in out Instance) is
   begin
      -- Setup flags to log only packets
      Self.Tester.Log_Sys_Time_T_Get := False;
      Self.Tester.Log_Tick_T_Recv_Sync := False;
      -- Send the example component a tick:
      Self.Log ("Sending a Tick with Log 1");
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Make sure the internal counter inside the component is now 1:
      Byte_Assert.Eq (Self.Tester.Get_Component_Counter, 1);

      -- Send the example component a tick:
      Self.Tester.Log ("Sending a Tick with Log 2");
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Make sure the internal counter inside the component is now 2:
      Byte_Assert.Eq (Self.Tester.Get_Component_Counter, 2);

      -- Send the example component a tick:
      Self.Tester.Log ("    Sending a Tick with spaces and Log 2");
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Make sure the internal counter inside the component is now 3:
      Byte_Assert.Eq (Self.Tester.Get_Component_Counter, 3);
   end Test_That_Should_Fail;

end Example_Component_Tests.Implementation;
