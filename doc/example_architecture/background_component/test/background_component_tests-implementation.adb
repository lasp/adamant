--------------------------------------------------------------------------------
-- Background_Component Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Packet.Assertion; use Packet.Assertion;

package body Background_Component_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

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

   overriding procedure Test_Nominal (Self : in out Instance) is
      -- Define the packet we are going to compare against.
      A_Packet : Packet.T := (
         Header => (
            Time => (0, 0),
            Id => 0,
            Sequence_Count => 0,
            Buffer_Length => 2
         ),
         Buffer => [others => 0]
      );
   begin
      -- Put some data in the component's queue:
      Self.Tester.Packed_U16_T_Send ((Value => 4));
      Self.Tester.Packed_U16_T_Send ((Value => 6));

      -- Expect no packets to be send out:
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Tell the component to execute its "cycle" function, simulating the
      -- execution of the component's task.
      Self.Tester.Cycle_Component;

      -- Expect one packet with a U16 in it:
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 1);
      A_Packet.Buffer (0) := 0;
      A_Packet.Buffer (1) := 6;
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (1), A_Packet);

      -- Put some data in the component's queue:
      Self.Tester.Packed_U16_T_Send ((Value => 15));

      -- Tell the component to execute its "cycle" function, simulating the
      -- execution of the component's task (twice).
      Self.Tester.Cycle_Component;
      Self.Tester.Cycle_Component;

      -- Expect two more packets:
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 3);
      A_Packet.Buffer (0) := 0;
      A_Packet.Buffer (1) := 15;
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (2), A_Packet);
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (3), A_Packet);
   end Test_Nominal;

end Background_Component_Tests.Implementation;
