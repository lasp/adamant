--------------------------------------------------------------------------------
-- Queued_Component Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Packet.Assertion; use Packet.Assertion;

package body Queued_Component_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

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
            Buffer_Length => 1
         ),
         Buffer => [others => 0]
      );
   begin
      -- Put some data in the component's queue:
      Self.Tester.Packed_Byte_T_Send ((Value => 5));
      Self.Tester.Packed_U16_T_Send ((Value => 4));

      -- Expect no packets to be send out:
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send a tick:
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Expect one packet with a U16 in it:
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 1);
      A_Packet.Header.Buffer_Length := 2;
      A_Packet.Buffer (0) := 0;
      A_Packet.Buffer (1) := 4;
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (1), A_Packet);

      -- Put some data in the component's queue:
      Self.Tester.Packed_U16_T_Send ((Value => 15));
      Self.Tester.Packed_Byte_T_Send ((Value => 12));

      -- Send a tick:
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Expect a second packet with a byte in it:
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 2);
      A_Packet.Header.Buffer_Length := 1;
      A_Packet.Buffer (0) := 12;
      A_Packet.Buffer (1) := 0;
      Packet_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get (2), A_Packet);
   end Test_Nominal;

   overriding procedure Test_Queue_Overflow (Self : in out Instance) is
   begin
      -- Fill up the queue:
      Self.Tester.Packed_U16_T_Send ((Value => 4));
      Self.Tester.Packed_U16_T_Send ((Value => 4));
      Self.Tester.Packed_U16_T_Send ((Value => 4));
      -- This call should produce a unit test failure.
      Self.Tester.Packed_U16_T_Send ((Value => 4));
   end Test_Queue_Overflow;

end Queued_Component_Tests.Implementation;
