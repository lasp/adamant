--------------------------------------------------------------------------------
-- Event_Component Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Tick.Assertion; use Tick.Assertion;

package body Event_Component_Tests.Implementation is

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

   overriding procedure Unit_Test (Self : in out Instance) is
   begin
      -- Send some ticks and check for events.
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Make sure a total of 13 events were received.
      Natural_Assert.Eq (Self.Tester.Event_T_Recv_Sync_History.Get_Count, 13);

      -- Check the individual event histories to make sure the correct
      -- number of each event was sent:
      Natural_Assert.Eq (Self.Tester.First_Tick_Received_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Tick_Received_History.Get_Count, 11);
      Natural_Assert.Eq (Self.Tester.Ten_More_Ticks_Received_History.Get_Count, 1);

      -- Check the event parameters and make sure they are as expected.
      Tick_Assert.Eq (
         Self.Tester.Tick_Received_History.Get (11),
         (Time => (0, 0), Count => 0)
      );
      Packed_U16_Assert.Eq (
         Self.Tester.Ten_More_Ticks_Received_History.Get (1),
         (Value => 10)
      );
   end Unit_Test;

end Event_Component_Tests.Implementation;
