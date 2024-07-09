--------------------------------------------------------------------------------
-- Packet_Component Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Tick.Assertion; use Tick.Assertion;

package body Packet_Component_Tests.Implementation is

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
      -- Send some ticks and check for data products.
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));
      Self.Tester.Tick_T_Send ((Time => (0, 0), Count => 0));

      -- Make sure 4 total packets were sent out:
      Natural_Assert.Eq (Self.Tester.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Make sure 2 Counter packets were sent:
      Natural_Assert.Eq (Self.Tester.Counter_History.Get_Count, 2);
      Byte_Array_Assert.Eq (Self.Tester.Counter_History.Get (1).Buffer (0 .. 1), [0, 0]);
      Byte_Array_Assert.Eq (Self.Tester.Counter_History.Get (2).Buffer (0 .. 1), [0, 1]);

      -- Make sure 2 Last_Tick_Received packets were sent:
      Natural_Assert.Eq (Self.Tester.Last_Tick_Received_History.Get_Count, 2);
      Tick_Assert.Eq (
         Self.Tester.Last_Tick_Received_History.Get (1),
         (Time => (0, 0), Count => 0)
      );
      Tick_Assert.Eq (Self.Tester.Last_Tick_Received_History.Get (2),
         (Time => (0, 0), Count => 0)
      );
   end Unit_Test;

end Packet_Component_Tests.Implementation;
