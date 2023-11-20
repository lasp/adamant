--------------------------------------------------------------------------------
-- Fault_Component Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Packed_U32.Assertion; use Packed_U32.Assertion;

package body Fault_Component_Tests.Implementation is

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
      -- A tick with a zero seconds timestamp should always produce a fault:
      Self.Tester.Tick_T_Send ((Time => (0, 14), Count => 12));

      -- Make sure we got a fault:
      Natural_Assert.Eq (Self.Tester.Fault_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Zero_Time_Fault_History.Get_Count, 1);

      -- If we send a non-zero seconds value we should not get a fault:
      Self.Tester.Tick_T_Send ((Time => (1, 14), Count => 12));
      Natural_Assert.Eq (Self.Tester.Fault_T_Recv_Sync_History.Get_Count, 1);

      -- If we send continuous seconds values we should not get a fault,
      -- (within 1-2 seconds):
      Self.Tester.Tick_T_Send ((Time => (2, 14), Count => 12));
      Self.Tester.Tick_T_Send ((Time => (3, 14), Count => 12));
      Self.Tester.Tick_T_Send ((Time => (5, 14), Count => 12));
      Self.Tester.Tick_T_Send ((Time => (7, 14), Count => 12));
      Natural_Assert.Eq (Self.Tester.Fault_T_Recv_Sync_History.Get_Count, 1);

      -- If we send non-continuous seconds value we should get a fault:
      Self.Tester.Tick_T_Send ((Time => (7, 14), Count => 12));
      Natural_Assert.Eq (Self.Tester.Fault_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (Self.Tester.Discontinuous_Time_Fault_History.Get_Count, 1);
      Packed_U32_Assert.Eq (Self.Tester.Discontinuous_Time_Fault_History.Get (1), (Value => 7));

      -- Again.
      Self.Tester.Tick_T_Send ((Time => (10, 14), Count => 12));
      Natural_Assert.Eq (Self.Tester.Fault_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (Self.Tester.Discontinuous_Time_Fault_History.Get_Count, 2);
      Packed_U32_Assert.Eq (Self.Tester.Discontinuous_Time_Fault_History.Get (2), (Value => 10));
   end Unit_Test;

end Fault_Component_Tests.Implementation;
