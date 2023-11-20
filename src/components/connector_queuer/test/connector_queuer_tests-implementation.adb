--------------------------------------------------------------------------------
-- Connector_Queuer Tests Body
--------------------------------------------------------------------------------

with Safe_Deallocator;
with Basic_Assertions; use Basic_Assertions;
with Tick.Assertion; use Tick.Assertion;

package body Connector_Queuer_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Dynamically allocate the generic component tester:
      Self.Tester := new Component_Tester_Package.Instance;

      -- Set the logger in the component
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);

      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      -- Free the tester component:
      procedure Free_Tester is new Safe_Deallocator.Deallocate_If_Testing (Object => Component_Tester_Package.Instance, Name => Component_Tester_Package.Instance_Access);
   begin
      -- Free component heap:
      Self.Tester.Final_Base;

      -- Delete tester:
      Free_Tester (Self.Tester);
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Queued_Call (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
   begin
      -- Call the connector:
      T.T_Send (((1, 2), 3));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);

      -- Expect tick to be passed through:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 1);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (1), ((1, 2), 3));

      -- Call the connector:
      T.T_Send (((2, 4), 6));
      T.T_Send (((3, 5), 7));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 1);

      -- Expect tick to be passed through:
      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (2), ((2, 4), 6));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (3), ((3, 5), 7));

      -- Call the connector:
      T.T_Send (((8, 10), 12));
      T.T_Send (((9, 11), 13));
      T.T_Send (((15, 15), 15));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);

      -- Expect tick to be passed through:
      Natural_Assert.Eq (T.Dispatch_All, 3);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 6);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (4), ((8, 10), 12));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (5), ((9, 11), 13));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (6), ((15, 15), 15));
   end Test_Queued_Call;

   overriding procedure Test_Full_Queue (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
   begin
      -- Fill the queue:
      T.T_Send (((8, 10), 12));
      T.T_Send (((9, 11), 13));
      T.T_Send (((15, 15), 15));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);

      -- Expect next one to be dropped:
      T.Expect_T_Send_Dropped := True;
      T.T_Send (((13, 13), 13));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Message_History.Get_Count, 1);

      -- Expect tick to be passed through now.
      Natural_Assert.Eq (T.Dispatch_All, 3);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (1), ((8, 10), 12));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (2), ((9, 11), 13));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (3), ((15, 15), 15));
   end Test_Full_Queue;

end Connector_Queuer_Tests.Implementation;
