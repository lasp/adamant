--------------------------------------------------------------------------------
-- Limiter Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Simple_Variable.Assertion; use Simple_Variable.Assertion;
with Tick;
with Tester_Allocator;

package body Variable_Tests.Implementation is

   -- Target-aware tester allocator: heap on Linux, a static instance on
   -- bareboard, where Free also restores the Tester's fresh state.
   -- Generic components must instantiate this themselves; the generated
   -- unit test base cannot (it does not know the concrete types).
   package Tester_Alloc is new Tester_Allocator (
      Tester_Inst => Component_Tester_Package.Instance,
      Tester_Access => Component_Tester_Package.Instance_Access
   );

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Acquire a Tester from the target-aware allocator:
      Self.Tester := Tester_Alloc.Allocate;

      -- Set the logger in the component
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);

      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 4);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Max_Sends_Per_Tick => 3);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;

      -- Release the tester (restores its fresh state on bareboard):
      Tester_Alloc.Free (Self.Tester);
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Queueing_Variable_Length (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 0);
   begin
      -- Send some ticks, expect no sends since no data was enqueued.
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 0);

      -- Enqueue one max size piece of data, make sure one send occurs.
      T.T_Send ((Length => 20, Buffer => [others => 5]));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 25);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 1);
      Simple_Variable_Assert.Eq (T.T_Recv_Sync_History.Get (1), (Length => 20, Buffer => [others => 5]));

      -- Enqueue two max size pieces of data, make sure two sends occur.
      T.T_Send ((Length => 20, Buffer => [others => 2]));
      T.T_Send ((Length => 20, Buffer => [others => 3]));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 1);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 50);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      Simple_Variable_Assert.Eq (T.T_Recv_Sync_History.Get (2), (Length => 20, Buffer => [others => 2]));
      Simple_Variable_Assert.Eq (T.T_Recv_Sync_History.Get (3), (Length => 20, Buffer => [others => 3]));

      -- Enqueue two half size pieces of data, make sure two sends occur.
      T.T_Send ((Length => 10, Buffer => [others => 2]));
      T.T_Send ((Length => 10, Buffer => [others => 3]));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      -- IMPORTANT check, the queue usage should be less than 50% if variable length queueing of the
      -- generic is working!
      Byte_Assert.Lt (T.Component_Instance.Get_Queue_Current_Percent_Used, 40);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 5);
      Simple_Variable_Assert.Eq (T.T_Recv_Sync_History.Get (4), (Length => 10, Buffer => [others => 2]));
      Simple_Variable_Assert.Eq (T.T_Recv_Sync_History.Get (5), (Length => 10, Buffer => [others => 3]));

      -- Enqueue one half size pieces of data, make sure one send occurs.
      T.T_Send ((Length => 9, Buffer => [others => 4]));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 5);
      -- IMPORTANT check, the queue usage should be less than 25% if variable length queueing of the
      -- generic is working!
      Byte_Assert.Lt (T.Component_Instance.Get_Queue_Current_Percent_Used, 25);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 6);
      Simple_Variable_Assert.Eq (T.T_Recv_Sync_History.Get (6), (Length => 9, Buffer => [others => 4]));
   end Test_Queueing_Variable_Length;

end Variable_Tests.Implementation;
