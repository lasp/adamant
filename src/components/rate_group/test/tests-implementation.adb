--------------------------------------------------------------------------------
-- Rate_Group Tests Body
--------------------------------------------------------------------------------

with Connector_Types;
with Basic_Assertions; use Basic_Assertions;
with Tick.Assertion; use Tick.Assertion;
--with Time_Exceeded.Assertion; use Time_Exceeded.Assertion;
with Cycle_Slip_Param.Assertion; use Cycle_Slip_Param.Assertion;
with Full_Queue_Param.Assertion; use Full_Queue_Param.Assertion;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10, Tick_T_Send_Count => 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Initialize the component:
      Self.Tester.Component_Instance.Init (Ticks_Per_Timing_Report => 1, Timing_Report_Delay_Ticks => 0, Issue_Time_Exceeded_Events => True);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Nominal (Self : in out Instance) is
      T : Component.Rate_Group.Implementation.Tester.Instance_Access renames Self.Tester;
      Thetick : constant Tick.T := (Time => (1, 1), Count => 1);
   begin
      -- Send the first tick to the component:
      T.Tick_T_Send (Thetick);
      Boolean_Assert.Eq (T.Tick_T_Recv_Sync_History.Is_Empty, True);

      -- Execute the component:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure ticks were send out of the component:
      Natural_Assert.Eq (T.Tick_T_Recv_Sync_History.Get_Count, 2);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (1), Thetick);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (2), Thetick);

      -- Make sure there were no cycle slips:
      Boolean_Assert.Eq (T.Cycle_Slip_History.Is_Empty, True);

      -- Make sure pet sent out:
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);
   end Nominal;

   overriding procedure Cycle_Slip_Trigger (Self : in out Instance) is
      T : Component.Rate_Group.Implementation.Tester.Instance_Access renames Self.Tester;
      Thetick : constant Tick.T := (Time => (1, 1), Count => 1);
   begin
      --------------------------------------------------
      -- Send the first tick to the component:
      T.Tick_T_Send (Thetick);

      -- Send a second tick to the component which will trigger a cycle slip:
      T.Tick_T_Send (Thetick);
      Boolean_Assert.Eq (T.Tick_T_Recv_Sync_History.Is_Empty, True);

      -- Execute the component:
      Natural_Assert.Eq (T.Dispatch_N (1), 1);

      -- Make sure ticks were send out of the component:
      Natural_Assert.Eq (T.Tick_T_Recv_Sync_History.Get_Count, 2);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (1), Thetick);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (2), Thetick);

      -- Make sure pet sent out:
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);

      -- Make sure a single cycle slip was triggered:
      Natural_Assert.Eq (T.Cycle_Slip_History.Get_Count, 1);
      Cycle_Slip_Param_Assert.Eq (T.Cycle_Slip_History.Get (1), (Slipped_Tick => Thetick, Num_Slips => 1));

      --------------------------------------------------
      -- Make sure another cycle slip is not triggered on the next execution:
      Natural_Assert.Eq (T.Dispatch_N (1), 1);

      -- Make sure ticks were send out of the component:
      Natural_Assert.Eq (T.Tick_T_Recv_Sync_History.Get_Count, 4);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (3), Thetick);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (4), Thetick);

      -- Make sure pet sent out:
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 2);

      -- Make sure there is still only 1 cycle slip:
      Natural_Assert.Eq (T.Cycle_Slip_History.Get_Count, 1);

      --------------------------------------------------
      -- Trigger another slip and make sure the count increases:
      -- Send the first tick to the component:
      T.Tick_T_Send (Thetick);

      -- Send a second tick to the component which will trigger a cycle slip:
      T.Tick_T_Send (Thetick);

      -- Execute the component:
      Natural_Assert.Eq (T.Dispatch_N (1), 1);

      -- Make sure ticks were send out of the component:
      Natural_Assert.Eq (T.Tick_T_Recv_Sync_History.Get_Count, 6);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (5), Thetick);
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (6), Thetick);

      -- Make sure pet sent out:
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 3);

      -- Make sure a single cycle slip was triggered:
      Natural_Assert.Eq (T.Cycle_Slip_History.Get_Count, 2);
      Cycle_Slip_Param_Assert.Eq (T.Cycle_Slip_History.Get (2), (Slipped_Tick => Thetick, Num_Slips => 2));
   end Cycle_Slip_Trigger;

   overriding procedure Time_Reporting (Self : in out Instance) is
      T : Component.Rate_Group.Implementation.Tester.Instance_Access renames Self.Tester;
      Thetick1 : constant Tick.T := (Time => (0, 0), Count => 1);
      --theTick2 : constant Tick.T := (Time => (0, 0), Count => 2);
      --theTick3 : constant Tick.T := (Time => (6, 7), Count => 3);
   begin
      --------------------------------------------------
      -- Send the first tick to the component and make
      -- sure that the correct events are thrown:
      T.Tick_T_Send (Thetick1);

      -- Execute the component:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Cycle_Slip_History.Get_Count, 0);
      Natural_Assert.Eq (T.Max_Cycle_Time_Exceeded_History.Get_Count, 1);
      --Time_Exceeded_Assert.eq(t.Max_Cycle_Time_Exceeded_History.get(1), (Time_Delta => (2, 0), Count => 1));
      Natural_Assert.Eq (T.Max_Execution_Time_Exceeded_History.Get_Count, 1);
      --Time_Exceeded_Assert.eq(t.Max_Execution_Time_Exceeded_History.get(1), (Time_Delta => (2, 0), Count => 1));

      -- Check data products:
      Natural_Assert.Eq (T.Timing_Report_History.Get_Count, 1);

      -- Make sure pet sent out:
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);

      --
      -- We can no longer test the following since we are using Ada.Real_Time and Ada.Execution_Time
      -- instead of Sys_Time arithmetic in the component.
      --
      --   --------------------------------------------------
      --   -- Send another of the same tick, we expect
      --   -- execution time to stay the same but execution
      --   -- time to go up.
      --   t.Tick_T_Send(theTick2);
      --
      --   -- Execute the component:
      --   Natural_Assert.eq(t.dispatch_all, 1);

      --   -- Check events:
      --   Natural_Assert.eq(t.Cycle_Slip_History.Get_Count, 0);
      --   Natural_Assert.eq(t.Max_Cycle_Time_Exceeded_History.Get_Count, 2);
      --   Time_Exceeded_Assert.eq(t.Max_Cycle_Time_Exceeded_History.get(2), (Time_Delta => (4, 0), Count => 2));
      --   Natural_Assert.eq(t.Max_Execution_Time_Exceeded_History.Get_Count, 1);

      --   --------------------------------------------------
      --   -- For this test change the time delta so more
      --   -- elapsed time occurs during execution. This
      --   -- should trigger the mex execution time. The
      --   -- tick time is set high enough that a new
      --   -- cycle time should not be thrown:
      --   t.Seconds_Delta := 2;
      --   t.Subseconds_Delta := 0;
      --   t.Tick_T_Send(theTick3);

      --   -- Execute the component:
      --   Natural_Assert.eq(t.dispatch_all, 1);

      --   -- Check events:
      --   Natural_Assert.eq(t.Cycle_Slip_History.Get_Count, 0);
      --   Natural_Assert.eq(t.Max_Cycle_Time_Exceeded_History.Get_Count, 2);
      --   Natural_Assert.eq(t.Max_Execution_Time_Exceeded_History.Get_Count, 2);
      --   Time_Exceeded_Assert.eq(t.Max_Execution_Time_Exceeded_History.get(2), (Time_Delta => (4, 0), Count => 3));
   end Time_Reporting;

   overriding procedure Full_Queue (Self : in out Instance) is
      T : Component.Rate_Group.Implementation.Tester.Instance_Access renames Self.Tester;
      Thetick1 : constant Tick.T := (Time => (0, 0), Count => 1);
   begin
      -- Set up the tester component to return the full queue status when invoked
      -- by the rate group component:
      T.Connector_Tick_T_Recv_Sync_Status := Connector_Types.Message_Dropped;

      -- Send a tick to the component:
      T.Tick_T_Send (Thetick1);

      -- Execute the component:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure correct events thrown:
      Natural_Assert.Eq (T.Component_Has_Full_Queue_History.Get_Count, 2);
      Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (1), (Dropped_Tick => Thetick1, Index => 1));
      Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (2), (Dropped_Tick => Thetick1, Index => 3));

      -- Make sure pet sent out:
      Natural_Assert.Eq (T.Pet_T_Recv_Sync_History.Get_Count, 1);

      -- Check data products:
      Natural_Assert.Eq (T.Timing_Report_History.Get_Count, 1);
   end Full_Queue;

   overriding procedure Test_Dropped_Tick (Self : in out Instance) is
      T : Component.Rate_Group.Implementation.Tester.Instance_Access renames Self.Tester;
      Thetick : constant Tick.T := (Time => (0, 0), Count => 1);

      procedure Fill_Queue (N : Natural := 10) is
      begin
         for Idxx in 1 .. N loop
            T.Tick_T_Send (Thetick);
         end loop;
      end Fill_Queue;
   begin
      -- The queue depth is 10. Lets fill up the queue with commands and not let the component execute:
      Fill_Queue;

      -- Send another tick and expect an event to be thrown:
      T.Expect_Tick_T_Send_Dropped := True;
      T.Tick_T_Send (Thetick);
      Natural_Assert.Eq (T.Tick_T_Send_Dropped_Count, 1);
      T.Expect_Tick_T_Send_Dropped := True;
      T.Tick_T_Send (Thetick);
      Natural_Assert.Eq (T.Tick_T_Send_Dropped_Count, 2);
      Natural_Assert.Eq (T.Incoming_Tick_Dropped_History.Get_Count, 2);
      Tick_Assert.Eq (T.Incoming_Tick_Dropped_History.Get (1), Thetick);
      Tick_Assert.Eq (T.Incoming_Tick_Dropped_History.Get (2), Thetick);
   end Test_Dropped_Tick;

end Tests.Implementation;
