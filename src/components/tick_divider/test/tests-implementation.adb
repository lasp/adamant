--------------------------------------------------------------------------------
-- Tick_Divider Tests Body
--------------------------------------------------------------------------------

with Connector_Types;
with Sys_Time;
with AUnit.Assertions; use AUnit.Assertions;
with Interfaces; use Interfaces;
with Basic_Assertions; use Basic_Assertions;
with Tick.Assertion; use Tick.Assertion;
with Td_Full_Queue_Param.Assertion; use Td_Full_Queue_Param.Assertion;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Tick_T_Send_Count => 4);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
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
      T : Component.Tick_Divider.Implementation.Tester.Instance_Access renames Self.Tester;
      Systime : constant Sys_Time.T := (Seconds => 15, Subseconds => 26);
      Dividers : aliased Component.Tick_Divider.Divider_Array_Type := [1 => 5, 2 => 0, 3 => 7, 4 => 2];
   begin
      -- Init component:
      T.Component_Instance.Init (Dividers'Unchecked_Access);

      -- Make sure count and max_Count are correct:
      Boolean_Assert.Eq (T.Check_Counts (Count => 0, Max_Count => 70), True);

      -- Send a series of ticks and make sure that we get all the expected calls:
      for Idx in 0 .. 74 loop
         T.Tick_T_Send ((Time => Systime, Count => Unsigned_32 (Idx)));
      end loop;

      -- Make sure rollover occurred:
      Boolean_Assert.Eq (T.Check_Counts (Count => 5, Max_Count => 70), True);

      -- We are expecting 74/7 + 74/5 + 2 (for 0th iteration) for
      -- a total number invocations of 26:
      Natural_Assert.Eq (T.Tick_T_Recv_Sync_History.Get_Count, 26);

      -- Go through the entire history and check it to make sure we
      -- got everything we expected:
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (1), (Systime, 0));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (2), (Systime, 0));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (3), (Systime, 5));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (4), (Systime, 7));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (5), (Systime, 10));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (6), (Systime, 14));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (7), (Systime, 15));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (8), (Systime, 20));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (9), (Systime, 21));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (10), (Systime, 25));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (11), (Systime, 28));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (12), (Systime, 30));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (13), (Systime, 35));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (14), (Systime, 35));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (15), (Systime, 40));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (16), (Systime, 42));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (17), (Systime, 45));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (18), (Systime, 49));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (19), (Systime, 50));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (20), (Systime, 55));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (21), (Systime, 56));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (22), (Systime, 60));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (23), (Systime, 63));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (24), (Systime, 65));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (25), (Systime, 70));
      Tick_Assert.Eq (T.Tick_T_Recv_Sync_History.Get (26), (Systime, 70));

   end Nominal;

   overriding procedure Bad_Setup (Self : in out Instance) is
      T : Component.Tick_Divider.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal is
         Dividers : aliased Component.Tick_Divider.Divider_Array_Type := [1 => 0, 2 => 1, 3 => 0, 4 => 67];
      begin
         T.Component_Instance.Init (Dividers'Unchecked_Access);
      exception
         -- Expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init failed!");
      end Init_Nominal;

      procedure Init_Too_Few_Arguments is
         Dividers : aliased Component.Tick_Divider.Divider_Array_Type := [0, 1, 3];
      begin
         T.Component_Instance.Init (Dividers'Unchecked_Access);
         -- Should never get here:
         Assert (False, "Too few arguments to init did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Too_Few_Arguments;

      procedure Init_Too_Many_Arguments is
         Dividers : aliased Component.Tick_Divider.Divider_Array_Type := [0, 1, 2, 3, 5, 6];
      begin
         T.Component_Instance.Init (Dividers'Unchecked_Access);
         -- Should never get here:
         Assert (False, "Too many arguments to init did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Too_Many_Arguments;

      procedure Init_Bad_Index is
         Dividers : aliased Component.Tick_Divider.Divider_Array_Type := [2 => 0, 3 => 1, 4 => 0, 5 => 7];
      begin
         T.Component_Instance.Init (Dividers'Unchecked_Access);
         -- Should never get here:
         Assert (False, "Init bad index 1 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Bad_Index;

   begin
      Init_Nominal;
      Init_Too_Few_Arguments;
      Init_Too_Many_Arguments;
      Init_Bad_Index;
   end Bad_Setup;

   overriding procedure Full_Queue (Self : in out Instance) is
      T : Component.Tick_Divider.Implementation.Tester.Instance_Access renames Self.Tester;
      Thetick1 : constant Tick.T := (Time => (0, 0), Count => 1);
      Dividers : aliased Component.Tick_Divider.Divider_Array_Type := [1 => 1, 2 => 0, 3 => 2, 4 => 0];
   begin
      -- Init component:
      T.Component_Instance.Init (Dividers'Unchecked_Access);

      -- Set up the tester component to return the full queue status when invoked
      -- by the rate group component:
      T.Connector_Tick_T_Recv_Sync_Status := Connector_Types.Message_Dropped;

      -- Send four ticks to the component:
      T.Tick_T_Send (Thetick1);
      T.Tick_T_Send (Thetick1);
      T.Tick_T_Send (Thetick1);
      T.Tick_T_Send (Thetick1);

      -- Make sure correct events thrown:
      Natural_Assert.Eq (T.Component_Has_Full_Queue_History.Get_Count, 6);
      Td_Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (1), (Dropped_Tick => Thetick1, Index => 1));
      Td_Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (2), (Dropped_Tick => Thetick1, Index => 3));
      Td_Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (3), (Dropped_Tick => Thetick1, Index => 1));
      Td_Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (4), (Dropped_Tick => Thetick1, Index => 1));
      Td_Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (5), (Dropped_Tick => Thetick1, Index => 3));
      Td_Full_Queue_Param_Assert.Eq (T.Component_Has_Full_Queue_History.Get (6), (Dropped_Tick => Thetick1, Index => 1));
   end Full_Queue;

end Tests.Implementation;
