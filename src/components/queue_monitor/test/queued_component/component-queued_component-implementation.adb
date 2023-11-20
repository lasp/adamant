--------------------------------------------------------------------------------
-- Queued_Component Component Implementation Body
--------------------------------------------------------------------------------

with Connector_Types;

package body Component.Queued_Component.Implementation is

   not overriding procedure Enqueue_13_Bytes (Self : in out Instance) is
      The_Time : constant Sys_Time.T := (0, 0);
      Ignore : Connector_Types.Connector_Status;
   begin
      Ignore := Sys_Time_T_Recv_Async_Hook (Self, The_Time, Connector_Index_Type'First);
   end Enqueue_13_Bytes;

   not overriding procedure Drain_Queue (Self : in out Instance) is
      The_Tick : constant Tick.T := ((0, 0), 0);
   begin
      pragma Assert (Tick_T_Recv_Sync_Hook (Self, The_Tick, Connector_Index_Type'First) = Success);
   end Drain_Queue;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Used to queue up time stamps for testing.
   overriding procedure Sys_Time_T_Recv_Async (Self : in out Instance; Arg : in Sys_Time.T) is
      Ignore : Sys_Time.T renames Arg;
   begin
      -- No action.
      null;
   end Sys_Time_T_Recv_Async;

   -- Used to drain entire queue.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Drain the entire queue.
      Ignore : constant Natural := Self.Dispatch_All;
   begin
      -- No other action.
      null;
   end Tick_T_Recv_Sync;

end Component.Queued_Component.Implementation;
