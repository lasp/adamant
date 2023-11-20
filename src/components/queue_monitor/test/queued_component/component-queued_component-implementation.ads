--------------------------------------------------------------------------------
-- Queued_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Sys_Time;
with Tick;

-- Component with a queue used for unit testing.
package Component.Queued_Component.Implementation is

   -- The component class instance record:
   type Instance is new Queued_Component.Base_Instance with private;

   -- Public methods used to call connectors:
   not overriding procedure Enqueue_13_Bytes (Self : in out Instance); -- 8 bytes timestamp, 5 bytes overhead
   not overriding procedure Drain_Queue (Self : in out Instance);

private

   -- The component class instance record:
   type Instance is new Queued_Component.Base_Instance with record
      null;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Used to queue up time stamps for testing.
   overriding procedure Sys_Time_T_Recv_Async (Self : in out Instance; Arg : in Sys_Time.T);
   -- This procedure is called when a Sys_Time_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Sys_Time_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Sys_Time.T) is null;
   -- Used to drain entire queue.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

end Component.Queued_Component.Implementation;
