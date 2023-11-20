--------------------------------------------------------------------------------
-- Connector_Queuer Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Connector_Queuer.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invokee connector.
   overriding procedure T_Recv_Async (Self : in out Instance; Arg : in T) is
   begin
      -- Forward the incoming data along:
      Self.T_Send (Arg);
   end T_Recv_Async;

   -- This procedure is called when a T_Recv_Async message is dropped due to a full queue.
   overriding procedure T_Recv_Async_Dropped (Self : in out Instance; Arg : in T) is
      Ignore : T renames Arg;
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Message (Self.Sys_Time_T_Get));
   end T_Recv_Async_Dropped;

end Component.Connector_Queuer.Implementation;
