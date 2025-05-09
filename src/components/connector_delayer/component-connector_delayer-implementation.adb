--------------------------------------------------------------------------------
-- Connector_Delayer Component Implementation Body
--------------------------------------------------------------------------------

with Sleep;

package body Component.Connector_Delayer.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Configure how long the component will delay prior to sending out any queued
   -- data.
   --
   -- Init Parameters:
   -- Delay_Us : Natural - The amount of time to delay prior to transmission in
   -- microseconds. The delay time begins right after the element is dequeued from
   -- the Adamant queue.
   --
   overriding procedure Init (Self : in out Instance; Delay_Us : in Natural) is
   begin
      Self.Delay_Us := Delay_Us;
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invokee connector.
   overriding procedure T_Recv_Async (Self : in out Instance; Arg : in T) is
   begin
      -- Delay first:
      if Self.Delay_Us > 0 then
         Sleep.Sleep_Us (Self.Delay_Us);
      end if;

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

end Component.Connector_Delayer.Implementation;
