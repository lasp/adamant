--------------------------------------------------------------------------------
-- Init_Base_Component Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Init_Base_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
   -- TODO declarations
   begin
      null; -- TODO statements
   end Tick_T_Recv_Async;

   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- TODO: Usually in Adamant this procedure is implemented on the invokee side of a connector, so it is
      -- recommended that the appropriate queue overflow action be implemented below.
      -- Example:
      -- -- Throw event:
      -- self.Event_T_Send_If_Connected(self.events.Dropped_Message(
      --    self.Sys_Time_T_Get, arg
      -- ));
      null;
   end Tick_T_Recv_Async_Dropped;

end Component.Init_Base_Component.Implementation;
