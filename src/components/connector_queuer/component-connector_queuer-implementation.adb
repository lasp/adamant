--------------------------------------------------------------------------------
-- Connector_Queuer Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Connector_Queuer.Implementation is

   -- Initialize the dropped message count and send its initial value:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      Self.Dropped_Message_Count.Set_Count (0);
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Dropped_Message_Count (Self.Sys_Time_T_Get, (Value => Self.Dropped_Message_Count.Get_Count)));
   end Set_Up;

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
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Increment the dropped message count and report it as a data product:
      Self.Dropped_Message_Count.Increment_Count;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Dropped_Message_Count (The_Time, (Value => Self.Dropped_Message_Count.Get_Count)));

      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Dropped_Message (The_Time));
   end T_Recv_Async_Dropped;

end Component.Connector_Queuer.Implementation;
