--------------------------------------------------------------------------------
-- Tick_Listener Component Tester Body
--------------------------------------------------------------------------------

package body Component.Tick_Listener.Implementation.Tester is

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Attach_Get_Tick_Count_Reciprocal (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Get_Tick_Count_Access);
      Self.Attach_Tick_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Tick_T_Recv_Sync_Access);
   end Connect;

end Component.Tick_Listener.Implementation.Tester;
