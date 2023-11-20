--------------------------------------------------------------------------------
-- Connector_Protector Component Tester Body
--------------------------------------------------------------------------------

package body Component.Connector_Protector.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.T_Recv_Sync_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.T_Recv_Sync_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.T_Recv_Sync_Access);
      Self.Attach_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invoker connector. Calls originating from this connector are contained within a protected object and thus downstream operations are atomic and thread-safe.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.T_Recv_Sync_History.Push (Arg);
   end T_Recv_Sync;

end Component.Connector_Protector.Implementation.Tester;
