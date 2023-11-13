--------------------------------------------------------------------------------
-- Connector_Protector Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Connector_Protector.Implementation is

   protected body Protected_Connector is

      procedure Call (Self : in out Instance; Arg : in T) is
      begin
         -- Simply call the connector from within the protected
         -- procedure.
         Self.T_Send (Arg);
      end Call;

   end Protected_Connector;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invokee connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
   begin
      -- Call protected connector procedure:
      Self.P_Connector.Call (Self, Arg);
   end T_Recv_Sync;

end Component.Connector_Protector.Implementation;
