--------------------------------------------------------------------------------
-- Splitter Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Splitter.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invokee connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
   begin
      -- Send T to any output connector that we have that is currently connected:
      for Index in Self.Connector_T_Send'Range loop
         Self.T_Send_If_Connected (Index, Arg);
      end loop;
   end T_Recv_Sync;

end Component.Splitter.Implementation;
