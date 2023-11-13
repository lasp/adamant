--------------------------------------------------------------------------------
-- Test_Component_2 Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Test_Component_2.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Need a tick to enable an update on the pet connector
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Just send a pet with the count of the tick for testing.
      Self.Pet_1_T_Send_If_Connected ((Count => Arg.Count));
      Self.Pet_2_T_Send_If_Connected ((Count => Arg.Count));
   end Tick_T_Recv_Sync;

end Component.Test_Component_2.Implementation;
