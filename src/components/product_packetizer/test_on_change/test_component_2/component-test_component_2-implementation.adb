--------------------------------------------------------------------------------
-- Test_Component_2 Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Test_Component_2.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick to regulate the execution of the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   begin
      null;
   end Tick_T_Recv_Sync;

end Component.Test_Component_2.Implementation;
