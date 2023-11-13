--------------------------------------------------------------------------------
-- Discriminated_Component Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Discriminated_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- TODO declarations
   begin
      null; -- TODO statements
   end Tick_T_Recv_Sync;

end Component.Discriminated_Component.Implementation;
