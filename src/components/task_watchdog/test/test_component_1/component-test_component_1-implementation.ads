--------------------------------------------------------------------------------
-- Test_Component_1 Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;

-- A simple component which has a pet send connector.
package Component.Test_Component_1.Implementation is

   -- The component class instance record:
   type Instance is new Test_Component_1.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Test_Component_1.Base_Instance with record
      null;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Need a tick to enable an update on the pet connector
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Pet_T_Send message is dropped due to a full queue.
   overriding procedure Pet_T_Send_Dropped (Self : in out Instance; Arg : in Pet.T) is null;

end Component.Test_Component_1.Implementation;
