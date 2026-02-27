--------------------------------------------------------------------------------
-- Tick_Listener Component Tester Spec
--------------------------------------------------------------------------------

-- Standard Includes:
with Component.Tick_Listener_Reciprocal;

-- This is the Tick Listener component. It is attached to a tick and provides a connector which will give the caller a count. The count includes the number of times the tick has been received since the last invocation of the connector. This component should be made passive in order to function properly. This component is a useful substitute for the Interrupt Listener component when you want to simulate interrupts with software ticks.
package Component.Tick_Listener.Implementation.Tester is

   -- Component class instance:
   type Instance is new Component.Tick_Listener_Reciprocal.Base_Instance with record
      Component_Instance : aliased Component.Tick_Listener.Implementation.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

end Component.Tick_Listener.Implementation.Tester;
