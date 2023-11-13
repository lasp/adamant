--------------------------------------------------------------------------------
-- Interrupt_Pender Tests Spec
--------------------------------------------------------------------------------

-- Component Tester Include:
with Component.Interrupt_Pender.Implementation.Tester;
with Tick;

-- This is a unit test suite for the Interrupt Pender component. It tests the component in a Linux environment, responding to signals.
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test sends many interrupts to the Interrupt Pender component and expects it to produce ticks.
   overriding procedure Test_Interrupt_Handling (Self : in out Instance);

   -- Instantiate generic component:
   package Component_Package is new Component.Interrupt_Pender (Tick.T);
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
   end record;
end Tests.Implementation;
