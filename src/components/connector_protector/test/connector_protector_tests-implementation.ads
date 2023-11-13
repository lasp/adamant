--------------------------------------------------------------------------------
-- Connector_Protector Tests Spec
--------------------------------------------------------------------------------

-- Component Tester Include:
with Component.Connector_Protector.Implementation.Tester;
with Tick;

-- This is a unit test suite for the Connector Protector.
package Connector_Protector_Tests.Implementation is
   -- Test data and state:
   type Instance is new Connector_Protector_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test invokes the protected connector and makes sure the arguments are passed through synchronously, as expected.
   overriding procedure Test_Protected_Call (Self : in out Instance);

   -- Instantiate generic component package:
   package Component_Package is new Component.Connector_Protector (T => Tick.T);
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;

   -- Test data and state:
   type Instance is new Connector_Protector_Tests.Base_Instance with record
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
   end record;
end Connector_Protector_Tests.Implementation;
