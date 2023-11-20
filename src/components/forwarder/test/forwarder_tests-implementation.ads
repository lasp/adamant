--------------------------------------------------------------------------------
-- Forwarder Tests Spec
--------------------------------------------------------------------------------

-- Component Tester Include:
with Component.Forwarder.Implementation.Tester;
with Tick;

-- This is a unit test suite for the Forwarder component.
package Forwarder_Tests.Implementation is
   -- Test data and state:
   type Instance is new Forwarder_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests initialization.
   overriding procedure Test_Init (Self : in out Instance);
   -- This unit test sends commands to enable and disable forwarding and make sure it functions correctly.
   overriding procedure Test_Enable_Disable_Forwarding (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Instantiate generic component package:
   package Component_Package is new Component.Forwarder (T => Tick.T);
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;

   -- Test data and state:
   type Instance is new Forwarder_Tests.Base_Instance with record
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
   end record;
end Forwarder_Tests.Implementation;
