--------------------------------------------------------------------------------
-- Limiter Tests Spec
--------------------------------------------------------------------------------

-- Component Tester Include:
with Component.Limiter.Implementation.Tester;
with Simple_Variable;

-- This is unit test suite tests out the variable queueing of a generic component. The purpose of this unit test is not to test the limiter specifically, but the autocoding logic of a generic asynchronous connector of variable length type.
package Variable_Tests.Implementation is
   -- Test data and state:
   type Instance is new Variable_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test excersizes the limiter component while queueing variable length packets.
   overriding procedure Test_Queueing_Variable_Length (Self : in out Instance);

   -- Instantiate generic component package:
   package Component_Package is new Component.Limiter (T => Simple_Variable.T, Serialized_Length => Simple_Variable.Serialized_Length);
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;

   -- Test data and state:
   type Instance is new Variable_Tests.Base_Instance with record
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
   end record;
end Variable_Tests.Implementation;
