--------------------------------------------------------------------------------
-- Limiter Tests Spec
--------------------------------------------------------------------------------

-- Component Tester Include:
with Component.Limiter.Implementation.Tester;
with Tick;

-- This is a unit test suite for the Limiter component
package Limiter_Tests.Implementation is
   -- Test data and state:
   type Instance is new Limiter_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises the limiter component, making sure it behaves as expected.
   overriding procedure Test_Nominal_Limiting (Self : in out Instance);
   -- This unit test exercises the change rate command of the limiter and makes sure it works.
   overriding procedure Test_Change_Rate_Command (Self : in out Instance);
   -- This unit test exercises the change rate parameter of the limiter and makes sure it works.
   overriding procedure Test_Change_Rate_Parameter (Self : in out Instance);
   -- This unit test exercises that a queue overflow results in the appropriate event.
   overriding procedure Test_Queue_Overflow (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test exercises that an invalid parameter throws the appropriate event.
   overriding procedure Test_Invalid_Parameter (Self : in out Instance);

   -- Instantiate generic component package:
   package Component_Package is new Component.Limiter (T => Tick.T, Serialized_Length => Tick.Serialized_Length);
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;

   -- Test data and state:
   type Instance is new Limiter_Tests.Base_Instance with record
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
   end record;
end Limiter_Tests.Implementation;
