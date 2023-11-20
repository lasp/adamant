--------------------------------------------------------------------------------
-- Example_Component Tests Spec
--------------------------------------------------------------------------------

-- This is a set of unit tests for the Example Component.
package Example_Component_Tests.Implementation is
   -- Test data and state:
   type Instance is new Example_Component_Tests.Base_Instance with private;
private
   -- This test should pass.
   overriding procedure Test_That_Should_Pass (Self : in out Instance);
   -- This test should not pass.
   overriding procedure Test_That_Should_Fail (Self : in out Instance);

   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- Test data and state:
   type Instance is new Example_Component_Tests.Base_Instance with record
      null;
   end record;
end Example_Component_Tests.Implementation;
