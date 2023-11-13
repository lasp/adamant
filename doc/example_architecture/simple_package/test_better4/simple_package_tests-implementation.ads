--------------------------------------------------------------------------------
-- Simple_Package Tests Spec
--------------------------------------------------------------------------------

-- This is a set of unit tests for the Simple_Package package.
package Simple_Package_Tests.Implementation is
   -- Test data and state:
   type Instance is new Simple_Package_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test should pass.
   overriding procedure Test_That_Should_Pass (Self : in out Instance);
   -- This test should not pass.
   overriding procedure Test_That_Should_Fail (Self : in out Instance);

   -- Test data and state:
   type Instance is new Simple_Package_Tests.Base_Instance with record
      null;
   end record;
end Simple_Package_Tests.Implementation;
