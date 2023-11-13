--------------------------------------------------------------------------------
-- Oo_Package Tests Spec
--------------------------------------------------------------------------------

with Oo_Package;

-- This is a set of unit tests for the Oo_Package package.
package Oo_Package_Tests.Implementation is
   -- Test data and state:
   type Instance is new Oo_Package_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test makes sure the initialization function works as expected.
   overriding procedure Test_Init (Self : in out Instance);
   -- This test makes sure the addition function works as expected.
   overriding procedure Test_Add (Self : in out Instance);

   -- Test data and state:
   type Instance is new Oo_Package_Tests.Base_Instance with record
      Oo : Oo_Package.Instance;
   end record;
end Oo_Package_Tests.Implementation;
