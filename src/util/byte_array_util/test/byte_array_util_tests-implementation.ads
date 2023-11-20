--------------------------------------------------------------------------------
-- Byte_Array_Util Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the byte array util package.
package Byte_Array_Util_Tests.Implementation is
   -- Test data and state:
   type Instance is new Byte_Array_Util_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test makes sure the extract polytype function works as expected.
   overriding procedure Test_Extract (Self : in out Instance);
   -- This test makes sure the set polytype function works as expected.
   overriding procedure Test_Set (Self : in out Instance);
   -- This test makes sure the set and extract polytype functions work together as expected.
   overriding procedure Test_Set_And_Extract (Self : in out Instance);
   -- This test makes sure the extract polytype function works for signed numbers that are negative.
   overriding procedure Test_Extract_Signed (Self : in out Instance);

   -- Test data and state:
   type Instance is new Byte_Array_Util_Tests.Base_Instance with record
      null;
   end record;
end Byte_Array_Util_Tests.Implementation;
