--------------------------------------------------------------------------------
-- Product_Database Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Product Database Component
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the updating and fetching of data products with no errors.
   overriding procedure Test_Nominal_Scenario (Self : in out Instance);
   -- This unit test tests the overriding and fetching of data products with no errors.
   overriding procedure Test_Nominal_Override (Self : in out Instance);
   -- This unit test tests the dumping of a data product packet with no errors.
   overriding procedure Test_Nominal_Dump (Self : in out Instance);
   -- This unit test tests the sending of a data product poly type.
   overriding procedure Test_Nominal_Dump_Poly (Self : in out Instance);
   -- This unit test tests the fetching of data that has not yet been stored.
   overriding procedure Test_Data_Not_Available (Self : in out Instance);
   -- This test tries to update and fetch data that has a bad id.
   overriding procedure Test_Id_Out_Of_Range (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
