--------------------------------------------------------------------------------
-- Variable_Database Tests Spec
--------------------------------------------------------------------------------

with Variable_Database;
with Simple_Variable;

-- This is a unit test suite for the database data structure
package Variable_Database_Tests.Implementation is
   -- Test data and state:
   type Instance is new Variable_Database_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the database in a nominal scenario and makes sure it functions.
   overriding procedure Test_Nominal_Update_Fetch (Self : in out Instance);
   -- This unit test tests trying to access the database with a bad Id.
   overriding procedure Test_Id_Out_Of_Range (Self : in out Instance);
   -- This test tries to access data that has not yet been stored.
   overriding procedure Test_Data_Not_Available (Self : in out Instance);
   -- This test tries to store and access data that is malformed and returns a serialization failure.
   overriding procedure Test_Serialization_Failure (Self : in out Instance);
   -- This test excersizes the override feature.
   overriding procedure Test_Override (Self : in out Instance);

   -- Type definitions:
   type Id_Type is range 15 .. 30;
   package My_Database is new Variable_Database (Id_Type, Simple_Variable.T, Simple_Variable.Serialized_Length, Simple_Variable.Serialized_Length);

   -- Test data and state:
   type Instance is new Variable_Database_Tests.Base_Instance with record
      Db : My_Database.Instance;
   end record;

end Variable_Database_Tests.Implementation;
