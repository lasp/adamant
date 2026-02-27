--------------------------------------------------------------------------------
-- Database Tests Spec
--------------------------------------------------------------------------------

with Database;

-- This is a unit test suite for the database data structure
package Database_Tests.Implementation is
   -- Test data and state:
   type Instance is new Database_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the database in a nominal scenario and makes sure it functions.
   overriding procedure Test_Nominal_Update_Fetch (Self : in out Instance);
   -- This unit test tests trying to access the database with a bad Id.
   overriding procedure Test_Id_Out_Of_Range (Self : in out Instance);
   -- This unit test tries to add too many items to the database.
   overriding procedure Test_Not_Enough_Memory (Self : in out Instance);
   -- This test tries to access data that has not yet been stored.
   overriding procedure Test_Data_Not_Available (Self : in out Instance);

   -- Type definitions:
   type Id_Type is range 15 .. 30;
   type Id_Range is range 17 .. 29;
   package My_Database is new Database (Id_Type, Natural);

   -- Test data and state:
   type Instance is new Database_Tests.Base_Instance with record
      Db : My_Database.Instance;
   end record;
end Database_Tests.Implementation;
