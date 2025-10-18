--------------------------------------------------------------------------------
-- Parameters Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Parameters component specifically testing
-- grouped parameters functionality.
package Parameters_Grouped_Tests.Implementation is

   -- Test data and state:
   type Instance is new Parameters_Grouped_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the nominal dumping of grouped parameters by command.
   overriding procedure Test_Grouped_Dump_Parameters (Self : in out Instance);
   -- This unit test tests the nominal updating of a grouped parameter by command.
   overriding procedure Test_Grouped_Update_Parameters (Self : in out Instance);
   -- This unit test tests the nominal updating of grouped parameters by memory
   -- region upload.
   overriding procedure Test_Grouped_Table_Upload (Self : in out Instance);
   -- This unit test tests the nominal validation of grouped parameters by memory
   -- region upload.
   overriding procedure Test_Grouped_Table_Validate (Self : in out Instance);
   -- This unit test tests the nominal fetching of grouped parameters into a provided
   -- memory region.
   overriding procedure Test_Grouped_Table_Fetch (Self : in out Instance);
   -- This unit test tests error handling when dumping grouped parameters fails.
   overriding procedure Test_Grouped_Dump_Parameters_Error (Self : in out Instance);
   -- This unit test tests error handling when updating grouped parameters by command
   -- fails.
   overriding procedure Test_Grouped_Update_Parameters_Error (Self : in out Instance);
   -- This unit test tests error handling when updating grouped parameters by memory
   -- region upload fails.
   overriding procedure Test_Grouped_Table_Upload_Error (Self : in out Instance);
   -- This unit test tests that the Parameter_Fetch_Value_Mismatch event is produced
   -- when grouped parameters have diverged values during a fetch operation.
   overriding procedure Test_Grouped_Fetch_Value_Mismatch (Self : in out Instance);

   -- Test data and state:
   type Instance is new Parameters_Grouped_Tests.Base_Instance with record
      null;
   end record;
end Parameters_Grouped_Tests.Implementation;
