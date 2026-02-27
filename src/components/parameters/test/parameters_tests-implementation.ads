--------------------------------------------------------------------------------
-- Parameters Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Parameters component.
package Parameters_Tests.Implementation is
   -- Test data and state:
   type Instance is new Parameters_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure invalid initializations result in proper assertions.
   overriding procedure Test_Init (Self : in out Instance);
   -- This unit test tests the nominal dumping of the parameter table by command.
   overriding procedure Test_Nominal_Dump_Parameters (Self : in out Instance);
   -- This unit test tests the nominal updating of the parameter table by command.
   overriding procedure Test_Nominal_Update_Parameters (Self : in out Instance);
   -- This unit test tests the nominal updating of the parameter table by memory region upload.
   overriding procedure Test_Nominal_Table_Upload (Self : in out Instance);
   -- This unit test tests the nominal validation of the parameter table by memory region upload.
   overriding procedure Test_Nominal_Table_Validate (Self : in out Instance);
   -- This unit test tests the nominal fetching of the parameter table into a provided memory region.
   overriding procedure Test_Nominal_Table_Fetch (Self : in out Instance);
   -- This unit test tests the behavior when dumping the parameter table fails.
   overriding procedure Test_Dump_Parameters_Error (Self : in out Instance);
   -- This unit test tests the behavior when updating of the parameter table fails.
   overriding procedure Test_Update_Parameters_Error (Self : in out Instance);
   -- This unit test tests the behavior when updating of the parameter table by memory region upload fails.
   overriding procedure Test_Table_Upload_Error (Self : in out Instance);
   -- This unit test tests the behavior when validation of the parameter table by memory region upload fails.
   overriding procedure Test_Table_Validate_Error (Self : in out Instance);
   -- This unit test tests the behavior when fetching of the parameter table into a memory region fails.
   overriding procedure Test_Table_Fetch_Error (Self : in out Instance);
   -- This unit test tests the no-dump-on-change configuration for the Init function and makes sure the component behaves as expected.
   overriding procedure Test_No_Dump_On_Change (Self : in out Instance);
   -- This unit test tests a command or memory region being dropped due to a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Parameters_Tests.Base_Instance with record
      null;
   end record;
end Parameters_Tests.Implementation;
