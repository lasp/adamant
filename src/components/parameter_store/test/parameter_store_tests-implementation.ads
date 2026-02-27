--------------------------------------------------------------------------------
-- Parameter_Store Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Parameter Store component.
package Parameter_Store_Tests.Implementation is
   -- Test data and state:
   type Instance is new Parameter_Store_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the nominal dumping of the parameter table by command.
   overriding procedure Test_Nominal_Dump_Parameters (Self : in out Instance);
   -- This unit test tests the nominal updating of the parameter table by memory region upload.
   overriding procedure Test_Nominal_Table_Upload (Self : in out Instance);
   -- This unit test tests the nominal fetching of the parameter table into a provided memory region.
   overriding procedure Test_Nominal_Table_Fetch (Self : in out Instance);
   -- This unit test tests the behavior when updating the parameter table with a memory region of invalid length.
   overriding procedure Test_Table_Upload_Length_Error (Self : in out Instance);
   -- This unit test tests the behavior when updating the parameter table with a memory region that contains an invalid CRC.
   overriding procedure Test_Table_Upload_Crc_Error (Self : in out Instance);
   -- This unit test tests the behavior when validation of the parameter table by memory region upload fails.
   overriding procedure Test_Table_Validate_Unimplemented (Self : in out Instance);
   -- This unit test tests the behavior when fetching the parameter table with a memory region of invalid length.
   overriding procedure Test_Table_Fetch_Length_Error (Self : in out Instance);
   -- This unit test tests the no-dump-on-change configuration for the Init function and makes sure the component behaves as expected.
   overriding procedure Test_No_Dump_On_Change (Self : in out Instance);
   -- This unit test tests a command or memory region being dropped due to a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Parameter_Store_Tests.Base_Instance with record
      null;
   end record;
end Parameter_Store_Tests.Implementation;
