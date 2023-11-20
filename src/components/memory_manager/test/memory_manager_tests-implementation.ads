--------------------------------------------------------------------------------
-- Memory_Manager Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Memory Manager component
package Memory_Manager_Tests.Implementation is
   -- Test data and state:
   type Instance is new Memory_Manager_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test excersizes all possible combinations of initializing the component to make sure only valid initializations succeed, and others throw an assertion.
   overriding procedure Test_Init (Self : in out Instance);
   -- This unit test excersizes the nominal request/release of the component's memory region.
   overriding procedure Test_Nominal_Request_Release (Self : in out Instance);
   -- This unit test excersizes the error conditions that can arise when requesting/releasing the component's memory region.
   overriding procedure Test_Off_Nominal_Request_Release (Self : in out Instance);
   -- This unit test excersizes the memory dump commands.
   overriding procedure Test_Nominal_Memory_Dump (Self : in out Instance);
   -- This unit test excersizes the memory crc commands.
   overriding procedure Test_Nominal_Memory_Crc (Self : in out Instance);
   -- This unit test excersizes the memory write command.
   overriding procedure Test_Nominal_Memory_Write (Self : in out Instance);
   -- This unit test excersizes the write command when the memory is not available.
   overriding procedure Test_Write_Unreleased_Region (Self : in out Instance);
   -- This unit test excersizes the memory dump command with invalid memory regions.
   overriding procedure Test_Dump_Invalid_Region (Self : in out Instance);
   -- This unit test excersizes the crc command with invalid memory regions.
   overriding procedure Test_Crc_Invalid_Region (Self : in out Instance);
   -- This unit test excersizes the write command with invalid memory regions.
   overriding procedure Test_Write_Invalid_Region (Self : in out Instance);
   -- This unit test excersizes the force release command.
   overriding procedure Test_Force_Release_Command (Self : in out Instance);
   -- This unit test excersizes the behavior when the internal queue overflows.
   overriding procedure Test_Command_Dropped (Self : in out Instance);
   -- This unit test makes sure an invalid command is reported and ignored.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Memory_Manager_Tests.Base_Instance with record
      null;
   end record;
end Memory_Manager_Tests.Implementation;
