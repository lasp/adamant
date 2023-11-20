--------------------------------------------------------------------------------
-- Memory_Dumper Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Memory Dumper component
package Memory_Dumper_Tests.Implementation is
   -- Test data and state:
   type Instance is new Memory_Dumper_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test excersizes dumping a valid region of memory managed by the component
   overriding procedure Test_Nominal_Dumping (Self : in out Instance);
   -- This unit test excersizes CRCing from a valid region of memory
   overriding procedure Test_Memory_Crc (Self : in out Instance);
   -- This unit test excersizes dumping and CRCing from an invalid region of memory
   overriding procedure Test_Invalid_Address (Self : in out Instance);
   -- This unit test makes sure an invalid command is reported and ignored.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Memory_Dumper_Tests.Base_Instance with record
      null;
   end record;
end Memory_Dumper_Tests.Implementation;
