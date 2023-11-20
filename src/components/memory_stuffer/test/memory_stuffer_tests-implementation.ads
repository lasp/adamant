--------------------------------------------------------------------------------
-- Memory_Stuffer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Memory Stuffer component
package Memory_Stuffer_Tests.Implementation is
   -- Test data and state:
   type Instance is new Memory_Stuffer_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure that an invalid initialization results in a runtime assertion.
   overriding procedure Test_Invalid_Initialization (Self : in out Instance);
   -- This unit test excersizes stuffing a region that is unprotected.
   overriding procedure Test_Unprotected_Stuffing (Self : in out Instance);
   -- This unit test excersizes stuffing a region that is protected.
   overriding procedure Test_Protected_Stuffing (Self : in out Instance);
   -- This unit test excersizes all of the ways that the arm command can be invalidated prior to a write (except via timeout).
   overriding procedure Test_Arm_Unarm (Self : in out Instance);
   -- This unit test excersizes the unarming of the arm command via timeout.
   overriding procedure Test_Arm_Timeout (Self : in out Instance);
   -- This unit test excersizes writing to an invalid region of memory
   overriding procedure Test_Invalid_Address (Self : in out Instance);
   -- This unit test makes sure an invalid command is reported and ignored.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test excersizes the memory region copy and release connectors.
   overriding procedure Test_Memory_Region_Copy (Self : in out Instance);
   -- This unit test excersizes the memory region copy and release connectors with an invalid destination address.
   overriding procedure Test_Memory_Region_Copy_Invalid_Address (Self : in out Instance);

   -- Test data and state:
   type Instance is new Memory_Stuffer_Tests.Base_Instance with record
      null;
   end record;
end Memory_Stuffer_Tests.Implementation;
