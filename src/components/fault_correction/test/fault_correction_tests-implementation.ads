--------------------------------------------------------------------------------
-- Fault_Correction Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Fault Correction component.
package Fault_Correction_Tests.Implementation is
   -- Test data and state:
   type Instance is new Fault_Correction_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests permutations of initializing the component and makes sure improper initialization results in a runtime assertion.
   overriding procedure Test_Initialization (Self : in out Instance);
   -- This unit test tests that fault handling is done appropriately when a fault is received.
   overriding procedure Test_Fault_Handling (Self : in out Instance);
   -- This unit test makes sure that the fault response enable and disable commands function properly.
   overriding procedure Test_Enable_Disable_Fault_Response (Self : in out Instance);
   -- This unit test makes sure that the fault response clear commands function properly.
   overriding procedure Test_Clear_Fault_Response (Self : in out Instance);
   -- This unit test tests the reset data products command.
   overriding procedure Test_Reset_Data_Products (Self : in out Instance);
   -- This unit test makes sure that commanding a change to a response with an unknown fault ID fails.
   overriding procedure Test_Unrecognized_Fault_Id (Self : in out Instance);
   -- This unit test tests that appropriate actions are taken when items are dropped off a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance);
   -- This unit test makes sure that an invalid command is handled gracefully.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Fault_Correction_Tests.Base_Instance with record
      null;
   end record;
end Fault_Correction_Tests.Implementation;
