--------------------------------------------------------------------------------
-- Sequence_Store Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Sequence Store.
package Sequence_Store_Tests.Implementation is
   -- Test data and state:
   type Instance is new Sequence_Store_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests all permutations of initializing the component and makes sure improper initialization results in a runtime assertion.
   overriding procedure Test_Initialization (Self : in out Instance);
   -- This unit test tests the dump summary packet and command.
   overriding procedure Test_Dump_Summary (Self : in out Instance);
   -- This unit test tests the slot activate/deactivate commands.
   overriding procedure Test_Nominal_Activate_Deactivate_Slot (Self : in out Instance);
   -- This unit test tests the slot activate/deactivate command failure conditions.
   overriding procedure Test_Activate_Deactivate_Slot_Fail (Self : in out Instance);
   -- This unit test tests the check slot command.
   overriding procedure Test_Check_Slot (Self : in out Instance);
   -- This unit test tests the check all slots command.
   overriding procedure Test_Check_All_Slots (Self : in out Instance);
   -- This unit test tests writing a sequence to a slot in the nominal case.
   overriding procedure Test_Nominal_Write_Sequence (Self : in out Instance);
   -- This unit test tests writing a sequence to a slot when the operation fails for different reasons.
   overriding procedure Test_Write_Sequence_Fail (Self : in out Instance);
   -- This unit test tests fetching a sequence by ID when it exists in the store, when it does not exist in the store, and when it is in the store but marked inactive.
   overriding procedure Test_Sequence_Fetch (Self : in out Instance);
   -- This unit test tests that appropriate actions are taken when items are dropped off a full queue.
   overriding procedure Test_Full_Queue (Self : in out Instance);
   -- This unit test makes sure that an invalid command is handled gracefully.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Sequence_Store_Tests.Base_Instance with record
      null;
   end record;
end Sequence_Store_Tests.Implementation;
