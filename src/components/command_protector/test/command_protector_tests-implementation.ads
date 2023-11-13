--------------------------------------------------------------------------------
-- Command_Protector Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Command Protector.
package Command_Protector_Tests.Implementation is
   -- Test data and state:
   type Instance is new Command_Protector_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests all permutations of initializing the component and makes sure improper initialization results in a runtime assertion.
   overriding procedure Test_Initialization (Self : in out Instance);
   -- This unit test tests that an unprotected command is passed along no matter the state of the component.
   overriding procedure Test_Unprotected_Command_Accept (Self : in out Instance);
   -- This unit test tests that a protected command is accepted if the component is armed.
   overriding procedure Test_Protected_Command_Accept (Self : in out Instance);
   -- This unit test tests that a protected command is rejected if the component is unarmed.
   overriding procedure Test_Protected_Command_Reject (Self : in out Instance);
   -- This unit test tests that a protected command is rejected if the component is unarmed due to timeout.
   overriding procedure Test_Protected_Command_Reject_Timeout (Self : in out Instance);
   -- This unit test makes sure that an invalid command is handled gracefully.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Command_Protector_Tests.Base_Instance with record
      null;
   end record;
end Command_Protector_Tests.Implementation;
