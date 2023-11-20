--------------------------------------------------------------------------------
-- Command_Rejector Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Command Rejector.
package Command_Rejector_Tests.Implementation is
   -- Test data and state:
   type Instance is new Command_Rejector_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests all permutations of initializing the component and makes sure improper initialization results in a runtime assertion.
   overriding procedure Test_Initialization (Self : in out Instance);
   -- This unit test tests that a command not on the reject list is passed along.
   overriding procedure Test_Command_Accept (Self : in out Instance);
   -- This unit test tests that a command on the reject list is dropped by the component.
   overriding procedure Test_Command_Reject (Self : in out Instance);

   -- Test data and state:
   type Instance is new Command_Rejector_Tests.Base_Instance with record
      null;
   end record;
end Command_Rejector_Tests.Implementation;
