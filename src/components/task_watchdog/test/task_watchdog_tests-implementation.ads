--------------------------------------------------------------------------------
-- Task_Watchdog Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Task Watchdog.
package Task_Watchdog_Tests.Implementation is
   -- Test data and state:
   type Instance is new Task_Watchdog_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test is here to make sure arrayed connectors work as expected.
   overriding procedure Test_Received_Pet (Self : in out Instance);
   -- This unit test tests the commanding to change the state of the component for checking upstream watchdog pets.
   overriding procedure Test_Watchdog_Petter_Check_Command (Self : in out Instance);
   -- This unit test tests the commanding to change the state of the pet index for checking that particular upstream watchdog pets.
   overriding procedure Test_Watchdog_Action_Command (Self : in out Instance);
   -- This unit test tests the commanding to change the limit for a particular index to a new specified value.
   overriding procedure Test_Watchdog_Limit_Command (Self : in out Instance);
   -- This unit test makes sure that an invalid command is handled gracefully.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Task_Watchdog_Tests.Base_Instance with record
      null;
   end record;
end Task_Watchdog_Tests.Implementation;
