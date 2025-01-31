--------------------------------------------------------------------------------
-- Logger Tests Spec
--------------------------------------------------------------------------------

-- Component Tester Include:
with Component.Logger.Implementation.Tester;
with Tick_32;

-- This is a unit test suite for the Logger component
package Logger_Tests.Implementation is
   -- Test data and state:
   type Instance is new Logger_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the storing of log data and subsequent dumping by command when the log is enabled.
   overriding procedure Test_Log_And_Dump_Enabled (Self : in out Instance);
   -- This unit test tests the storing of log data and subsequent dumping by command when the log is disabled.
   overriding procedure Test_Log_And_Dump_Disabled (Self : in out Instance);
   -- This unit test tests the storing of a lot of log data, such that the circular buffer overwrites, and subsequent dumping by command.
   overriding procedure Test_Log_Overwrite_And_Dump (Self : in out Instance);
   -- This unit test tests the enabled/disable commands to the logger to make sure they behave as expected.
   overriding procedure Test_Enable_Disable (Self : in out Instance);
   -- This unit test tests initializing the log with both valid and invalid values.
   overriding procedure Test_Init (Self : in out Instance);
   -- This unit test makes sure an invalid command is reported and ignored.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Instantiate generic component:
   package Component_Package is new Component.Logger (T => Tick_32.T, Serialized_Length => Tick_32.Serialized_Length);
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;

   -- Test data and state:
   type Instance is new Logger_Tests.Base_Instance with record
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
   end record;
end Logger_Tests.Implementation;
