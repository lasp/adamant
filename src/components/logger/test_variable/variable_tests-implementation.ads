--------------------------------------------------------------------------------
-- Logger Tests Spec
--------------------------------------------------------------------------------

-- Component Tester Include:
with Component.Logger.Implementation.Tester;
with Simple_Variable;

-- This is a unit test suite for the Logger component which logs a variable sized log type onto a log instantiated statically, not on the heap.
package Variable_Tests.Implementation is
   -- Test data and state:
   type Instance is new Variable_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the storing of variable length log data and subsequent dumping by command.
   overriding procedure Test_Log_And_Dump (Self : in out Instance);
   -- This unit test tests the behavior when the logger receives a poorly formatted variable type.
   overriding procedure Test_Logger_Error (Self : in out Instance);
   -- This unit test makes sure an invalid command is reported and ignored.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Instantiate generic component package:
   package Component_Package is new Component.Logger (T => Simple_Variable.T, Serialized_Length => Simple_Variable.Serialized_Length);
   package Component_Implementation_Package is new Component_Package.Implementation;
   package Component_Tester_Package is new Component_Implementation_Package.Tester;

   -- Test data and state:
   type Instance is new Variable_Tests.Base_Instance with record
      -- The tester component:
      Tester : Component_Tester_Package.Instance_Access;
   end record;
end Variable_Tests.Implementation;
