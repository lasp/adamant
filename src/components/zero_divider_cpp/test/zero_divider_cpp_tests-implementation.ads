--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Zero Divider component.
package Zero_Divider_Cpp_Tests.Implementation is

   -- Test data and state:
   type Instance is new Zero_Divider_Cpp_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure the Divide_By_Zero command does not execute if the
   -- correct magic number is not provided.
   overriding procedure Test_Bad_Magic_Number (Self : in out Instance);
   -- This unit test makes sure a constraint error is thrown when the divide by zero
   -- command executes.
   overriding procedure Test_Divide_By_Zero_In_Cpp (Self : in out Instance);
   -- This unit test makes sure an invalid command is rejected.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Zero_Divider_Cpp_Tests.Base_Instance with record
      null;
   end record;
end Zero_Divider_Cpp_Tests.Implementation;
