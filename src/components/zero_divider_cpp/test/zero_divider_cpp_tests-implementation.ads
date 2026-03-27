--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Tests Spec
--------------------------------------------------------------------------------

-- This is a test suite for the Zero Divider Cpp component.
package Zero_Divider_Cpp_Tests.Implementation is

   -- Test data and state:
   type Instance is new Zero_Divider_Cpp_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test makes sure the Int_Divide_By_Zero_In_Cpp, FP_Divide_By_Zero_In_Cpp,
   -- and Raise_Exception_In_Cpp commands do not execute if the incorrect magic
   -- number is provided.
   overriding procedure Test_Bad_Magic_Number (Self : in out Instance);
   -- This test makes sure a division by zero occurs, the result is returned to Ada
   -- and a constraint error is thrown.
   overriding procedure Test_Int_Divide_By_Zero_In_Cpp (Self : in out Instance);
   -- This test makes sure a floating-point division by zero occurs in C++, the
   -- result (infinity) is returned to Ada and a constraint error is thrown.
   overriding procedure Test_Fp_Divide_By_Zero_In_Cpp (Self : in out Instance);
   -- This test makes sure a C++ exception is raised and propagated.
   overriding procedure Test_Raise_Exception_In_Cpp (Self : in out Instance);
   -- This test makes sure an invalid command is rejected.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Zero_Divider_Cpp_Tests.Base_Instance with record
      null;
   end record;
end Zero_Divider_Cpp_Tests.Implementation;
