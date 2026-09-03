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

   -- This test makes sure the Int_Divide_By_Zero_In_Cpp, Fp_Divide_By_Zero_In_Cpp,
   -- and Raise_Exception_In_Cpp commands do not execute if an incorrect but
   -- representable magic number is provided.
   overriding procedure Test_Bad_Magic_Number (Self : in out Instance);
   -- This test makes sure that each of the Int_Divide_By_Zero_In_Cpp,
   -- Fp_Divide_By_Zero_In_Cpp, and Raise_Exception_In_Cpp commands is rejected when
   -- it carries a magic number of 0 or 1, the two values the magic number type
   -- excludes. Such a command is caught by command validation and reported as an
   -- invalid command, so it never reaches a command handler and never reports an
   -- invalid magic number.
   overriding procedure Test_Out_Of_Range_Magic_Number (Self : in out Instance);
   -- This test records how the integer division by zero behaves in the configuration
   -- the unit tests are built and run in, which is the Linux_Test target on x86-64
   -- with GNAT numeric overflow checking (-gnato), assertions (-gnata) and full
   -- validity checking (-gnatVa) enabled. In that configuration the processor traps
   -- on the division and the GNAT Linux runtime signal manager raises a
   -- Constraint_Error, so the command never returns to report a value. The assertion
   -- below characterizes that configuration only. Another target or flag set may
   -- return a value instead, which the command reports in an event.
   overriding procedure Test_Int_Divide_By_Zero_In_Cpp (Self : in out Instance);
   -- This test records how the floating point division by zero behaves in the
   -- configuration the unit tests are built and run in, which is the Linux_Test
   -- target on x86-64 with GNAT numeric overflow checking (-gnato), assertions
   -- (-gnata) and full validity checking (-gnatVa) enabled. In that configuration
   -- C++ returns an infinity and the validity check on the Ada Short_Float result
   -- rejects it as invalid data, raising a Constraint_Error, so the command never
   -- returns to report a value. The assertion below characterizes that configuration
   -- only. Another target or flag set may deliver the infinity to Ada intact, which
   -- the command reports in an event.
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
