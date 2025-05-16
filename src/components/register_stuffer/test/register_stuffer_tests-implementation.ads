--------------------------------------------------------------------------------
-- Register_Stuffer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Register Stuffer component
package Register_Stuffer_Tests.Implementation is

   -- Test data and state:
   type Instance is new Register_Stuffer_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure the component can write registers by command.
   overriding procedure Test_Nominal_Register_Write (Self : in out Instance);
   -- This unit test makes sure the component can read registers by command.
   overriding procedure Test_Nominal_Register_Read (Self : in out Instance);
   -- This unit test makes sure the component rejects reading or writing registers
   -- that are not 4-byte aligned.
   overriding procedure Test_Bad_Address (Self : in out Instance);
   -- This unit test makes sure an malformed command is rejected.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test makes sure the protected register write feature works as
   -- intended.
   overriding procedure Test_Protected_Register_Write (Self : in out Instance);
   -- This unit test makes sure the component can dump one register by command.
   overriding procedure Test_Nominal_Dump_One_Registers (Self : in out Instance);
   -- This unit test makes sure the component can dump the maximum number of
   -- registers by command.
   overriding procedure Test_Nominal_Dump_Max_Registers (Self : in out Instance);
   -- This unit test makes sure the component can dump four registers by command.
   overriding procedure Test_Dump_Four_Registers (Self : in out Instance);
   -- Test data and state:
   type Instance is new Register_Stuffer_Tests.Base_Instance with record
      null;
   end record;
end Register_Stuffer_Tests.Implementation;
