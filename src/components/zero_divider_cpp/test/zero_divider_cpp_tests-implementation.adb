--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Tests Body
--------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Command;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command_Response.Assertion; use Command_Response.Assertion;
with Fp_Divide_By_Zero_In_Cpp_Arg;
with Int_Divide_By_Zero_In_Cpp_Arg;
with Packed_U32.Assertion; use Packed_U32.Assertion;

package body Zero_Divider_Cpp_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Magic_Number => 42, Sleep_Before_Execute_Ms => 100);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   -- This test makes sure the Int_Divide_By_Zero_In_Cpp, Fp_Divide_By_Zero_In_Cpp,
   -- and Raise_Exception_In_Cpp commands do not execute if the incorrect magic
   -- number is provided.
   overriding procedure Test_Bad_Magic_Number (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send Int_Divide_By_Zero_In_Cpp command with bad magic number:
      T.Command_T_Send (T.Commands.Int_Divide_By_Zero_In_Cpp (Int_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => 1_000, Dividend => 1)))); -- bad magic number
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Int_Divide_By_Zero_In_Cpp_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 1);
      Packed_U32_Assert.Eq (T.Invalid_Magic_Number_History.Get (1), (Value => 1_000));

      -- Send Fp_Divide_By_Zero_In_Cpp command with bad magic number:
      T.Command_T_Send (T.Commands.Fp_Divide_By_Zero_In_Cpp (Fp_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => 1_000, Dividend => 1.0)))); -- bad magic number
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Fp_Divide_By_Zero_In_Cpp_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 2);
      Packed_U32_Assert.Eq (T.Invalid_Magic_Number_History.Get (2), (Value => 1_000));

      -- Send Raise_Exception_In_Cpp command with bad magic number:
      T.Command_T_Send (T.Commands.Raise_Exception_In_Cpp ((Value => 1_000))); -- bad magic number
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Raise_Exception_In_Cpp_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 3);
      Packed_U32_Assert.Eq (T.Invalid_Magic_Number_History.Get (3), (Value => 1_000));
   end Test_Bad_Magic_Number;

   -- This test makes sure a division by zero occurs, the result is returned to Ada
   -- and a constraint error is thrown.
   overriding procedure Test_Int_Divide_By_Zero_In_Cpp (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command with good magic number:
      T.Command_T_Send (T.Commands.Int_Divide_By_Zero_In_Cpp (Int_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => 42, Dividend => 1))));
      pragma Assert (False, "Should never get here...");
   exception
      when E : others =>
         -- Verify that we caught a CONSTRAINT_ERROR
         pragma Assert (Exception_Name (E) = "CONSTRAINT_ERROR",
            "Expected Constraint_Error but got " & Exception_Information (E));
         Put_Line ("Expected exception " & Exception_Information (E));
         -- Verify no invalid magic number event was sent:
         Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 0);
         -- Verify the info event was sent before the exception:
         Natural_Assert.Eq (T.Int_Dividing_By_Zero_In_Cpp_History.Get_Count, 1);
         -- Verify the no-exception event did not fire:
         Natural_Assert.Eq (T.Int_Divide_By_Zero_No_Exception_History.Get_Count, 0);
         -- Verify only 1 event total was sent:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
   end Test_Int_Divide_By_Zero_In_Cpp;

   -- This test makes sure a floating-point division by zero occurs in C++, the
   -- result (infinity) is returned to Ada and a constraint error is thrown.
   overriding procedure Test_Fp_Divide_By_Zero_In_Cpp (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command with good magic number and a non-zero dividend:
      T.Command_T_Send (T.Commands.Fp_Divide_By_Zero_In_Cpp (Fp_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => 42, Dividend => 1.0))));
      pragma Assert (False, "Should never get here...");
   exception
      when E : others =>
         -- Verify that we caught a CONSTRAINT_ERROR (from assigning infinity to
         -- the constrained Fp_Divide_By_Zero_In_Cpp_Return_Type)
         pragma Assert (Exception_Name (E) = "CONSTRAINT_ERROR",
            "Expected Constraint_Error but got " & Exception_Information (E));
         Put_Line ("Expected exception " & Exception_Information (E));
         -- Verify no invalid magic number event was sent:
         Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 0);
         -- Verify the info event was sent before the exception:
         Natural_Assert.Eq (T.Fp_Dividing_By_Zero_In_Cpp_History.Get_Count, 1);
         -- Verify the no-exception event did not fire:
         Natural_Assert.Eq (T.Fp_Divide_By_Zero_No_Exception_History.Get_Count, 0);
         -- Verify only 1 event total was sent:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
   end Test_Fp_Divide_By_Zero_In_Cpp;

   -- This test makes sure a C++ exception is raised and propagated.
   overriding procedure Test_Raise_Exception_In_Cpp (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command with good magic number:
      T.Command_T_Send (T.Commands.Raise_Exception_In_Cpp ((Value => 42)));
      pragma Assert (False, "Should never get here...");
   exception
      when E : others =>
         -- Verify we caught a SYSTEM.EXCEPTIONS.FOREIGN_EXCEPTION
         pragma Assert (Exception_Name (E) = "SYSTEM.EXCEPTIONS.FOREIGN_EXCEPTION",
            "Expected Foreign_Exception but got " & Exception_Information (E));
         Put_Line ("Expected exception " & Exception_Information (E));
         -- Verify no invalid magic number event was sent:
         Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 0);
         -- Verify the info event was sent before the exception:
         Natural_Assert.Eq (T.Raising_Exception_In_Cpp_History.Get_Count, 1);
         -- Verify the no-exception event did not fire:
         Natural_Assert.Eq (T.Raise_Exception_In_Cpp_No_Exception_History.Get_Count, 0);
         -- Verify only 1 event total was sent:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
   end Test_Raise_Exception_In_Cpp;

   -- This test makes sure an invalid command is rejected.
   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Int_Divide_By_Zero_In_Cpp (Int_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => 42, Dividend => 1)));
   begin
      -- Make command invalid by setting an invalid field number:
      Cmd.Header.Arg_Buffer_Length := 42; -- invalid length

      -- Send bad command:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Int_Divide_By_Zero_In_Cpp_Id, Status => Length_Error));
      -- Verify the Invalid_Command_Received event was sent:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
   end Test_Invalid_Command;

end Zero_Divider_Cpp_Tests.Implementation;
