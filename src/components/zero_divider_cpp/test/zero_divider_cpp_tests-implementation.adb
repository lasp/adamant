--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;

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
      Self.Tester.Component_Instance.Init (Magic_Number => 1_000, Sleep_Before_Divide_Ms => 100);

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

   -- This unit test makes sure the Divide_By_Zero command does not execute if the
   -- correct magic number is not provided.
   overriding procedure Test_Bad_Magic_Number (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command with bad magic number:
      T.Command_T_Send (T.Commands.Divide_By_Zero_In_Cpp ((Value => 42))); -- bad magic number
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Divide_By_Zero_In_Cpp_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 1);
      Packed_U32_Assert.Eq (T.Invalid_Magic_Number_History.Get (1), (Value => 42));
   end Test_Bad_Magic_Number;

   -- This unit test makes sure a constraint error is thrown when the divide by zero
   -- command executes.
   overriding procedure Test_Divide_By_Zero_In_Cpp (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command with good magic number:
      T.Command_T_Send (T.Commands.Divide_By_Zero_In_Cpp ((Value => 1_000)));
      pragma Assert (False, "Should never get here...");
   exception
      when others =>
         declare
         begin
            Put_Line ("An exception was thrown!");
         end;
   end Test_Divide_By_Zero_In_Cpp;

   -- This unit test makes sure an invalid command is rejected.
   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Divide_By_Zero_In_Cpp ((Value => 42)); -- bad magic number
   begin
      -- Make command invalid by setting an invalid field number:
      Cmd.Header.Arg_Buffer_Length := 42; -- invalid length

      -- Send bad command:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Divide_By_Zero_In_Cpp_Id, Status => Length_Error));
   end Test_Invalid_Command;

end Zero_Divider_Cpp_Tests.Implementation;
