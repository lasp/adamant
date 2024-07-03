--------------------------------------------------------------------------------
-- Zero_Divider Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces;

package body Zero_Divider_Tests.Implementation is

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
      Self.Tester.Component_Instance.Init (Magic_Number => 1_297, Sleep_Before_Divide_Ms => 100);

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

   overriding procedure Test_Bad_Magic_Number (Self : in out Instance) is
      T : Component.Zero_Divider.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command with bad magic number:
      T.Command_T_Send (T.Commands.Divide_By_Zero ((Value => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Divide_By_Zero_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 1);
      Packed_U32_Assert.Eq (T.Invalid_Magic_Number_History.Get (1), (Value => 3));

      -- Send command with bad magic number:
      T.Command_T_Send (T.Commands.Divide_By_Zero ((Value => 999)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Divide_By_Zero_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 2);
      Packed_U32_Assert.Eq (T.Invalid_Magic_Number_History.Get (2), (Value => 999));
   end Test_Bad_Magic_Number;

   overriding procedure Test_Divide_By_Zero (Self : in out Instance) is
      T : Component.Zero_Divider.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command with good magic number:
      T.Command_T_Send (T.Commands.Divide_By_Zero ((Value => 1_297)));
      pragma Assert (False, "Should never get here...");
   exception
      when Constraint_Error =>
         Put_Line ("Constraint_Error thrown! Yay!");
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 0);
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
         Natural_Assert.Eq (T.Dividing_By_Zero_History.Get_Count, 1);
         Natural_Assert.Eq (T.Dividing_By_Zero_History.Get (1).Value, 100);
   end Test_Divide_By_Zero;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Zero_Divider.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Divide_By_Zero ((Value => 3));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Divide_By_Zero_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Divide_By_Zero_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Zero_Divider_Tests.Implementation;
