--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Tests Body
--------------------------------------------------------------------------------

with Zero_Divider_Test_Config;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Basic_Types;
with Command;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Types;
with Fp_Divide_By_Zero_In_Cpp_Arg;
with Int_Divide_By_Zero_In_Cpp_Arg;
with Packed_Magic_Number.Assertion; use Packed_Magic_Number.Assertion;

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
   -- and Raise_Exception_In_Cpp commands do not execute if an incorrect but
   -- representable magic number is provided.
   overriding procedure Test_Bad_Magic_Number (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send Int_Divide_By_Zero_In_Cpp command with bad magic number:
      T.Command_T_Send (T.Commands.Int_Divide_By_Zero_In_Cpp (Int_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => (Magic_Number => 1_000), Dividend => 1)))); -- bad magic number
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Int_Divide_By_Zero_In_Cpp_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 1);
      Packed_Magic_Number_Assert.Eq (T.Invalid_Magic_Number_History.Get (1), (Magic_Number => 1_000));

      -- Send Fp_Divide_By_Zero_In_Cpp command with bad magic number:
      T.Command_T_Send (T.Commands.Fp_Divide_By_Zero_In_Cpp (Fp_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => (Magic_Number => 1_000), Dividend => 1.0)))); -- bad magic number
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Fp_Divide_By_Zero_In_Cpp_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 2);
      Packed_Magic_Number_Assert.Eq (T.Invalid_Magic_Number_History.Get (2), (Magic_Number => 1_000));

      -- Send Raise_Exception_In_Cpp command with bad magic number:
      T.Command_T_Send (T.Commands.Raise_Exception_In_Cpp ((Magic_Number => 1_000))); -- bad magic number
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Raise_Exception_In_Cpp_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 3);
      Packed_Magic_Number_Assert.Eq (T.Invalid_Magic_Number_History.Get (3), (Magic_Number => 1_000));
   end Test_Bad_Magic_Number;

   -- This test makes sure that each of the Int_Divide_By_Zero_In_Cpp,
   -- Fp_Divide_By_Zero_In_Cpp, and Raise_Exception_In_Cpp commands is rejected when
   -- it carries a magic number of 0 or 1, the two values the magic number type
   -- excludes. Such a command is caught by command validation and reported as an
   -- invalid command, so it never reaches a command handler and never reports an
   -- invalid magic number.
   overriding procedure Test_Out_Of_Range_Magic_Number (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
      Sent : Natural := 0;

      -- Commands built with a representable magic number, to be corrupted below:
      Int_Cmd : constant Command.T := T.Commands.Int_Divide_By_Zero_In_Cpp (Int_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => (Magic_Number => 42), Dividend => 1)));
      Fp_Cmd : constant Command.T := T.Commands.Fp_Divide_By_Zero_In_Cpp (Fp_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => (Magic_Number => 42), Dividend => 1.0)));
      Raise_Cmd : constant Command.T := T.Commands.Raise_Exception_In_Cpp ((Magic_Number => 42));

      -- The magic number is the first field of every one of this component's command
      -- arguments, so overwriting the first four argument bytes sets it to a value
      -- that Pack cannot be asked to produce:
      procedure Send_With_Magic_Number (Cmd : in Command.T; Magic_Number : in Basic_Types.Byte; Id : in Command_Types.Command_Id) is
         Bad_Cmd : Command.T := Cmd;
      begin
         Bad_Cmd.Arg_Buffer (0 .. 3) := [0, 0, 0, Magic_Number];
         T.Command_T_Send (Bad_Cmd);
         Sent := @ + 1;

         -- The command is rejected by validation rather than by a handler:
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, Sent);
         Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (Sent), (Source_Id => 0, Registration_Id => 0, Command_Id => Id, Status => Validation_Error));
         Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, Sent);
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, Sent);
      end Send_With_Magic_Number;
   begin
      -- 0 and 1 are the two values the magic number type excludes:
      for Magic_Number in Basic_Types.Byte range 0 .. 1 loop
         Send_With_Magic_Number (Int_Cmd, Magic_Number, T.Commands.Get_Int_Divide_By_Zero_In_Cpp_Id);
         Send_With_Magic_Number (Fp_Cmd, Magic_Number, T.Commands.Get_Fp_Divide_By_Zero_In_Cpp_Id);
         Send_With_Magic_Number (Raise_Cmd, Magic_Number, T.Commands.Get_Raise_Exception_In_Cpp_Id);
      end loop;

      -- No command reached a handler, so no magic number event was sent:
      Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 0);
   end Test_Out_Of_Range_Magic_Number;

   -- This test records how the integer division by zero behaves in the configuration
   -- the unit tests are built and run in, which is the Linux_Test target on x86-64
   -- with GNAT numeric overflow checking (-gnato), assertions (-gnata) and full
   -- validity checking (-gnatVa) enabled. In that configuration the processor traps
   -- on the division and the GNAT Linux runtime signal manager raises a
   -- Constraint_Error, so the command never returns to report a value. The assertion
   -- below characterizes that configuration only. Another target or flag set may
   -- return a value instead, which the command reports in an event.
   overriding procedure Test_Int_Divide_By_Zero_In_Cpp (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
      Exception_Was_Raised : Boolean := False;
   begin
      begin
         -- Send command with good magic number:
         T.Command_T_Send (T.Commands.Int_Divide_By_Zero_In_Cpp (Int_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => (Magic_Number => 42), Dividend => 1))));
      exception
         when E : others =>
            Exception_Was_Raised := True;
            -- Verify that we caught a CONSTRAINT_ERROR:
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
      end;

      if Zero_Divider_Test_Config.Integer_Division_Traps then
         pragma Assert (Exception_Was_Raised, "Command returned without raising an exception.");
      else
         -- Where the division returns a value instead of trapping, the command
         -- runs to completion and reports the value it got back. This covers
         -- the send and the return that a trapping target cannot reach.
         pragma Assert (not Exception_Was_Raised, "Command raised an exception on a target where division does not trap.");
         -- Verify no invalid magic number event was sent:
         Natural_Assert.Eq (T.Invalid_Magic_Number_History.Get_Count, 0);
         -- Verify the info event was sent:
         Natural_Assert.Eq (T.Int_Dividing_By_Zero_In_Cpp_History.Get_Count, 1);
         -- Verify the no-exception event reported the returned value:
         Natural_Assert.Eq (T.Int_Divide_By_Zero_No_Exception_History.Get_Count, 1);
         -- Verify only those 2 events were sent:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      end if;
   end Test_Int_Divide_By_Zero_In_Cpp;

   -- This test records how the floating point division by zero behaves in the
   -- configuration the unit tests are built and run in, which is the Linux_Test
   -- target on x86-64 with GNAT numeric overflow checking (-gnato), assertions
   -- (-gnata) and full validity checking (-gnatVa) enabled. In that configuration
   -- C++ returns an infinity and the validity check on the Ada Short_Float result
   -- rejects it as invalid data, raising a Constraint_Error, so the command never
   -- returns to report a value. The assertion below characterizes that configuration
   -- only. Another target or flag set may deliver the infinity to Ada intact, which
   -- the command reports in an event.
   overriding procedure Test_Fp_Divide_By_Zero_In_Cpp (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
      Exception_Was_Raised : Boolean := False;
   begin
      begin
         -- Send command with good magic number and a non-zero dividend:
         T.Command_T_Send (T.Commands.Fp_Divide_By_Zero_In_Cpp (Fp_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => (Magic_Number => 42), Dividend => 1.0))));
      exception
         when E : others =>
            Exception_Was_Raised := True;
            -- Verify that we caught a CONSTRAINT_ERROR:
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
      end;

      pragma Assert (Exception_Was_Raised, "Command returned without raising an exception.");
   end Test_Fp_Divide_By_Zero_In_Cpp;

   -- This test makes sure a C++ exception is raised and propagated.
   overriding procedure Test_Raise_Exception_In_Cpp (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
      Exception_Was_Raised : Boolean := False;
   begin
      begin
         -- Send command with good magic number:
         T.Command_T_Send (T.Commands.Raise_Exception_In_Cpp ((Magic_Number => 42)));
      exception
         when E : others =>
            Exception_Was_Raised := True;
            -- Verify we caught a SYSTEM.EXCEPTIONS.FOREIGN_EXCEPTION:
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
      end;

      pragma Assert (Exception_Was_Raised, "Command returned without raising an exception.");
   end Test_Raise_Exception_In_Cpp;

   -- This test makes sure an invalid command is rejected.
   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Zero_Divider_Cpp.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Int_Divide_By_Zero_In_Cpp (Int_Divide_By_Zero_In_Cpp_Arg.Pack ((Magic_Number => (Magic_Number => 42), Dividend => 1)));
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
