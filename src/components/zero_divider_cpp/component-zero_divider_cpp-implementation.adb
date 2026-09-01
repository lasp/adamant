--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Implementation Body
--------------------------------------------------------------------------------

with Packed_U32;
with Sleep;

package body Component.Zero_Divider_Cpp.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The magic number and the time to sleep before each command executes are
   -- provided at instantiation.
   --
   -- Init Parameters:
   -- Magic_Number : Magic_Number_Type - As commands to this component crash the
   -- system, provide this number as the key value of the safety interlock mechanism
   -- that guards against unintentional execution of commands in this component.
   -- Sleep_Before_Execute_Ms : Natural - The number of milliseconds to sleep after
   -- receiving a command before executing its implementation. This allows time for
   -- any events to be written by the component, if desired.
   --
   overriding procedure Init (Self : in out Instance; Magic_Number : in Magic_Number_Type; Sleep_Before_Execute_Ms : in Natural := 1_000) is
   begin
      -- Save off Sleep and construct a zeroDividerCpp class:
      Self.Sleep_Before_Execute_Ms := Sleep_Before_Execute_Ms;
      Self.Zero_Divider_Cpp := Zerodividercpp_Create (Magic_Number);
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The command receive connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Helper subprograms:
   -----------------------------------------------

   -- Helper subprogram which returns the configured sleep as an event parameter.
   function Sleep_Duration (Self : in Instance) return Packed_U32.T is
      (Value => Interfaces.Unsigned_32 (Self.Sleep_Before_Execute_Ms));

   -- Helper subprogram which performs the staging every command in this component
   -- runs before doing its own work: reports the provided magic number and returns
   -- Failure when it does not match the one held in the C++ class, and otherwise
   -- sends the provided announcement event, sleeps for the configured time, and
   -- returns Success.
   function Stage_Command (Self : in out Instance; Magic_Number : in Packed_Magic_Number.T; Announcement : in Event.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      if Zerodividercpp_Checkmagicnumber (Self.Zero_Divider_Cpp, Magic_Number.Magic_Number) = False then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Magic_Number (Self.Sys_Time_T_Get, Magic_Number));
         return Failure;
      end if;

      Self.Event_T_Send_If_Connected (Announcement);
      Sleep.Sleep_Ms (Self.Sleep_Before_Execute_Ms);
      return Success;
   end Stage_Command;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Zero Divider Cpp component.
   -- Performs an integer division by zero in C++ and reports the outcome. Integer
   -- division by zero is undefined behavior in C++, so what happens depends on the
   -- target and the flags the project is built with. A target may trap, or may
   -- return a value that is reported in an event. This command is informational, run
   -- it to establish what a given configuration does. You must provide the correct
   -- value for the magic number and an integer dividend for this command to execute.
   overriding function Int_Divide_By_Zero_In_Cpp (Self : in out Instance; Arg : in Int_Divide_By_Zero_In_Cpp_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Stage_Status : constant Command_Execution_Status.E := Stage_Command (Self, Arg.Magic_Number, Announcement => Self.Events.Int_Dividing_By_Zero_In_Cpp (Self.Sys_Time_T_Get, Sleep_Duration (Self)));
   begin
      case Stage_Status is
         when Success =>
            -- Do the dirty, call the cpp:
            declare
               Result : constant Interfaces.Integer_32 := Zerodividercpp_Intdividebyzero (Self.Zero_Divider_Cpp, Arg.Dividend);
            begin
               -- Report the value the cpp returned. The unit tests cannot cover this
               -- send or the return below: in the test configuration the division
               -- raises Constraint_Error instead of returning (see
               -- Test_Int_Divide_By_Zero_In_Cpp).
               Self.Event_T_Send_If_Connected (Self.Events.Int_Divide_By_Zero_No_Exception (Self.Sys_Time_T_Get, (Value => Result)));
            end;
            return Success;
         when Failure =>
            return Stage_Status;
      end case;
   end Int_Divide_By_Zero_In_Cpp;

   -- Performs a floating point division by zero in C++ and reports the outcome. A
   -- target that implements IEEE 754 typically produces a signed infinity for a non-
   -- zero dividend and a NaN for a zero dividend, but whether such a value reaches
   -- Ada or raises an exception on the way depends on the target and the flags the
   -- project is built with. This command is informational, run it to establish what
   -- a given configuration does. You must provide the correct value for the magic
   -- number and a floating point dividend for this command to execute.
   overriding function Fp_Divide_By_Zero_In_Cpp (Self : in out Instance; Arg : in Fp_Divide_By_Zero_In_Cpp_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Stage_Status : constant Command_Execution_Status.E := Stage_Command (Self, Arg.Magic_Number, Announcement => Self.Events.Fp_Dividing_By_Zero_In_Cpp (Self.Sys_Time_T_Get, Sleep_Duration (Self)));
   begin
      case Stage_Status is
         when Success =>
            -- Do the dirty, call the cpp:
            declare
               Result : constant Short_Float := Zerodividercpp_Fpdividebyzero (Self.Zero_Divider_Cpp, Arg.Dividend);
            begin
               -- Report the value the cpp returned. The unit tests cannot cover this
               -- send or the return below: in the test configuration the division
               -- raises Constraint_Error instead of returning (see
               -- Test_Fp_Divide_By_Zero_In_Cpp).
               Self.Event_T_Send_If_Connected (Self.Events.Fp_Divide_By_Zero_No_Exception (Self.Sys_Time_T_Get, (Value => Result)));
            end;
            return Success;
         when Failure =>
            return Stage_Status;
      end case;
   end Fp_Divide_By_Zero_In_Cpp;

   -- Raises a standard exception in C++. You must provide the correct value for the
   -- magic number argument of this command for it to be executed.
   overriding function Raise_Exception_In_Cpp (Self : in out Instance; Arg : in Packed_Magic_Number.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      Stage_Status : constant Command_Execution_Status.E := Stage_Command (Self, Arg, Announcement => Self.Events.Raising_Exception_In_Cpp (Self.Sys_Time_T_Get, Sleep_Duration (Self)));
   begin
      case Stage_Status is
         when Success =>
            -- Do the dirty, call the cpp:
            Zerodividercpp_Raiseexception (Self.Zero_Divider_Cpp);

            -- We should never reach here: the raised C++ exception propagates to Ada
            -- rather than returning, so this send and the return below are not
            -- covered by the unit tests.
            Self.Event_T_Send_If_Connected (Self.Events.Raise_Exception_In_Cpp_No_Exception (Self.Sys_Time_T_Get));
            return Success;
         when Failure =>
            return Stage_Status;
      end case;
   end Raise_Exception_In_Cpp;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (
        Self.Sys_Time_T_Get,
        (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Zero_Divider_Cpp.Implementation;
