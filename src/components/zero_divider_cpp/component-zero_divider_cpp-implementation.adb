--------------------------------------------------------------------------------
-- Zero_Divider_Cpp Component Implementation Body
--------------------------------------------------------------------------------

with Sleep;

package body Component.Zero_Divider_Cpp.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The magic number is provided at instantiation.
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
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Zero Divider Cpp component.
   -- Performs an integer division by zero in C++. You must provide the correct value
   -- for the magic number and an integer dividend for this command to execute.
   overriding function Int_Divide_By_Zero_In_Cpp (Self : in out Instance; Arg : in Int_Divide_By_Zero_In_Cpp_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      -- Unpack the argument to access the fields:
      Unpacked_Arg : constant Int_Divide_By_Zero_In_Cpp_Arg.U := Int_Divide_By_Zero_In_Cpp_Arg.Unpack (Arg);
   begin
      -- See if the provided argument matches the magic number. If it doesn't then don't execute the command.
      if Zerodividercpp_Checkmagicnumber (Self.Zero_Divider_Cpp, Unpacked_Arg.Magic_Number) = False then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Magic_Number (Self.Sys_Time_T_Get, (Value => Unpacked_Arg.Magic_Number)));
         return Failure;
      else
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Int_Dividing_By_Zero_In_Cpp (Self.Sys_Time_T_Get, (Value => Interfaces.Unsigned_32 (Self.Sleep_Before_Execute_Ms))));

         -- Sleep for a bit:
         Sleep.Sleep_Ms (Self.Sleep_Before_Execute_Ms);

         -- Do the dirty, call the cpp:
         declare
            -- Integer divide-by-zero is undefined behavior in C++. The result varies by
            -- compiler, optimization level, and platform. Many targets return -1 (NaN)
            -- rather than raising an exception. We explicitly check for this value and
            -- raise a Constraint_Error to propagate to the Last Chance Handler.
            Result : constant Interfaces.Integer_32 := Zerodividercpp_Intdividebyzero (Self.Zero_Divider_Cpp, Unpacked_Arg.Dividend);
         begin
            if Result = -1 then
               raise Constraint_Error with "Integer divide-by-zero in C++ returned NaN (-1)";
            end if;
            -- We should never reach here:
            Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Int_Divide_By_Zero_Result (Self.Sys_Time_T_Get, (Value => Result)));
         end;
      end if;

      return Success;
   end Int_Divide_By_Zero_In_Cpp;

   -- Performs a floating-point division by zero in C++. You must provide the correct
   -- value for the magic number and a floating-point dividend for this command to
   -- execute.
   overriding function Fp_Divide_By_Zero_In_Cpp (Self : in out Instance; Arg : in Fp_Divide_By_Zero_In_Cpp_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      -- Unpack the argument to access the fields:
      Unpacked_Arg : constant Fp_Divide_By_Zero_In_Cpp_Arg.U := Fp_Divide_By_Zero_In_Cpp_Arg.Unpack (Arg);
   begin
      -- See if the provided argument matches the magic number. If it doesn't then don't execute the command.
      if Zerodividercpp_Checkmagicnumber (Self.Zero_Divider_Cpp, Unpacked_Arg.Magic_Number) = False then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Magic_Number (Self.Sys_Time_T_Get, (Value => Unpacked_Arg.Magic_Number)));
         return Failure;
      else
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Fp_Dividing_By_Zero_In_Cpp (Self.Sys_Time_T_Get, (Value => Interfaces.Unsigned_32 (Self.Sleep_Before_Execute_Ms))));

         -- Sleep for a bit:
         Sleep.Sleep_Ms (Self.Sleep_Before_Execute_Ms);

         -- Do the dirty, call the cpp:
         declare
            -- Per the C++ reference, if both operands have a floating-point type
            -- and the type supports IEEE floating-point arithmetic (std::numeric_limits::is_iec559):
            --   - If one operand is NaN, the result is NaN.
            --   - Dividing a non-zero number by +/-0.0 gives the correctly-signed
            --     infinity and FE_DIVBYZERO is raised.
            --   - Dividing 0.0 by 0.0 gives NaN and FE_INVALID is raised.
            -- To detect these conditions, we assign the C++ result to an Ada constrained
            -- float subtype that excludes infinities and NaN, triggering a
            -- Constraint_Error routed to the Last Chance Handler.
            Result : constant Short_Float := Zerodividercpp_Fpdividebyzero (Self.Zero_Divider_Cpp, Unpacked_Arg.Dividend);
         begin
            -- We should never reach here:
            Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Fp_Divide_By_Zero_Result (Self.Sys_Time_T_Get, (Value => Result)));
         end;
      end if;

      return Success;
   end Fp_Divide_By_Zero_In_Cpp;

   -- Raises a standard exception in C++. You must provide the correct value for the
   -- magic number argument of this command for it to be executed.
   overriding function Raise_Exception_In_Cpp (Self : in out Instance; Arg : in Packed_U32.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- See if the provided argument matches the magic number. If it doesn't then don't execute the command.
      if Zerodividercpp_Checkmagicnumber (Self.Zero_Divider_Cpp, Arg.Value) = False then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Magic_Number (Self.Sys_Time_T_Get, Arg));
         return Failure;
      else
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Raising_Exception_In_Cpp (Self.Sys_Time_T_Get, (Value => Interfaces.Unsigned_32 (Self.Sleep_Before_Execute_Ms))));

         -- Sleep for a bit:
         Sleep.Sleep_Ms (Self.Sleep_Before_Execute_Ms);

         -- Do the dirty, call the cpp:
         Zerodividercpp_Raiseexception (Self.Zero_Divider_Cpp);

         -- We should never reach here:
         Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Raising_Exception_In_Cpp_Result (Self.Sys_Time_T_Get));
      end if;
      return Success;
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
