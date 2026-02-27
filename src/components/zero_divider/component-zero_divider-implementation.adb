--------------------------------------------------------------------------------
-- Zero_Divider Component Implementation Body
--------------------------------------------------------------------------------

with Sleep;

package body Component.Zero_Divider.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The magic number is provided at instantiation.
   --
   -- Init Parameters:
   -- Magic_Number : Magic_Number_Type - Pick a number that must be provided with the Divide_By_Zero command for it to be executed. If any other number is provided, the command is failed and no divide by zero instruction is executed. Note - The values of 0 and 1 are not accepted as magic numbers.
   -- Sleep_Before_Divide_Ms : Natural - The number of milliseconds to sleep after receiving the command but before performing the divide by zero. This allows time for any events to be written by the component, if desired.
   --
   overriding procedure Init (Self : in out Instance; Magic_Number : in Magic_Number_Type; Sleep_Before_Divide_Ms : in Natural := 1_000) is
   begin
      -- Save off internal state:
      Self.Magic_Number := Magic_Number;
      Self.Sleep_Before_Divide_Ms := Sleep_Before_Divide_Ms;
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
   --    Commands for the Zero Divider component.
   -- You must provide the correct magic number as argument to this command for it to be executed.
   overriding function Divide_By_Zero (Self : in out Instance; Arg : in Packed_U32.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- See if the provided argument matches the magic number. If it doesn't then don't execute the command.
      if Arg.Value /= Self.Magic_Number then
         Self.Event_T_Send_If_Connected (Self.Events.Invalid_Magic_Number (Self.Sys_Time_T_Get, Arg));
         return Failure;
      else
         -- Send info event:
         Self.Event_T_Send_If_Connected (Self.Events.Dividing_By_Zero (Self.Sys_Time_T_Get, (Value => Self.Sleep_Before_Divide_Ms)));

         -- Sleep for a bit:
         Sleep.Sleep_Ms (Self.Sleep_Before_Divide_Ms);

         -- Do the dirty:
         declare
            Infinity : constant Natural := 1_000 / Self.Zero;
         begin
            -- This will never execute.
            Self.Event_T_Send_If_Connected (Self.Events.Dividing_By_Zero (Self.Sys_Time_T_Get, (Value => Infinity)));
         end;
      end if;

      return Success;
   end Divide_By_Zero;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

end Component.Zero_Divider.Implementation;
