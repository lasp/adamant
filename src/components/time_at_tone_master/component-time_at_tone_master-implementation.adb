--------------------------------------------------------------------------------
-- Time_At_Tone_Master Component Implementation Body
--------------------------------------------------------------------------------

with Sys_Time.Arithmetic;

package body Component.Time_At_Tone_Master.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Wait_Time_Ms : Natural - Number of milliseconds the master waits between the sending of the time message and the sending of the tone message. This is implemented internally by an Ada 'delay until' statement.
   -- Sync_Period : Positive - The number of ticks between sending clock sync messages.
   -- Enabled_State : Tat_State.Tat_State_Type - Is time at tone enabled or disabled by default at startup.
   --
   overriding procedure Init (Self : in out Instance; Wait_Time_Ms : in Natural; Sync_Period : in Positive := 1; Enabled_State : in Tat_State.Tat_State_Type := Tat_State.Enabled) is
      use Tat_State;
   begin
      -- Save off initialization variables:
      Self.Wait_Time := Ada.Real_Time.Milliseconds (Wait_Time_Ms);
      Self.Sync_Period := Sync_Period;
      Self.Do_Sync_Once.Set_Var (False);

      -- Set the period based on the enable state:
      case Enabled_State is
         when Enabled =>
            Self.Send_Counter.Set_Period_And_Reset_Count (Interfaces.Unsigned_32 (Self.Sync_Period));
         when Disabled =>
            Self.Send_Counter.Set_Period_And_Reset_Count (0);
      end case;
   end Init;

   -- Send out data products:
   overriding procedure Set_Up (Self : in out Instance) is
      use Tat_State;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Update data products after init:
      if Self.Send_Counter.Get_Period = 0 then
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_At_Tone_State (The_Time, (State => Disabled)));
      else
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_At_Tone_State (The_Time, (State => Enabled)));
      end if;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Tone_Messages_Sent (The_Time, (Value => Self.Transaction_Count)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick used to trigger the sending of time messages.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Pull out some protected variables:
      Do_Once : constant Boolean := Self.Do_Sync_Once.Get_Var;
   begin
      -- If it is time to time sync based on the period or we were commanded
      -- to do so once, then perform the time at tone operation.
      if Self.Send_Counter.Is_Count_At_Period or else Do_Once then

         declare
            use Ada.Real_Time;
            use Sys_Time.Arithmetic;
            -- Grab the current time:
            Current_Sys_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
            -- Calculate the wake up time. We grab Clock as close to grabbing the
            -- system time as possible. We cannot just use Clock as the system time
            -- since getting the system time is mission dependent, and Clock may not
            -- be accurate as a system time. Clock is a good monotonic time, however,
            -- so is useful for determining the delay time.
            Wake_Up_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Self.Wait_Time;
            -- Calculate the time message time:
            Time_Message_Time : Sys_Time.T;
            Ignore : constant Sys_Time_Status := Add (Current_Sys_Time, Self.Wait_Time, Result => Time_Message_Time);
            -- Create the message:
            Message : constant Tick.T := (Time => Time_Message_Time, Count => Self.Transaction_Count);
         begin
            -- Send the time message:
            Self.Time_Message_Send_If_Connected (Message);

            -- Sleep a bit:
            delay until Wake_Up_Time;

            -- Send the tone message:
            Self.Tone_Message_Send_If_Connected (Message);

            -- Increment the transaction count:
            Self.Transaction_Count := @ + 1;

            -- Send out data products:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Tone_Messages_Sent (Time_Message_Time, (Value => Self.Transaction_Count)));
         end;

         -- Reset the "do once" variable if necessary:
         if Do_Once then
            Self.Do_Sync_Once.Set_Var (False);
         end if;
      end if;

      -- Increment the counter:
      Self.Send_Counter.Increment_Count;
   end Tick_T_Recv_Sync;

   -- The command receive connector.
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
   --    Commands for the Time at Tone Master component.
   -- This enables the sending of time at tone messages.
   overriding function Enable_Time_At_Tone (Self : in out Instance) return Command_Execution_Status.E is
      use Tat_State;
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new period:
      Self.Send_Counter.Set_Period_And_Reset_Count (Interfaces.Unsigned_32 (Self.Sync_Period));
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_At_Tone_State (The_Time, (State => Enabled)));
      -- Send out event:
      Self.Event_T_Send_If_Connected (Self.Events.Time_At_Tone_Enabled (The_Time));
      return Success;
   end Enable_Time_At_Tone;

   -- This disables the sending of time at tone messages.
   overriding function Disable_Time_At_Tone (Self : in out Instance) return Command_Execution_Status.E is
      use Tat_State;
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new period:
      Self.Send_Counter.Set_Period_And_Reset_Count (0);
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_At_Tone_State (The_Time, (State => Disabled)));
      -- Send out event:
      Self.Event_T_Send_If_Connected (Self.Events.Time_At_Tone_Disabled (The_Time));
      return Success;
   end Disable_Time_At_Tone;

   -- This sends a time at tone message followed by a tone message at the next tick, regardless of the current sync period. This is useful during testing to send a sync one time.
   overriding function Sync_Once (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Set the do once boolean to true so that we do a time sync next iteration:
      Self.Do_Sync_Once.Set_Var (True);
      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Sending_Sync_Once (Self.Sys_Time_T_Get));
      return Success;
   end Sync_Once;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

end Component.Time_At_Tone_Master.Implementation;
