--------------------------------------------------------------------------------
-- Time_Of_Tone_Master Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Time_Of_Tone_Master.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Sync_Period : Positive - The number of ticks between sending clock sync messages.
   -- Enabled_State : Tat_State.Tat_State_Type - Is time at tone enabled or disabled by default at startup.
   --
   overriding procedure Init (Self : in out Instance; Sync_Period : in Positive := 1; Enabled_State : in Tat_State.Tat_State_Type := Tat_State.Enabled) is
      use Tat_State;
   begin
      -- Save off initialization variables:
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
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Tone_Messages_Sent (The_Time, (Value => Self.Tone_Message_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_Messages_Sent (The_Time, (Value => Self.Time_Message_Count)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick used to trigger the sending of tone messages.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Pull out some protected variables:
      Do_Once : constant Boolean := Self.Do_Sync_Once.Get_Var;
   begin
      -- If it is time to time sync based on the period or we were commanded
      -- to do so once, then perform the time at tone operation.
      if Self.Send_Counter.Is_Count_At_Period or else Do_Once then

         -- Increment the transaction count:
         Self.Tone_Message_Count := @ + 1;

         declare
            -- Grab the current time:
            Current_Sys_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
            -- Create the message:
            Message : constant Tick.T := (Time => Current_Sys_Time, Count => Self.Tone_Message_Count);
         begin
            -- Send the tone message:
            Self.Tone_Message_Send_If_Connected (Message);

            -- Send out data products:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Tone_Messages_Sent (Current_Sys_Time, (Value => Self.Tone_Message_Count)));
         end;

         -- Reset the "do once" variable if necessary:
         if Do_Once then
            Self.Do_Sync_Once.Set_Var (False);
         end if;
      end if;

      -- Increment the counter:
      Self.Send_Counter.Increment_Count;
   end Tick_T_Recv_Sync;

   -- This connector triggers the sending of the time message. The time received here is assumed to be an accurate time stamp of when the tone message was sent. This time can be provided to this component by a lower level component which actually records the time the tone message leaves the system.
   overriding procedure Tone_Message_Sys_Time_T_Recv_Sync (Self : in out Instance; Arg : in Sys_Time.T) is
   begin
      -- Increment the transaction count:
      Self.Time_Message_Count := @ + 1;

      declare
         -- Grab the current time:
         Current_Sys_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
         -- Create the message:
         Message : constant Tick.T := (Time => Arg, Count => Self.Time_Message_Count);
      begin
         -- Send the tone message:
         Self.Time_Message_Send_If_Connected (Message);

         -- Send out data products:
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Time_Messages_Sent (Current_Sys_Time, (Value => Self.Time_Message_Count)));
      end;
   end Tone_Message_Sys_Time_T_Recv_Sync;

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
   --    Commands for the Time of Tone Master component.
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

end Component.Time_Of_Tone_Master.Implementation;
