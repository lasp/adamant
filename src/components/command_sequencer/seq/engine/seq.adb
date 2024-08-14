-- with Ada.Text_IO; use Ada.Text_IO;
with Safe_Deallocator;

package body Seq is
   use Seq_Error;

   procedure Initialize (Self : in out Engine; Stack_Depth : in Stack_Depth_Type; Engine_Id : in Sequence_Engine_Id) is
   begin
      pragma Assert (Stack_Depth < 255, "GNAT SAS points out that 255 can break things.");
      Self.Stack := new Seq_Array (Max_Seq_Num'First .. Max_Seq_Num'First + Stack_Depth - 1);
      Self.Initialized := True;
      Self.Engine_Id := Engine_Id;
      Self.Finish_Initialization;
   end Initialize;

   procedure Set_Source_Id (Self : in out Engine; New_Id : in Command_Source_Id) is
   begin
      Self.Source_Id := New_Id;
      Self.Source_Id_Set := True;
      Self.Finish_Initialization;
   end Set_Source_Id;

   -- Set engine to the inactive state if it has finished being properly initialized. This requires
   -- the Initialize and Set_Source_Id procedures to be called prior.
   procedure Finish_Initialization (Self : in out Engine) is
   begin
      if Self.Initialized and then Self.Source_Id_Set then
         Self.State := Inactive;
      end if;
   end Finish_Initialization;

   procedure Destroy (Self : in out Engine) is
      procedure Free is new Safe_Deallocator.Deallocate_If_Testing (Object => Seq_Array, Name => Seq_Access);
   begin
      -- Reset state:
      Self.Reset;

      -- Free the stack:
      Free (Self.Stack);

      -- Set some of our state to uninitialized values.
      Self.Stack := null;
      Self.Initialized := False;
      Self.Source_Id_Set := False;
      Self.State := Uninitialized;
   end Destroy;

   function Get_Engine_State (Self : in Engine) return Seq_Engine_State.E is
   begin
      return Self.State;
   end Get_Engine_State;

   procedure Set_Arguments (Self : in out Engine; Args : in Variable_Array) is
   begin
      Self.Arguments := Args;
   end Set_Arguments;

   procedure Reserve_Engine (Self : in out Engine; Id : in Sequence_Types.Sequence_Id) is
   begin
      Self.State := Reserved;
      Self.Reserved_Sequence_Id := Id;
   end Reserve_Engine;

   function Get_Arguments (Self : in Engine) return Variable_Array is
   begin
      return Self.Arguments;
   end Get_Arguments;

   function Get_Load_Destination (Self : in Engine) return Sequence_Engine_Id is
   begin
      return Self.Engine_To_Load;
   end Get_Load_Destination;

   function Get_Engine_Id (Self : in Engine) return Sequence_Engine_Id is
   begin
      return Self.Engine_Id;
   end Get_Engine_Id;

   procedure Set_Engine_Error (Self : in out Engine; Error_Code : in Seq_Error.E) is
   begin
      Self.State := Engine_Error;
      Self.Last_Execute_State := Seq_Execute_State.Error;
      if Self.Stack /= null then
         Self.Stack.all (Self.Current).Force_Error (Error_Code);
      end if;
   end Set_Engine_Error;

   function Get_Stack_Depth (Self : in Engine) return Max_Seq_Num is
      To_Return : Max_Seq_Num := Max_Seq_Num'First;
   begin
      if Self.Stack /= null then
         To_Return := Self.Stack'Length;
      end if;
      return To_Return;
   end Get_Stack_Depth;

   function Get_Num_Commands_Sent (Self : in Engine) return Interfaces.Unsigned_16 is
   begin
      return Self.Commands_Sent;
   end Get_Num_Commands_Sent;

   function Get_Stack_Level (Self : in Engine) return Max_Seq_Num is
   begin
      return Self.Current;
   end Get_Stack_Level;

   function Get_Parent_Id (Self : in Engine) return Sequence_Types.Sequence_Id is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Stack.all'First).Get_Sequence_Id;
      else
         return Sequence_Types.Sequence_Id'First;
      end if;
   end Get_Parent_Id;

   function Get_Parent_Position (Self : in Engine) return Seq_Position is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Stack.all'First).Get_Position;
      else
         return Seq_Position'First;
      end if;
   end Get_Parent_Position;

   function Get_Lowest_Child_Id (Self : in Engine) return Sequence_Types.Sequence_Id is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Current).Get_Sequence_Id;
      else
         return Sequence_Types.Sequence_Id'First;
      end if;
   end Get_Lowest_Child_Id;

   function Get_Lowest_Child_Position (Self : in Engine) return Seq_Position is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Current).Get_Position;
      else
         return Seq_Position'First;
      end if;
   end Get_Lowest_Child_Position;

   function Get_Wakeup_Time (Self : in Engine) return Sys_Time.T is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Current).Get_Wake_Time;
      else
         return (0, 0);
      end if;
   end Get_Wakeup_Time;

   function Get_Telemetry_Timeout (Self : in Engine) return Sys_Time.T is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Current).Get_Telemetry_Timeout;
      else
         return (0, 0);
      end if;
   end Get_Telemetry_Timeout;

   function Get_Last_Command_Id_Sent (Self : in Engine) return Command_Id is
   begin
      return Self.Last_Command_Id;
   end Get_Last_Command_Id_Sent;

   function Get_Seq_Error_Code (Self : in Engine) return Seq_Error.E is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Current).Get_Error_Code;
      else
         return None;
      end if;
   end Get_Seq_Error_Code;

   function Get_Reserved_Sequence_Id (Self : in Engine) return Sequence_Types.Sequence_Id is
   begin
      return Self.Reserved_Sequence_Id;
   end Get_Reserved_Sequence_Id;

   procedure Reset (Self : in out Engine) is
   begin
      -- Unload all sequences:
      if Self.Stack /= null then
         for Stack_Entry of Self.Stack.all loop
            Stack_Entry.Unload;
         end loop;
      end if;

      -- Reset some important state:
      Self.Current := Max_Seq_Num'First;
      Self.Arguments := [others => (Value => [others => 0])];
      Self.Last_Command_Id := 0;
      Self.Reserved_Sequence_Id := Sequence_Types.Sequence_Id'First;
      Self.Last_Execute_State := Seq_Execute_State.Unloaded;
      Self.State := Uninitialized;

      -- Set state to inactive if we are initialized.
      Self.Finish_Initialization;
   end Reset;

   -- This function returns the engine state, which allows us to hide Seq_Runtime_State upstream
   function Load (Self : in out Engine; Sequence_Region : in Memory_Region.T) return Load_Status is
      Load_State : Seq_Runtime.Load_State_Type;
   begin
      -- Increment only when loading a new sub-sequence
      if Self.Stack.all (Self.Current).Get_State = Seq_Runtime_State.Wait_Load_New_Sub_Seq then
         if Self.Current + 1 > Self.Stack.all'Last then
            Self.Set_Engine_Error (Load);
            return Failure;
         else
            Self.Current := Self.Current + 1;
         end if;
      end if;

      -- This is not expressed in a pre-condition because there are other runtime states that load may get called in because load handles changing of the stack pointer.
      -- Changing the stack pointer will change the runtime state of the current sequence DURING this function.
      if Self.Stack.all (Self.Current).Get_State /= Seq_Runtime_State.Unloaded and then Self.Stack.all (Self.Current).Get_State /= Seq_Runtime_State.Wait_Load_New_Seq_Overwrite then
         Self.Set_Engine_Error (Load);
         return Failure;
      end if;

      -- Load the new sequence
      Self.Stack.all (Self.Current).Give_Arguments (Self.Arguments);
      Self.Arguments := [others => (Value => [others => 0])]; -- Reset engine argument buffer

      -- Load the sequence
      Load_State := Self.Stack.all (Self.Current).Load_New_Sequence (Sequence_Region);

      -- If the load succeeded then load the arguments and set the engine to ACTIVE
      -- Else set the engine to an error state.
      case Load_State is
         when Success =>
            -- Set the engine state to active.
            Self.State := Active;
         when Failure =>
            -- Set our state to engine error. The error code will have already been set
            -- by the sequence runtime.
            Self.State := Engine_Error;
            return Failure;
      end case;

      return Success;
   end Load;

   function Execute (Self : in out Engine; Instruction_Limit : in Positive; Timestamp : in Sys_Time.T) return Seq_Execute_State.E is
      Runtime_State : constant Seq_Runtime_State.E := Self.Stack.all (Self.Current).Execute_Sequence (Instruction_Limit, Timestamp);
   begin
      case Runtime_State is
         -- If we ever see these states following an engine execution then there is a software bug.
         when Seq_Runtime_State.Ready | Seq_Runtime_State.Telemetry_Set | Seq_Runtime_State.Timeout | Seq_Runtime_State.Unloaded =>
            pragma Assert (False);
            return Self.Last_Execute_State;
         -- If the sequence is done running then we pop it off the stack and start executing the parent sequence. If this is already
         -- the parent sequence, then we set the engine to inactive.
         when Seq_Runtime_State.Done =>
            -- Take return internal from finished sequence and pass it to the calling sequence
            if Self.Current > Self.Stack.all'First then
               -- Copy the return variable from the callee to the caller sequence.
               Self.Stack.all (Self.Current - 1).Set_Return (Self.Stack.all (Self.Current).Get_Return);
               -- Unload the current runtime stack entry
               Self.Stack.all (Self.Current).Unload;
               -- Pop the finished sequence off the stack
               Self.Current := Self.Current - 1;
               -- Continue executing the caller sequence.
               return Self.Execute (Instruction_Limit, Timestamp);
            else
               -- If we are here then the engine has finished executing the entire stack, and can transition to an inactive state
               Self.Reset;
               return Self.Last_Execute_State;
            end if;
         -- If we need to load a new sequence, we collapse all the states into one state
         when Seq_Runtime_State.Wait_Load_New_Seq_Overwrite | Seq_Runtime_State.Wait_Load_New_Sub_Seq | Seq_Runtime_State.Wait_Load_New_Seq_Elsewhere =>
            -- Load the runtime arguments into the engine arguments
            Self.Arguments := Self.Stack.all (Self.Current).Get_And_Reset_Arguments;
            -- If we need to load elsewhere, get the destination
            if Runtime_State = Seq_Runtime_State.Wait_Load_New_Seq_Elsewhere then
               Self.Engine_To_Load := Self.Stack.all (Self.Current).Get_Spawn_Destination;

               -- Makes sure we are not spawning into the same engine, please use start for that
               if Self.Engine_To_Load = Self.Engine_Id then
                  Self.Set_Engine_Error (Spawn);
                  return Self.Last_Execute_State;
               end if;
            else
               Self.Engine_To_Load := Self.Engine_Id; -- Otherwise we are loading to this engine
            end if;
            Self.Last_Execute_State := Seq_Execute_State.Wait_Load_Seq;
            return Self.Last_Execute_State;
         -- These two states switch the engine into a waiting state. This allows us to hide Seq_Runtime_State upstream
         when Seq_Runtime_State.Wait_Relative =>
            Self.State := Waiting;
            Self.Last_Execute_State := Seq_Execute_State.Wait_Relative;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Wait_Absolute =>
            Self.State := Waiting;
            Self.Last_Execute_State := Seq_Execute_State.Wait_Absolute;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Wait_Command =>
            Self.Last_Command_Id := Self.Stack.all (Self.Current).Get_Command_Id;
            Self.Commands_Sent := Self.Commands_Sent + 1;
            Self.Last_Execute_State := Seq_Execute_State.Wait_Command;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Wait_Telemetry_Set =>
            Self.Last_Execute_State := Seq_Execute_State.Set_Telemetry;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Wait_Telemetry_Value =>
            Self.Last_Execute_State := Seq_Execute_State.Wait_Telemetry;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Kill_Engine =>
            Self.Last_Execute_State := Seq_Execute_State.Kill_Engines;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Wait_Telemetry_Relative =>
            Self.Last_Execute_State := Seq_Execute_State.Wait_Telemetry_Relative;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Error =>
            Self.State := Engine_Error;
            Self.Last_Execute_State := Seq_Execute_State.Error;
            return Self.Last_Execute_State;
         when Seq_Runtime_State.Print =>
            Self.Last_Execute_State := Seq_Execute_State.Print;
            return Self.Last_Execute_State;
      end case;
   end Execute;

   function Get_Source_Id (Self : in Engine) return Command_Source_Id is
   begin
      return Self.Source_Id;
   end Get_Source_Id;

   function Get_Running_Sequence_State (Self : in Engine) return Seq_Runtime_State.E is
   begin
      return Self.Get_Sequence_State (Index => Self.Current);
   end Get_Running_Sequence_State;

   function Get_Command (Self : in Engine) return Command.T is
   begin
      return Self.Stack.all (Self.Current).Get_Command;
   end Get_Command;

   function Get_Telemetry_Request (Self : in Engine) return Telemetry_Record.T is
   begin
      return Self.Stack.all (Self.Current).Get_Telemetry_Request;
   end Get_Telemetry_Request;

   procedure Set_Telemetry (Self : in out Engine; Telemetry : in Poly_32_Type) is
   begin
      Self.Stack.all (Self.Current).Set_Telemetry (Telemetry);
   end Set_Telemetry;

   -- Runtime doesn't have access to time
   procedure Change_Relative_Wait_To_Absolute (Self : in out Engine; Current_Time : in Sys_Time.T) is
   begin
      Self.Stack.all (Self.Current).Change_Relative_Wait_To_Absolute (Current_Time);
      Self.Last_Execute_State := Seq_Execute_State.Wait_Absolute;
   end Change_Relative_Wait_To_Absolute;

   procedure Change_Relative_Timeout_To_Absolute (Self : in out Engine; Current_Time : in Sys_Time.T) is
   begin
      Self.Stack.all (Self.Current).Change_Relative_Timeout_To_Absolute (Current_Time);
      Self.Last_Execute_State := Seq_Execute_State.Wait_Telemetry;
   end Change_Relative_Timeout_To_Absolute;

   function Is_Done_Waiting (Self : in out Engine; Current_Time : in Sys_Time.T) return Done_Waiting_Status is
      Wake_Status : constant Seq_Runtime.Check_Wake_Type := Self.Stack.all (Self.Current).Check_Wake (Current_Time);
   begin
      case Wake_Status is
         when Woken =>
            -- Set engine state to active:
            Self.State := Active;
            return Done;
         when Still_Waiting =>
            -- We are already in the waiting state based on the precondition.
            return Still_Waiting;
      end case;
   end Is_Done_Waiting;

   function Get_Seq_To_Load (Self : in Engine) return Sequence_Types.Sequence_Id is
   begin
      return Self.Stack.all (Self.Current).Get_Seq_Id_To_Load;
   end Get_Seq_To_Load;

   function Get_Sequence_Header (Self : in Engine; Index : in Max_Seq_Num) return Sequence_Header.T is
   begin
      return Self.Stack.all (Index).Get_Header;
   end Get_Sequence_Header;

   function Get_Sequence_State (Self : in Engine; Index : in Max_Seq_Num) return Seq_Runtime_State.E is
   begin
      return Self.Stack.all (Index).Get_State;
   end Get_Sequence_State;

   function Get_Last_Execute_State (Self : in Engine) return Seq_Execute_State.E is
   begin
      return Self.Last_Execute_State;
   end Get_Last_Execute_State;

   function Get_Sequence_Region (Self : in Engine; Index : in Max_Seq_Num) return Memory_Region.T is
   begin
      return Self.Stack.all (Index).Get_Memory_Region;
   end Get_Sequence_Region;

   function Get_Sequence_Position (Self : in Engine; Index : in Max_Seq_Num) return Seq_Position is
   begin
      return Self.Stack.all (Index).Get_Position;
   end Get_Sequence_Position;

   function Get_Sequence_Start_Time (Self : in Engine; Index : in Max_Seq_Num) return Sys_Time.T is
   begin
      return Self.Stack.all (Index).Get_Start_Time;
   end Get_Sequence_Start_Time;

   function Get_Sequence_Last_Executed_Time (Self : in Engine; Index : in Max_Seq_Num) return Sys_Time.T is
   begin
      return Self.Stack.all (Index).Get_Most_Recent_Exec_Time;
   end Get_Sequence_Last_Executed_Time;

   function Get_Sequence_Telemetry_Wait_Start_Time (Self : in Engine; Index : in Max_Seq_Num) return Sys_Time.T is
   begin
      return Self.Stack.all (Index).Get_Telemetry_Wait_Start_Time;
   end Get_Sequence_Telemetry_Wait_Start_Time;

   function Get_Kill_Eng_Start (Self : in Engine) return Sequence_Engine_Id is
   begin
      return Self.Stack.all (Self.Current).Get_Kill_Eng_Start;
   end Get_Kill_Eng_Start;

   function Get_Num_Eng_Kill (Self : in Engine) return Sequence_Engine_Id is
   begin
      return Self.Stack.all (Self.Current).Get_Num_Eng_Kill;
   end Get_Num_Eng_Kill;

   function Get_String_To_Print (Self : in Engine) return Seq_Print.T is
   begin
      return Self.Stack.all (Self.Current).Get_String_To_Print;
   end Get_String_To_Print;

   function Get_Errant_Field_Number (Self : in Engine) return Interfaces.Unsigned_32 is
   begin
      if Self.Stack /= null then
         return Self.Stack.all (Self.Current).Get_Errant_Field_Number;
      else
         return Interfaces.Unsigned_32'First;
      end if;
   end Get_Errant_Field_Number;

end Seq;
