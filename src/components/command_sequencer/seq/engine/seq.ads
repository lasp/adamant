with Seq_Runtime; use Seq_Runtime;
with Basic_Types; use Basic_Types;
with Seq_Types; use Seq_Types;
with Command;
with Command_Types; use Command_Types;
with Memory_Region;
with Sys_Time; use Sys_Time;
with Telemetry_Record;
with Seq_Enums; use Seq_Enums;
with Sequence_Header;
with Sequence_Types;
with Interfaces; use Interfaces;
with Seq_Print;

-- This package implements a sequence engine which is designed to run a single sequence at a time. It includes
-- a stack that allows sequences to "call" each other up to a depth limit set at initialization.
package Seq is
   use Seq_Engine_State;
   use Seq_Runtime_State;

   -- This type represents a sequence engine. It runs a single sequence at a time. Sequences can "call" other
   -- sequences which are loaded on this engine's call stack.
   type Engine is tagged private;

   --------------------
   -- Engine API
   --------------------
   -- The following subprograms allow the user to modify and fetch information about the sequence engine itself.

   -- Sets the engine stack depth and engine id. A source id must still be set before the engine is initialized though.
   procedure Initialize (Self : in out Engine; Stack_Depth : in Stack_Depth_Type; Engine_Id : in Sequence_Engine_Id) with
      Pre => (Self.Get_Engine_State = Uninitialized);

   -- Frees the engine runtime stack.
   procedure Destroy (Self : in out Engine) with
      Post => (Self.Get_Engine_State = Uninitialized);

   -- Sets the source id. Both this function and initialize must be called before the engine can be used.
   procedure Set_Source_Id (Self : in out Engine; New_Id : in Command_Source_Id) with
      Pre => (Self.Get_Engine_State = Uninitialized);

   -- Sets the engines internal arguments, these will be passed to the next sequence loaded.
   procedure Set_Arguments (Self : in out Engine; Args : in Variable_Array) with
      Pre => (Self.Get_Engine_State /= Uninitialized),
      Post => (Self.Get_Engine_State /= Uninitialized);

   -- Get this engines arguments, these are set by a sequence that triggers another sequence load.
   function Get_Arguments (Self : in Engine) return Variable_Array with
      Pre => (Self.Get_Engine_State /= Uninitialized);

   -- Get the engine id for a sequence load request
   function Get_Load_Destination (Self : in Engine) return Sequence_Engine_Id with
      Pre => (Self.Get_Engine_State /= Uninitialized);

   -- Externally sets the engine to an error state, this is also cascaded down into the current runtime instance
   procedure Set_Engine_Error (Self : in out Engine; Error_Code : in Seq_Error.E);

   -- Get the allocated stack depth for the engine. Each entry in the stack can hold a single sequence.
   function Get_Stack_Depth (Self : in Engine) return Max_Seq_Num;

   -- Resets the engine and runtime stack. Useful if a nested sequence encounters an error, will clear the calling stack.
   -- This also effectively "kills" a running engine.
   procedure Reset (Self : in out Engine) with
      Post => (Self.Get_Stack_Level = 0 and then
                   (Self.Get_Engine_State = Inactive or else Self.Get_Engine_State = Uninitialized));

   -- Get the engine state.
   function Get_Engine_State (Self : in Engine) return Seq_Engine_State.E;

   -- Get the state of the currently running sequence.
   function Get_Running_Sequence_State (Self : in Engine) return Seq_Enums.Seq_Runtime_State.E;

   -- Get the currently deepest allocated level in the stack. 0 means that the highest level
   -- sequence is running.
   function Get_Stack_Level (Self : in Engine) return Max_Seq_Num;

   -- Get the sequence ID of the sequence running in stack entry 0, the parent.
   function Get_Parent_Id (Self : in Engine) return Sequence_Types.Sequence_Id;

   -- Get the position of the sequence running in stack entry 0, the parent.
   function Get_Parent_Position (Self : in Engine) return Seq_Position;

   -- Get the ID of the sequence running in the highest stack entry.
   function Get_Lowest_Child_Id (Self : in Engine) return Sequence_Types.Sequence_Id;

   -- Get the position of the sequence running in the highest stack entry.
   function Get_Lowest_Child_Position (Self : in Engine) return Seq_Position;

   -- Get the wakeup time of the currently running sequence.
   function Get_Wakeup_Time (Self : in Engine) return Sys_Time.T;

   -- Get the time at which telemetry fetching times out.
   function Get_Telemetry_Timeout (Self : in Engine) return Sys_Time.T;

   -- Get the ID of this sequence engine, set at initialization.
   function Get_Engine_Id (Self : in Engine) return Sequence_Engine_Id;

   -- Get the command source ID of the sequence engine, set at initialization.
   function Get_Source_Id (Self : in Engine) return Command_Source_Id;

   -- Get the ID of the last command parsed by this sequence engine.
   function Get_Last_Command_Id_Sent (Self : in Engine) return Command_Id;

   -- Get the number of commands sent by this engine.
   function Get_Num_Commands_Sent (Self : in Engine) return Interfaces.Unsigned_16;

   -- Get the code of the last encountered error by the sequence engine.
   function Get_Seq_Error_Code (Self : in Engine) return Seq_Error.E;

   -- This sets an engine's state to Running and "reserves" the engine for a sequence load of the
   -- provided ID.
   procedure Reserve_Engine (Self : in out Engine; Id : in Sequence_Types.Sequence_Id) with
      Pre => (Self.Get_Engine_State /= Uninitialized),
      Post => (Self.Get_Engine_State /= Uninitialized);

   -----------------------
   -- Sequence API
   -----------------------
   -- The following subprograms allow the user to modify and fetch information about currently executing sequence
   -- in the engine.

   -- Load a new sequence into the engine. The sequence including its header must be contained within the passed in memory region.
   type Load_Status is (Success, Failure);
   function Load (Self : in out Engine; Sequence_Region : in Memory_Region.T) return Load_Status with
      Pre => (Self.Get_Engine_State /= Uninitialized and then
                  Self.Get_Stack_Level <= Self.Get_Stack_Depth),
      Post => (Self.Get_Stack_Level <= Self.Get_Stack_Depth and then
                   Self.Get_Engine_State /= Uninitialized);

   -- Execute the sequence until it reaches a blocking state. The state the sequence reaches upon block is returned. If more than "Instruction_Limit"
   -- instructions are executed before a blocking state is reached, an error will be returned. This safeguard prevents a poorly written sequence
   -- with an infinite loop from taking over the CPU.
   function Execute (Self : in out Engine; Instruction_Limit : in Positive; Timestamp : in Sys_Time.T) return Seq_Execute_State.E with
      Pre => (Self.Get_Engine_State /= Uninitialized and then
                  Self.Get_Engine_State /= Inactive and then
                  Self.Get_Engine_State /= Reserved and then
                  Self.Get_Stack_Level <= Self.Get_Stack_Depth),
      Post => (Self.Get_Stack_Level <= Self.Get_Stack_Depth and then
                   Self.Get_Engine_State /= Uninitialized);

   -- Get the next command to send from the sequence. This should be called after the engine enters the Wait_Command state.
   function Get_Command (Self : in Engine) return Command.T with
      Pre => (Self.Get_Engine_State /= Uninitialized);

   -- Get the telemetry request record. This contains information necessary to fetch and parse the telemetry required by the engine. This should
   -- be called when the sequence engine enters a wait on telemetry state.
   function Get_Telemetry_Request (Self : in Engine) return Telemetry_Record.T with
      Pre => (Self.Get_Engine_State /= Uninitialized);

   -- After telemetry has been fetched it can be provided to the engine via this subprogram.
   procedure Set_Telemetry (Self : in out Engine; Telemetry : in Poly_32_Type) with
      Pre => (Self.Get_Engine_State /= Uninitialized);

   -- The sequence engine performs all waits internally using absolute times. If a relative time operation is encountered the engine enters
   -- a wait relative state which requires the caller to pass the current time so that the absolute times can be calculated and used internally.
   procedure Change_Relative_Wait_To_Absolute (Self : in out Engine; Current_Time : in Sys_Time.T) with
      Pre => (Self.Get_Engine_State /= Uninitialized);
   procedure Change_Relative_Timeout_To_Absolute (Self : in out Engine; Current_Time : in Sys_Time.T) with
      Pre => (Self.Get_Engine_State /= Uninitialized);

   -- If the sequence engine is in a wait state, this procedure can be called to check if it is time to for the sequence engine to be woken up
   -- or not. If the engine is done waiting, the engine moves to the Active state. The current state after the check is performed is returned.
   type Done_Waiting_Status is (Done, Still_Waiting);
   function Is_Done_Waiting (Self : in out Engine; Current_Time : in Sys_Time.T) return Done_Waiting_Status with
      Pre => (Self.Get_Engine_State = Waiting);

   -- Get the ID of the sequence to load. This should be called when the sequence engine enters a wait load state.
   function Get_Seq_To_Load (Self : in Engine) return Sequence_Types.Sequence_Id with
      Pre => (Self.Get_Engine_State /= Uninitialized);

   -- Return the header of the sequence at the specified stack index.
   function Get_Sequence_Header (Self : in Engine; Index : in Max_Seq_Num) return Sequence_Header.T;

   -- Return the runtime state of the sequence at the specified stack index.
   function Get_Sequence_State (Self : in Engine; Index : in Max_Seq_Num) return Seq_Runtime_State.E;

   -- Return state after the last execution of the sequence at the specified stack index.
   function Get_Last_Execute_State (Self : in Engine) return Seq_Execute_State.E;

   -- Return memory region containing the sequence at the specified stack index.
   function Get_Sequence_Region (Self : in Engine; Index : in Max_Seq_Num) return Memory_Region.T;

   -- Return the position of the sequence at the specified stack index.
   function Get_Sequence_Position (Self : in Engine; Index : in Max_Seq_Num) return Seq_Position;

   -- Return start time of the sequence at the specified stack index.
   function Get_Sequence_Start_Time (Self : in Engine; Index : in Max_Seq_Num) return Sys_Time.T;

   -- Return the last time the sequence at the specified stack index was executed.
   function Get_Sequence_Last_Executed_Time (Self : in Engine; Index : in Max_Seq_Num) return Sys_Time.T;

   -- Get the sequence ID of the sequence reserved for this engine by the Reserve_Engine subprogram.
   function Get_Reserved_Sequence_Id (Self : in Engine) return Sequence_Types.Sequence_Id;

   -- Return the time at which the last telemetry wait started. This returns the execution time from the
   -- first time in the telemetry comparison loop.
   function Get_Sequence_Telemetry_Wait_Start_Time (Self : in Engine; Index : in Max_Seq_Num) return Sys_Time.T;

   -- Get the sequence ID of the first engine to kill.
   function Get_Kill_Eng_Start (Self : in Engine) return Sequence_Engine_Id;

   -- Get the number of engines to kill.
   function Get_Num_Eng_Kill (Self : in Engine) return Sequence_Engine_Id;

   -- Get the string to print if in the Print state.
   function Get_String_To_Print (Self : in Engine) return Seq_Print.T;

   -- Get the errant field number of the last executed sequence.
   function Get_Errant_Field_Number (Self : in Engine) return Interfaces.Unsigned_32;
private

   -- Type that allocates an array (stack) of sequence runtimes.
   type Seq_Array is array (Max_Seq_Num range <>) of Seq_Runtime.Instance;
   type Seq_Access is access all Seq_Array;

   -- Checks if the engine is ok to transfer to the inactive state, i.e. all required initialization
   -- has been completed.
   procedure Finish_Initialization (Self : in out Engine);

   -- The engine type.
   type Engine is tagged record
      Stack : Seq_Access := null;
      State : Seq_Engine_State.E := Uninitialized;
      Initialized : Boolean := False;
      Source_Id_Set : Boolean := False;
      Current : Max_Seq_Num := Max_Seq_Num'First;
      Source_Id : Command_Source_Id := Command_Source_Id'First;
      Engine_To_Load : Sequence_Engine_Id := Sequence_Engine_Id'First;
      Arguments : Variable_Array := [others => (Value => [others => 0])];
      Engine_Id : Sequence_Engine_Id := Sequence_Engine_Id'First;
      Commands_Sent : Interfaces.Unsigned_16 := 0;
      Last_Command_Id : Command_Id := 0;
      Reserved_Sequence_Id : Sequence_Types.Sequence_Id := Sequence_Types.Sequence_Id'First;
      Last_Execute_State : Seq_Execute_State.E := Seq_Execute_State.Unloaded;
   end record;

end Seq;
