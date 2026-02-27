with Interfaces; use Interfaces;
with Sys_Time;
with Command;
with Command_Types; use Command_Types;
with Basic_Types; use Basic_Types;
with Packed_I32;

with Sequence_Header;
with Memory_Region;
with Seq_Types; use Seq_Types;
with Telemetry_Record;

with Seq_Enums; use Seq_Enums;
with Sequence_Types;
with System;

with Seq_Print;
with Packed_Poly_32_Type;
with Var_Record;

-- An instanceable sequence runtime. This runtime depends on the byte code produces from the LASP SEQ
-- compiler and the LASEL (LASP Awesome Sequence Execution Language). Most features of the LASEL are
-- implemented by this runtime. See the LASEL documentation and Adamant Addendum for more details.
package Seq_Runtime is
   use Seq_Runtime_State;
   use Seq_Error;

   -- This type represents a single sequence runtime object. A sequence can be loaded into this object and run
   -- via the primitive operations below.
   type Instance is tagged private;

   ----------------
   -- Runtime API
   ----------------

   -- Return the current state of the runtime.
   function Get_State (Self : in Instance) return Seq_Runtime_State.E;

   -- Load a new sequence into the runtime.
   type Load_State_Type is (Success, Failure);
   function Load_New_Sequence (Self : in out Instance; Sequence_Region : in Memory_Region.T) return Load_State_Type with
      Pre => Self.Get_State = Unloaded or else
                Self.Get_State = Wait_Load_New_Seq_Overwrite;

   -- Execute the currently loaded sequence until it reaches a blocking state. If more than Instruction_Limit operations
   -- are encountered before a blocking state is reached than an error will be returned. This prevents an infinitely executing
   -- sequence from taking over the CPU.
   -- If the runtime is an error state, then it must be cleared before calling this function.
   function Execute_Sequence (Self : in out Instance; Instruction_Limit : in Positive; Timestamp : in Sys_Time.T) return Seq_Runtime_State.E;

   -- If the sequence is in the Wait_Command state, this will fetch the next command to execute.
   function Get_Command (Self : in Instance) return Command.T with
      Pre => Self.Get_State = Wait_Command;

   -- Get the ID of the next command to send.
   function Get_Command_Id (Self : in Instance) return Command_Id with
      Pre => Self.Get_State = Wait_Command;

   -- Get the current position of the executing sequence. This returns the byte offset into the sequence starting at zero.
   function Get_Position (Self : in Instance) return Seq_Position;

   -- Get the memory region that contains the currently executing sequence.
   function Get_Memory_Region (Self : in Instance) return Memory_Region.T;

   -- If the sequence is blocked in a wait, this function will return the time at which the sequence is expected to unblock from a
   -- relative or absolute wait.
   function Get_Wake_Time (Self : in Instance) return Sys_Time.T;

   -- Fetch the variable set by a sequence's "return" instruction.
   function Get_Return (Self : in Instance) return Packed_Poly_32_Type.T;

   -- Set the "return" instruction variable.
   procedure Set_Return (Self : in out Instance; Value : in Packed_Poly_32_Type.T);

   -- The sequence runtime functions in terms of absolute time. If a relative time is found the runtime will transition
   -- to the Wait_Relative or Wait_Telemetry_Relative state, which requires one of the next two subpograms to be called
   -- to translate that relative wait into an absolute wakeup time based on the current time passed in.
   procedure Change_Relative_Wait_To_Absolute (Self : in out Instance; Current_Time : in Sys_Time.T);
   procedure Change_Relative_Timeout_To_Absolute (Self : in out Instance; Current_Time : in Sys_Time.T);

   -- Check to see if the runtime is ready to start running a blocked sequence again. The current time is passed in
   -- and the resulting state after checking wake is returned.
   type Check_Wake_Type is (Woken, Still_Waiting);
   function Check_Wake (Self : in out Instance; Current_Time : in Sys_Time.T) return Check_Wake_Type with
      Pre => Self.Get_State = Wait_Absolute;

   -- Provide arguments to the runtime. This should be done prior to running the sequence. Arguments are usually
   -- fetched from a called sequence, and provided to the running sequence prior to the first execution.
   procedure Give_Arguments (Self : in out Instance; Args : in Variable_Array) with
      Pre => Self.Get_State = Unloaded or else
                Self.Get_State = Wait_Load_New_Seq_Overwrite;
   -- This fetches the arguments from the runtime and then zeroes out the arguments stored here.
   -- If the runtime is not waiting to load another sequence then this function call will fail.
   function Get_And_Reset_Arguments (Self : in out Instance) return Variable_Array with
      Pre => Self.Get_State = Wait_Load_New_Seq_Overwrite or else
                Self.Get_State = Wait_Load_New_Sub_Seq or else
                Self.Get_State = Wait_Load_New_Seq_Elsewhere;

   -- If a "spawn" instruction is encountered, this function will return the destination engine that the
   -- new sequence should be spawned into.
   function Get_Spawn_Destination (Self : in Instance) return Sequence_Engine_Id with
      Pre => Self.Get_State = Wait_Load_New_Seq_Elsewhere;

   -- If the runtime is in a wait on telemetry type state, this will fetch the telemetry record which holds
   -- the necessary information for fetching telemetry in the system and parsing is out appropriately.
   function Get_Telemetry_Request (Self : in Instance) return Telemetry_Record.T;

   -- Telemetry can be provided to the runtime via this subprogram.
   procedure Set_Telemetry (Self : in out Instance; Telemetry : in Poly_32_Type);

   -- Get the ID of the sequence to load. This should be called after encountering a call/spawn/start
   -- instruction, i.e. in a Wait_Load_New_* state.
   function Get_Seq_Id_To_Load (Self : in Instance) return Sequence_Types.Sequence_Id;

   -- Get the last time that the runtime was executed.
   function Get_Most_Recent_Exec_Time (Self : in Instance) return Sys_Time.T;

   -- Get the start time of when the last loaded sequence was first executed.
   function Get_Start_Time (Self : in Instance) return Sys_Time.T;

   -- Get the time at which telemetry fetching times out.
   function Get_Telemetry_Timeout (Self : in Instance) return Sys_Time.T;

   -- Unload this sequence, so that it can no longer be run, and we can accept a new sequence into this runtime.
   procedure Unload (Self : in out Instance);

   -- Fetch the time at which the last wait on telemetry commenced.
   function Get_Telemetry_Wait_Start_Time (Self : in Instance) return Sys_Time.T;

   -- Get the error code for the last encountered error.
   function Get_Error_Code (Self : in Instance) return Seq_Error.E;

   -- Get the header of the currently loaded sequence.
   function Get_Header (Self : in Instance) return Sequence_Header.T;

   -- Get the ID of the currently loaded sequence.
   function Get_Sequence_Id (Self : in Instance) return Sequence_Types.Sequence_Id;

   -- Put the runtime into an error state with the provided error code.
   procedure Force_Error (Self : in out Instance; Error_Code : in Seq_Error.E);

   -- Get the engine kill start ID.
   function Get_Kill_Eng_Start (Self : in Instance) return Sequence_Engine_Id;

   -- Get the number of engines to kill.
   function Get_Num_Eng_Kill (Self : in Instance) return Sequence_Engine_Id;

   -- Get the string to print if in the Print state.
   function Get_String_To_Print (Self : in Instance) return Seq_Print.T;

   -- Get the errant field of the last instruction that failed to parse.
   function Get_Errant_Field_Number (Self : in Instance) return Interfaces.Unsigned_32;

private
   ---------------------
   -- Utility
   ---------------------
   function Process_Error (Self : in out Instance; Error_Type : in Seq_Error.E) return Seq_Position;
   procedure Set_State_Blocking (Self : in out Instance; New_State : Seq_Runtime_State.E);
   function Get_Opcode_From_Memory (Self : in Instance) return Seq_Opcode.E;
   function Bitwise_Ops (Left : in Packed_I32.T; Right : in Packed_I32.T; Code : in Seq_Operation.E) return Packed_I32.T;
   function Validate_Jump (Self : in Instance; Jump_Dest : in Interfaces.Unsigned_16) return Seq_Status;
   procedure Set_Timeout (Self : in out Instance);
   procedure Reset_Timeout (Self : in out Instance);
   -- procedure Print_String (To_Print : in Seq_String.T);
   function Wait_On_Helper (Self : in out Instance; Wait_Type : in Seq_Wait_Type.E; Wait_Amount : in Interfaces.Unsigned_32; Jump_Position : in Interfaces.Unsigned_16) return Seq_Position;
   function Wait_Helper (Self : in out Instance; Wait_Type : in Seq_Wait_Type.E; Wait_Amount : in Interfaces.Unsigned_32) return Seq_Position;
   type Fetch_Status is (Success, Failure);
   function Fetch_Var_Helper (Self : in out Instance; Var_Info : in Var_Record.T; Var : out Packed_Poly_32_Type.T) return Fetch_Status;

   -- Generic for parsing instructions out of the sequence. Each instruction is represented by
   -- a different packed type, which can be passed to this generic to instantiate a specific
   -- "get" function for that type.
   generic
      type T is private;
      with function Valid (R : in T; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
   function Get_Instruction (Inst : in out Instance; Instruction : out T) return Seq_Status;

   -- Generic function to set an internal sequence runtime variable of type T:
   generic
      type T is private;
      with function Valid (R : in T; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
   function Get_Internal (Inst : in out Instance; Src : in Seq_Internal.E; Dest : out T) return Seq_Status;

   -- Generic function to get an internal sequence runtime variable of type T:
   generic
      type T is private;
   function Set_Internal (Inst : in out Instance; Source : in Seq_Internal.E; New_Value : in T) return Seq_Status;

   -------------------------
   -- Parsing
   -------------------------
   procedure Parse_Sequence_Header (Self : in out Instance);
   function Parse_Instruction (Self : in out Instance) return Seq_Position;

   ---------------------
   -- Seq Instructions
   ---------------------
   -- Each one of these functions parses and executes a single SEQ instruction of the same name.
   function Cmd_Set_Bit_Pattern (Self : in out Instance) return Seq_Position;
   function Cmd_Send_Bit_Pattern (Self : in out Instance) return Seq_Position;
   function Cmd_Update_Bit_Pattern (Self : in out Instance) return Seq_Position;
   function Cmd_Call (Self : in out Instance) return Seq_Position;
   function Cmd_Spawn (Self : in out Instance) return Seq_Position;
   function Cmd_Start (Self : in out Instance) return Seq_Position;
   function Cmd_Push (Self : in out Instance) return Seq_Position;
   function Cmd_Eval (Self : in out Instance) return Seq_Position;
   function Cmd_Fetch_Var (Self : in out Instance; Internal_Dst : in Seq_Internal.E) return Seq_Position;
   function Cmd_Store_Var (Self : in out Instance) return Seq_Position;
   function Cmd_Fetch_Tlm (Self : in out Instance; Internal_Dst : in Seq_Internal.E) return Seq_Position;
   function Cmd_Wait (Self : in out Instance) return Seq_Position;
   function Cmd_Goto (Self : in out Instance) return Seq_Position;
   function Cmd_Jump_If_Zero (Self : in out Instance) return Seq_Position;
   function Cmd_Jump_Not_Zero (Self : in out Instance) return Seq_Position;
   function Cmd_Jump_If_Equal (Self : in out Instance) return Seq_Position;
   function Cmd_Jump_Not_Equal (Self : in out Instance) return Seq_Position;
   function Cmd_Return (Self : in out Instance) return Seq_Position;
   function Cmd_Wait_If_Zero (Self : in out Instance) return Seq_Position;
   -- function Cmd_Kill_Category             (Self : in out Instance) return Seq_Position; Remove from compiler, this instruction will NOT be implemented!
   function Cmd_Kill_Engine (Self : in out Instance) return Seq_Position;
   -- function Cmd_Kill_Name                   (Self : in out Instance) return Seq_Position; Remove from compiler, this instruction will NOT be implemented!
   -- function Cmd_Subscribe                   (Self : in out Instance) return Seq_Position; Not implemented, Adamant does not do sub/unsub for telemetry items.
   -- function Cmd_Unsubscribe                (Self : in out Instance) return Seq_Position; Not implemented, Adamant does not do sub/unsub for telemetry items.
   function Cmd_Eval_Flt (Self : in out Instance) return Seq_Position;
   function Cmd_Cast_F_To_U (Self : in out Instance) return Seq_Position;
   function Cmd_Cast_U_To_F (Self : in out Instance) return Seq_Position;
   function Cmd_Eval_S (Self : in out Instance) return Seq_Position;
   function Cmd_Cast_S_To_U (Self : in out Instance) return Seq_Position;
   function Cmd_Cast_U_To_S (Self : in out Instance) return Seq_Position;
   function Cmd_Cast_F_To_S (Self : in out Instance) return Seq_Position;
   function Cmd_Cast_S_To_F (Self : in out Instance) return Seq_Position;
   function Cmd_Wait_On_B (Self : in out Instance) return Seq_Position;
   function Cmd_Wait_If_Zero_On_B (Self : in out Instance) return Seq_Position;

   -- This runtime does not support strings or string variables. The only use of strings
   -- are in statically defined print opcodes.
   function Cmd_Str_Alloc (Self : in out Instance) return Seq_Position; -- Treated as noop
   -- function Cmd_Str_Dealloc                (Self : in out Instance) return Seq_Position;
   -- function Cmd_Str_Set (Self : in out Instance) return Seq_Position;
   -- function Cmd_Str_Update_Pattern      (Self : in out Instance) return Seq_Position;
   -- function Cmd_Str_Copy                     (Self : in out Instance) return Seq_Position;
   -- function Cmd_Str_Move                     (Self : in out Instance) return Seq_Position;

   -- Print opcodes:
   function Cmd_Print (Self : in out Instance) return Seq_Position;
   function Cmd_Print_Var (Self : in out Instance) return Seq_Position;
   -- We do not support the following because it requires string support, see above.
   -- function Cmd_Print_Str (Self : in out Instance) return Seq_Position;

   -------------------------
   -- Instance Data
   -------------------------
   type Instance is tagged record
      Sequence_Region : Memory_Region.T := (Length => 0, Address => System.Null_Address);
      Position : Seq_Position := 0;
      Next_Position : Seq_Position := 0;
      Bit_Pattern : Command.T;
      Seq_Header : Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 0);
      Internals : Internal_Array := [others => (Value => [0, 0, 0, 0])];
      Local_Variable_Array : Variable_Array := [others => (Value => [0, 0, 0, 0])];
      -- We are not supporting strings yet...
      -- Local_String_Pool : String_Pool := [others => [others => Character'Val (0)]]; -- This is a null initialization
      Out_Arguments : Variable_Array := [others => (Value => [0, 0, 0, 0])];
      Telemetry_Request : Telemetry_Record.T := (Id => 0, Offset => 0, Size => 1, Tlm_Type => Seq_Data_Format.Unsigned_Byte, New_Value_Required => False);
      Telemetry_Destination : Seq_Internal.E := Seq_Internal.Timeout;
      Telemetry_Timeout : Sys_Time.T := (0, 0);
      Kill_Engine_Start : Sequence_Engine_Id := Sequence_Engine_Id'First;
      Num_Eng_To_Kill : Sequence_Engine_Id := Sequence_Engine_Id'First;
      Timeout_Set : Boolean := False;
      Wake_Time : Sys_Time.T := (0, 0);
      First_Execution_Time : Sys_Time.T := (0, 0);
      First_Execution : Boolean := True;
      Most_Recent_Execution_Time : Sys_Time.T := (0, 0);
      State : Seq_Runtime_State.E := Seq_Runtime_State.Unloaded;
      Telemetry_Wait_Start_Time : Sys_Time.T := (0, 0);
      Blocked : Boolean := False;
      Error_Code : Seq_Error.E := Seq_Error.None;
      Seq_Id_To_Load : Sequence_Types.Sequence_Id := Sequence_Types.Sequence_Id'First;
      Spawn_Destination : Sequence_Engine_Id := Sequence_Engine_Id'First;
      Errant_Field : Interfaces.Unsigned_32 := 0;
      String_To_Print : Seq_Print.T := (Print_Type => Seq_Print_Type.Debug, Encoded_String => [others => 0]);
   end record;

end Seq_Runtime;
