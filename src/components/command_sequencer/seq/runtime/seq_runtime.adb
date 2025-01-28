with System.Storage_Elements;
with Serializer_Types; use Serializer_Types;
with Byte_Array_Util; use Byte_Array_Util;

-- Instruction records
with Set_Bit_Record.Validation;
with Send_Bit_Record;
with Update_Bit_Record.Validation;
with Fetch_Var_Record.Validation;
with Store_Var_Record.Validation;
with Fetch_Tlm_Record.Validation;
with Wait_Record.Validation;
with Goto_Record.Validation;
with Jump_Zero_Record.Validation;
with Jump_Equal_Record.Validation;
with Cast_Record.Validation;
with Eval_Record.Validation;
with Load_New_Seq_Record.Validation;
with Push_Record.Validation;
with Wait_If_Zero_Record.Validation;
with Wait_On_B_Record.Validation;
with Wait_If_Zero_On_B_Record.Validation;
with Kill_Eng_Record.Validation;
with Sequence_Header.Validation;
with Command_Header;

with Str_Alloc_Record.Validation;
-- with Str_Set_RecordValidation;
with Print_Record.Validation;
-- with Print_Str_Record;
-- with Print_Str_Record.Validation;
with Print_Var_Record.Validation;
with Packed_Seq_Opcode;

-- Other Packed Records
with Packed_Natural_32;
with Packed_U32.Validation;
with Packed_I32.Validation;
with Packed_F32.Validation;
with Packed_Natural_32.Validation;
with Packed_Poly_32_Type.Validation;
-- with Ada.Text_IO; use Ada.Text_IO;

package body Seq_Runtime is

   use Seq_Operation;
   use Seq_Opcode;
   use Seq_Wait_Type;
   use Var_Origin;

   --------------------------------
   -- Public
   --------------------------------

   -- Save off the error code and put the runtime into the Error state, and block further execution.
   function Process_Error (Self : in out Instance; Error_Type : in Seq_Error.E) return Seq_Position is
   begin
      Self.Error_Code := Error_Type;
      -- Put_Line ("Error encountered:"&Error_Type'Image&" at pc"&Self.Position'Image);
      Self.Set_State_Blocking (Error);
      return Self.Position;
   end Process_Error;

   -- Load a new sequence
   function Load_New_Sequence (Self : in out Instance; Sequence_Region : in Memory_Region.T) return Load_State_Type is
      Ignore : Seq_Position;
   begin
      -- Clean up instance data, except variables (due to respecting argument loads)
      Self.Unload;

      -- Check that the length is at least as big as the sequence header. Any smaller would be erroneous.
      if Sequence_Region.Length < Sequence_Header.Size_In_Bytes then
         Ignore := Self.Process_Error (Load_Header);
         return Failure;
      end if;

      -- Set instance buffer
      Self.Sequence_Region := Sequence_Region;

      -- Parse seq header
      Self.Parse_Sequence_Header;

      -- Add length checking to header
      if Natural (Self.Seq_Header.Length) > Sequence_Region.Length then
         Ignore := Self.Process_Error (Load_Length);
         return Failure;
      end if;

      Self.Set_State_Blocking (Ready);
      return Success;
   end Load_New_Sequence;

   -- Run engine until it is blocked or it completes the sequence.
   function Execute_Sequence (Self : in out Instance; Instruction_Limit : in Positive; Timestamp : in Sys_Time.T) return Seq_Runtime_State.E is
      Instruction_Counter : Natural := 0;
   begin
      Self.Blocked := False;

      -- Deny execution for certain states
      case Self.State is
         when Ready | Wait_Command | Wait_Telemetry_Value | Telemetry_Set | Timeout | Wait_Load_New_Seq_Overwrite | Wait_Load_New_Sub_Seq | Wait_Load_New_Seq_Elsewhere | Kill_Engine | Print | Error =>
            null;
         when Unloaded | Wait_Relative | Wait_Absolute | Wait_Telemetry_Set | Wait_Telemetry_Relative | Done =>
            return Self.State;
      end case;

      -- Save first execution timestamp
      if Self.First_Execution then
         Self.First_Execution_Time := Timestamp;
         Self.First_Execution := False;
      end if;

      -- Save most recent execution timestamp
      Self.Most_Recent_Execution_Time := Timestamp;

      -- Starting from the engines current position iterate over the sequence and execute instructions until
      -- the engine changes to a blocking state, exceeds the instruction limit, or finishes.
      while Self.Position < Seq_Position (Self.Seq_Header.Length) loop
         -- Execute the next instruction in the sequence
         Self.Position := Self.Parse_Instruction;

         -- If the instruction from this iteration changed the state of the engine, return.
         if Self.Blocked = True then
            return Self.State;
         end if;

         -- Increment the instruction counter
         -- Then check if we have hit the instruction limit.
         -- If we have, return with a READY state so we can resume execution during the next cycle.
         Instruction_Counter := @ + 1;
         if Instruction_Counter >= Instruction_Limit then
            declare
               Ignore : Seq_Position := Self.Process_Error (Limit);
            begin
               return Self.State;
            end;
         end if;
      end loop;

      -- If we reach the end of the above loop then we have completed the entire sequence.
      Self.Set_State_Blocking (Done);
      return Self.State;
   end Execute_Sequence;

   -- Unload any currently running sequence from the runtime and reset the state of all internal
   -- variables to get ready for a future sequence load.
   procedure Unload (Self : in out Instance) is
   begin
      -- Reset all important state for the runtime except for internal variables.
      Self.Sequence_Region := (Length => 0, Address => System.Null_Address);
      Self.Position := 0;
      Self.Seq_Header := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 0);
      Self.Internals := [others => (Value => [0, 0, 0, 0])];
      Self.Out_Arguments := [others => (Value => [0, 0, 0, 0])];
      Self.Telemetry_Timeout := (0, 0);
      Self.Timeout_Set := False;
      Self.Wake_Time := (0, 0);
      Self.First_Execution_Time := (0, 0);
      Self.First_Execution := True;
      Self.Most_Recent_Execution_Time := (0, 0);
      Self.Blocked := False;
      Self.Error_Code := None;
      Self.Errant_Field := 0;
      Self.State := Unloaded;
      Self.Next_Position := 0;
      Self.Bit_Pattern := (Header => (Source_Id => 0, Id => 0, Arg_Buffer_Length => 0), Arg_Buffer => [others => 0]);
      Self.Telemetry_Request := (Id => 0, Offset => 0, Size => 1, Tlm_Type => Seq_Data_Format.Unsigned_Byte, New_Value_Required => False);
      Self.Telemetry_Destination := Seq_Internal.Timeout;
      Self.Kill_Engine_Start := Sequence_Engine_Id'First;
      Self.Num_Eng_To_Kill := Sequence_Engine_Id'First;
      Self.Telemetry_Wait_Start_Time := (0, 0);
      Self.Seq_Id_To_Load := Sequence_Types.Sequence_Id'First;
      Self.Spawn_Destination := Sequence_Engine_Id'First;

      -- Note: We do NOT reset the following, as these can be used after the sequence has been unloaded to provide
      -- information to another running sequence.
      --
      --    Self.Local_Variable_Array
      --    Self.Local_String_Pool
      --
   end Unload;

   -------------------
   -- Instance API
   -------------------

   -- Get engine state
   function Get_State (Self : in Instance) return Seq_Runtime_State.E is
   begin
      return Self.State;
   end Get_State;

   -- Returns the internally stored bit pattern (an adamant command that
   -- was just extracted)
   function Get_Command (Self : in Instance) return Command.T is
   begin
      return Self.Bit_Pattern;
   end Get_Command;

   -- Get the command ID of the command most recently extracted.
   function Get_Command_Id (Self : in Instance) return Command_Id is
   begin
      return Self.Bit_Pattern.Header.Id;
   end Get_Command_Id;

   -- Get the current position the runtime is executing with in the sequence
   function Get_Position (Self : in Instance) return Seq_Position is
   begin
      return Self.Position;
   end Get_Position;

   -- Get the memory region of the currently executing sequence
   function Get_Memory_Region (Self : in Instance) return Memory_Region.T is
   begin
      return Self.Sequence_Region;
   end Get_Memory_Region;

   -- Get the wake time.
   function Get_Wake_Time (Self : in Instance) return Sys_Time.T is
   begin
      return Self.Wake_Time;
   end Get_Wake_Time;

   -- The next two functions transform relative waits to absolute waits.
   -- This is required for waiting at least as long as the wait has requested to wait for.
   -- The sequencer does not have access to the current time, and so must ask the component for it to accurately wait a relative amount.

   -- Sets the engines wake time. This is only called for relative waits to transform them into absolute waits.
   procedure Change_Relative_Wait_To_Absolute (Self : in out Instance; Current_Time : in Sys_Time.T) is
   begin
      Self.Wake_Time.Seconds := @ + Current_Time.Seconds;
      Self.Set_State_Blocking (Wait_Absolute);
   end Change_Relative_Wait_To_Absolute;

   -- Telemetry timeouts come in as relative, so must change to absolute.
   procedure Change_Relative_Timeout_To_Absolute (Self : in out Instance; Current_Time : in Sys_Time.T) is
   begin
      Self.Telemetry_Timeout.Seconds := @ + Current_Time.Seconds;
      Self.Set_State_Blocking (Wait_Telemetry_Value);
   end Change_Relative_Timeout_To_Absolute;

   -- Check if the engine should wake up.
   function Check_Wake (Self : in out Instance; Current_Time : in Sys_Time.T) return Check_Wake_Type is
   begin
      if Current_Time.Seconds >= Self.Wake_Time.Seconds then
         Self.Set_State_Blocking (Ready);
         return Woken;
      end if;
      return Still_Waiting;
   end Check_Wake;

   -- Takes arguments in and updates the local var array.
   -- The runtime must be unloaded for this function call to be successful
   procedure Give_Arguments (Self : in out Instance; Args : in Variable_Array) is
   begin
      Self.Local_Variable_Array := Args;
   end Give_Arguments;

   -- This fetches the arguments from the runtime and then zeroes out the arguments stored here.
   -- If the runtime is not waiting to load another sequence then this function call will fail.
   function Get_And_Reset_Arguments (Self : in out Instance) return Variable_Array is
      Temp : constant Variable_Array := Self.Out_Arguments;
   begin
      -- Reset arguments,
      Self.Out_Arguments := [others => (Value => [0, 0, 0, 0])];
      -- Return the copy.
      return Temp;
   end Get_And_Reset_Arguments;

   function Get_Spawn_Destination (Self : in Instance) return Sequence_Engine_Id is
   begin
      return Self.Spawn_Destination;
   end Get_Spawn_Destination;

   function Get_Telemetry_Request (Self : in Instance) return Telemetry_Record.T is
   begin
      return Self.Telemetry_Request;
   end Get_Telemetry_Request;

   procedure Set_Telemetry (Self : in out Instance; Telemetry : in Poly_32_Type) is
      function Set_Internal_Poly_32 is new Set_Internal (Poly_32_Type);
      Ignore : Seq_Status;
   begin
      if Self.State = Wait_Telemetry_Value or else Self.State = Wait_Telemetry_Set then
         Ignore := Set_Internal_Poly_32 (Self, Self.Telemetry_Destination, Telemetry);
         Self.Set_State_Blocking (Telemetry_Set);
         pragma Assert (Ignore = Success);
      end if;
   end Set_Telemetry;

   function Get_Seq_Id_To_Load (Self : in Instance) return Sequence_Types.Sequence_Id is
   begin
      return Self.Seq_Id_To_Load;
   end Get_Seq_Id_To_Load;

   function Get_Most_Recent_Exec_Time (Self : in Instance) return Sys_Time.T is
   begin
      return Self.Most_Recent_Execution_Time;
   end Get_Most_Recent_Exec_Time;

   function Get_Start_Time (Self : in Instance) return Sys_Time.T is
   begin
      return Self.First_Execution_Time;
   end Get_Start_Time;

   function Get_Telemetry_Timeout (Self : in Instance) return Sys_Time.T is
   begin
      return Self.Telemetry_Timeout;
   end Get_Telemetry_Timeout;

   function Get_Telemetry_Wait_Start_Time (Self : in Instance) return Sys_Time.T is
   begin
      return Self.Telemetry_Wait_Start_Time;
   end Get_Telemetry_Wait_Start_Time;

   function Get_Error_Code (Self : in Instance) return Seq_Error.E is
   begin
      return Self.Error_Code;
   end Get_Error_Code;

   function Get_Header (Self : in Instance) return Sequence_Header.T is
   begin
      return Self.Seq_Header;
   end Get_Header;

   function Get_Sequence_Id (Self : in Instance) return Sequence_Types.Sequence_Id is
   begin
      return Self.Seq_Header.Id;
   end Get_Sequence_Id;

   procedure Force_Error (Self : in out Instance; Error_Code : in Seq_Error.E) is
      Ignore : Seq_Position;
   begin
      Ignore := Self.Process_Error (Error_Code);
   end Force_Error;

   function Get_Return (Self : in Instance) return Packed_Poly_32_Type.T is
      use Seq_Internal;
   begin
      return Self.Internals (Seq_Internal.E'Pos (Seq_Return));
   end Get_Return;

   procedure Set_Return (Self : in out Instance; Value : in Packed_Poly_32_Type.T) is
      use Seq_Internal;
   begin
      Self.Internals (Seq_Internal.E'Pos (Seq_Return)) := Value;
   end Set_Return;

   function Get_Kill_Eng_Start (Self : in Instance) return Sequence_Engine_Id is
   begin
      return Self.Kill_Engine_Start;
   end Get_Kill_Eng_Start;

   function Get_Num_Eng_Kill (Self : in Instance) return Sequence_Engine_Id is
   begin
      return Self.Num_Eng_To_Kill;
   end Get_Num_Eng_Kill;

   function Get_String_To_Print (Self : in Instance) return Seq_Print.T is
   begin
      return Self.String_To_Print;
   end Get_String_To_Print;

   function Get_Errant_Field_Number (Self : in Instance) return Interfaces.Unsigned_32 is
   begin
      return Self.Errant_Field;
   end Get_Errant_Field_Number;

   --------------------------------
   -- Private
   --------------------------------

   ---------------------
   -- Private Utility
   ---------------------

   -- Updates the state of the engine
   procedure Set_State_Blocking (Self : in out Instance; New_State : in Seq_Runtime_State.E) is
   begin
      Self.State := New_State;
      Self.Blocked := True;
   end Set_State_Blocking;

   -- procedure Print_String (To_Print : in Seq_String.T) is
   -- begin
   --    for Char of To_Print loop
   --       exit when Char = Character'Val (0);
   --       -- Put (Seq_Runtime.Print_Output, Char);
   --       Put (Char);
   --    end loop;
   -- end Print_String;

   -- Gets a byte from the buffer assuming it is a valid opcode. If it is not a valid opcode it returns Invalid.
   function Get_Opcode_From_Memory (Self : in Instance) return Seq_Opcode.E is
      use System.Storage_Elements;
      Offset : constant Storage_Offset := Storage_Offset (Self.Position);
      This_Opcode : Packed_Seq_Opcode.T with Import, Convention => Ada, Address => Self.Sequence_Region.Address + Offset;
   begin
      -- Validate the opcode before returning it.
      if This_Opcode.Opcode'Valid then
         return This_Opcode.Opcode;
      else
         return Invalid;
      end if;
   end Get_Opcode_From_Memory;

   -- Performs bitwise or/and/xor on two signed packed records, returns signed packed record.
   function Bitwise_Ops (Left : in Packed_I32.T; Right : in Packed_I32.T; Code : in Seq_Operation.E) return Packed_I32.T is
      Op1 : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array (Packed_I32.Serialization.To_Byte_Array (Left));
      Op2 : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array (Packed_I32.Serialization.To_Byte_Array (Right));
      Ret : Packed_U32.T;
   begin
      case Code is
         when Bitwise_Or =>
            Ret := (Value => Op1.Value or Op2.Value);
         when Bitwise_And =>
            Ret := (Value => Op1.Value and Op2.Value);
         when Bitwise_Xor =>
            Ret := (Value => Op1.Value xor Op2.Value);
         when others =>
            -- This never gets called, please never call this
            pragma Assert (False);
      end case;
      return Packed_I32.Serialization.From_Byte_Array (Packed_U32.Serialization.To_Byte_Array (Ret));
   end Bitwise_Ops;

   -- Validates that a jump in sequence position is to a valid location in the sequence
   function Validate_Jump (Self : in Instance; Jump_Dest : in Interfaces.Unsigned_16) return Seq_Status is
   begin
      -- There is always a zeroed byte at the end of a sequence, and always a goto that jumps to it to end the sequence
      --   this means a jump to the length is allowed.
      if Seq_Position (Jump_Dest) <= Seq_Position (Self.Seq_Header.Length) then
         return Success;
      else
         return Failure;
      end if;
   end Validate_Jump;

   -- Returns an instruction as a byte array, this is a generic that requires an instruction type
   function Get_Instruction (Inst : in out Instance; Instruction : out T) return Seq_Status is
      use System.Storage_Elements;
      Offset : constant Storage_Offset := Storage_Offset (Inst.Position);
      To_Return : T with Import, Convention => Ada, Address => Inst.Sequence_Region.Address + Offset;
      Errant : Interfaces.Unsigned_32 := 0;
   begin
      Instruction := To_Return;

      -- Can fail if one of the fields is constrained (i.e. enum, range smaller than 0-255)
      if Valid (Instruction, Errant) then
         Inst.Next_Position := Seq_Position (T'Object_Size / Basic_Types.Byte'Object_Size) + Inst.Position;
         return Success;
      else
         Inst.Errant_Field := Errant;
         return Failure;
      end if;
   end Get_Instruction;

   function Get_Internal (Inst : in out Instance; Src : in Seq_Internal.E; Dest : out T) return Seq_Status is
      -- Overlay the internal variable with the generic variable type.
      pragma Warnings (Off, "overlay changes scalar storage order");
      This_Internal : T with Import, Convention => Ada, Address => Inst.Internals (Seq_Internal.E'Pos (Src))'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
      Errant : Interfaces.Unsigned_32;
   begin
      -- Make sure that the object can fit into a sequence runtime variable and that it is valid.
      -- This is invalid if we need to read into a packed record with a constrained field (i.e. enum)
      if T'Object_Size <= Packed_Poly_32_Type.T'Object_Size and then
          Valid (This_Internal, Errant)
      then
         Dest := This_Internal;
         return Success;
      else
         Inst.Errant_Field := Errant;
         return Failure;
      end if;
   end Get_Internal;

   function Set_Internal (Inst : in out Instance; Source : in Seq_Internal.E; New_Value : in T) return Seq_Status is
      Value_As_Seq_Var : Packed_Poly_32_Type.T with Import, Convention => Ada, Address => New_Value'Address;
   begin
      -- If the size of the object is larger than the size 4 bytes (the size of a seq variable)
      -- This can only occur if we write bad code, can this check be removed?
      if T'Object_Size / Basic_Types.Byte'Object_Size /= Packed_Poly_32_Type.T'Object_Size / Basic_Types.Byte'Object_Size then
         return Failure;
      end if;
      Inst.Internals (Seq_Internal.E'Pos (Source)) := Value_As_Seq_Var;
      return Success;
   end Set_Internal;

   -------------------------
   -- Instruction Utility
   -------------------------

   procedure Parse_Sequence_Header (Self : in out Instance) is
      function Get_Seq_Header is new Get_Instruction (Sequence_Header.T, Sequence_Header.Validation.Valid);
   begin
      if Get_Seq_Header (Inst => Self, Instruction => Self.Seq_Header) = Failure then
         -- This is currently unreachable code since the sequence header validation function is always true.
         -- However, the sequence header, unlike many of the instructions could change to include a ranged
         -- type. So, even though this is unreachable, I think leaving the process_error here is wise, instead
         -- of forcing an assertion as we do for many of the instructions that cannot be invalid.
         Self.Position := Self.Process_Error (Parse);
      else
         Self.Position := Self.Next_Position;
      end if;
   end Parse_Sequence_Header;

   function Parse_Instruction (Self : in out Instance) return Seq_Position is
      This_Opcode : constant Seq_Opcode.E := Self.Get_Opcode_From_Memory;
   begin
      case This_Opcode is
         when Set_Bit_Pattern =>
            return Self.Cmd_Set_Bit_Pattern;
         when Send_Bit_Pattern =>
            return Self.Cmd_Send_Bit_Pattern;
         when Update_Bit_Pattern =>
            return Self.Cmd_Update_Bit_Pattern;
         when Call =>
            return Self.Cmd_Call;
         when Spawn =>
            return Self.Cmd_Spawn;
         when Start =>
            return Self.Cmd_Start;
         when Push =>
            return Self.Cmd_Push;
         when Eval =>
            return Self.Cmd_Eval;
         when Fetch_Var_A =>
            return Self.Cmd_Fetch_Var (Seq_Internal.A);
         when Fetch_Var_B =>
            return Self.Cmd_Fetch_Var (Seq_Internal.B);
         when Store_Var =>
            return Self.Cmd_Store_Var;
         when Fetch_Tlm_A =>
            return Self.Cmd_Fetch_Tlm (Seq_Internal.A);
         -- Same logic as Fetch_Tlm_A, just to internal B instead of A.
         -- This instruction is not coverable currently because the sequence compiler uses B
         -- very sparingly. There is no case where telemetry is fetched into variable B right
         -- now.
         when Fetch_Tlm_B =>
            return Self.Cmd_Fetch_Tlm (Seq_Internal.B);
         when Invalid =>
            return Self.Process_Error (Opcode);
         when Wait =>
            return Self.Cmd_Wait;
         when Seq_Goto =>
            return Self.Cmd_Goto;
         when Jump_If_Zero =>
            return Self.Cmd_Jump_If_Zero;
         when Jump_Not_Zero =>
            return Self.Cmd_Jump_Not_Zero;
         when Jump_If_Equal =>
            return Self.Cmd_Jump_If_Equal;
         -- This is not coverable currently because the sequencer has no statements that compile
         -- this instruction in. Adamant does support it however.
         when Jump_Not_Equal =>
            return Self.Cmd_Jump_Not_Equal;
         when Seq_Return =>
            return Self.Cmd_Return;
         when Wait_If_Zero =>
            return Self.Cmd_Wait_If_Zero;
         -- This instruction is NOT used in adamant, so is skipped.
         when Subscribe =>
            return Self.Position + 4;
         -- This instruction is NOT used in adamant, so is skipped.
         when Unsubscribe =>
            return Self.Position + 4;
         when Eval_Flt =>
            return Self.Cmd_Eval_Flt;
         -- This instruction is currently disallowed in the Adamant compiler,
         -- so it is not coverable. The instruction is implemented however, and this
         -- feature can be supported by simply allowing this instruction in the compiler.
         when Cast_F_To_U =>
            return Self.Cmd_Cast_F_To_U;
         when Cast_U_To_F =>
            return Self.Cmd_Cast_U_To_F;
         when Eval_S =>
            return Self.Cmd_Eval_S;
         when Cast_S_To_U =>
            return Self.Cmd_Cast_S_To_U;
         when Cast_U_To_S =>
            return Self.Cmd_Cast_U_To_S;
         -- This instruction is currently disallowed in the Adamant compiler,
         -- so it is not coverable. The instruction is implemented however, and this
         -- feature can be supported by simply allowing this instruction in the compiler.
         when Cast_F_To_S =>
            return Self.Cmd_Cast_F_To_S;
         when Cast_S_To_F =>
            return Self.Cmd_Cast_S_To_F;
         when Wait_On_B =>
            return Self.Cmd_Wait_On_B;
         when Wait_If_Zero_On_B =>
            return Self.Cmd_Wait_If_Zero_On_B;
         -- This instruction is not implemented by Adamant and will result in a noop since
         -- Adamant does not support string pools.
         when Str_Alloc =>
            return Self.Cmd_Str_Alloc;
         when Print =>
            return Self.Cmd_Print;
         when Print_Var =>
            return Self.Cmd_Print_Var;
         when Kill_Engine =>
            return Self.Cmd_Kill_Engine;
         when Str_Dealloc | Str_Update_Bit_Pattern | Str_Copy | Str_Move | Str_Set | Kill_Name | Kill_Category | Print_Str =>
            return Self.Process_Error (Unimplemented);
      end case;
   end Parse_Instruction;

   -------------------------
   -- Seq Generics
   -------------------------

   function Get_Set_Bit_Pattern is new Get_Instruction (Set_Bit_Record.T, Set_Bit_Record.Validation.Valid);
   function Get_Update_Bit_Pattern is new Get_Instruction (Update_Bit_Record.T, Update_Bit_Record.Validation.Valid);
   function Get_Load_Instruction is new Get_Instruction (Load_New_Seq_Record.T, Load_New_Seq_Record.Validation.Valid);
   function Get_Push is new Get_Instruction (Push_Record.T, Push_Record.Validation.Valid);
   function Get_Eval is new Get_Instruction (Eval_Record.T, Eval_Record.Validation.Valid);
   function Get_Fetch is new Get_Instruction (Fetch_Var_Record.T, Fetch_Var_Record.Validation.Valid);
   function Get_Store is new Get_Instruction (Store_Var_Record.T, Store_Var_Record.Validation.Valid);
   function Get_Fetch_Tlm is new Get_Instruction (Fetch_Tlm_Record.T, Fetch_Tlm_Record.Validation.Valid);
   function Get_Wait is new Get_Instruction (Wait_Record.T, Wait_Record.Validation.Valid);
   function Get_Goto is new Get_Instruction (Goto_Record.T, Goto_Record.Validation.Valid);
   function Get_Jump_Zero is new Get_Instruction (Jump_Zero_Record.T, Jump_Zero_Record.Validation.Valid);
   function Get_Jump_Equal is new Get_Instruction (Jump_Equal_Record.T, Jump_Equal_Record.Validation.Valid);
   function Get_Kill_Engine is new Get_Instruction (Kill_Eng_Record.T, Kill_Eng_Record.Validation.Valid);
   function Get_Wait_If_Zero is new Get_Instruction (Wait_If_Zero_Record.T, Wait_If_Zero_Record.Validation.Valid);
   function Get_Cast is new Get_Instruction (Cast_Record.T, Cast_Record.Validation.Valid);
   function Get_Wait_On_B is new Get_Instruction (Wait_On_B_Record.T, Wait_On_B_Record.Validation.Valid);
   function Get_Wait_If_Zero_On_B is new Get_Instruction (Wait_If_Zero_On_B_Record.T, Wait_If_Zero_On_B_Record.Validation.Valid);

   function Get_Str_Alloc is new Get_Instruction (Str_Alloc_Record.T, Str_Alloc_Record.Validation.Valid);
   -- function Get_Str_Set is new Get_Instruction (Str_Set_Record.T, Str_Set_Record.Validation.Valid);
   function Get_Print is new Get_Instruction (Print_Record.T, Print_Record.Validation.Valid);
   -- function Get_Print_Str is new Get_Instruction (Print_Str_Record.T, Print_Str_Record.Validation.Valid);
   function Get_Print_Var is new Get_Instruction (Print_Var_Record.T, Print_Var_Record.Validation.Valid);

   pragma Warnings (Off, "overlay changes scalar storage order");
   function Get_Internal_Svar is new Get_Internal (Packed_Poly_32_Type.T, Packed_Poly_32_Type.Validation.Valid);
   function Get_Internal_U32 is new Get_Internal (Packed_U32.T, Packed_U32.Validation.Valid);
   function Get_Internal_F32 is new Get_Internal (Packed_F32.T, Packed_F32.Validation.Valid);
   function Get_Internal_I32 is new Get_Internal (Packed_I32.T, Packed_I32.Validation.Valid);
   function Get_Internal_Natural_32 is new Get_Internal (Packed_Natural_32.T, Packed_Natural_32.Validation.Valid);
   pragma Warnings (On, "overlay changes scalar storage order");

   function Set_Internal_Svar is new Set_Internal (Packed_Poly_32_Type.T);
   function Set_Internal_U32 is new Set_Internal (Packed_U32.T);
   function Set_Internal_F32 is new Set_Internal (Packed_F32.T);
   function Set_Internal_I32 is new Set_Internal (Packed_I32.T);
   function Set_Internal_Natural_32 is new Set_Internal (Packed_Natural_32.T);

   -- The following utility procedures use the generic functions described above, so these are put here
   procedure Set_Timeout (Self : in out Instance) is
      Status : constant Seq_Status := Set_Internal_U32 (Self, Seq_Internal.Timeout, (Value => 1));
   begin
      Self.State := Timeout;
      pragma Assert (Status = Success);
   end Set_Timeout;

   procedure Reset_Timeout (Self : in out Instance) is
      Status : constant Seq_Status := Set_Internal_U32 (Self, Seq_Internal.Timeout, (Value => 0));
   begin
      pragma Assert (Status = Success);
   end Reset_Timeout;

   function Wait_On_Helper (Self : in out Instance; Wait_Type : in Seq_Wait_Type.E; Wait_Amount : in Interfaces.Unsigned_32; Jump_Position : in Interfaces.Unsigned_16) return Seq_Position is
      To_Check : Packed_U32.T;
      Status : constant Seq_Status := Get_Internal_U32 (Self, Seq_Internal.A, To_Check);
   begin
      pragma Assert (Status = Success);

      -- If the telemetry was set, and the it meets the criteria, move on in the sequence with READY
      if Self.State = Telemetry_Set and then To_Check.Value /= 0 then
         Self.State := Ready;
         Self.Timeout_Set := False;
         return Self.Next_Position;
      end if;

      -- If we have not setup a timeout
      if Self.Timeout_Set = False then
         Self.Reset_Timeout; -- Reset internal timeout flag
         Self.Telemetry_Timeout.Seconds := Wait_Amount;

         -- Save off most recent execution time as the start telemetry wait time
         Self.Telemetry_Wait_Start_Time := Self.Most_Recent_Execution_Time;

         if Self.Validate_Jump (Jump_Position) = Failure then
            return Self.Process_Error (Jump);
         end if;

         case Wait_Type is
            when Relative =>
               Self.Set_State_Blocking (Wait_Telemetry_Relative);
            when Absolute =>
               if Self.Telemetry_Timeout.Seconds <= Self.Most_Recent_Execution_Time.Seconds then
                  return Self.Process_Error (Wait);
               end if;
               Self.Set_State_Blocking (Wait_Telemetry_Value);
         end case;

         Self.Timeout_Set := True;
         return Seq_Position (Jump_Position);
      else
         if Self.Most_Recent_Execution_Time.Seconds >= Self.Telemetry_Timeout.Seconds then
            -- Timeout should occur
            Self.Set_Timeout; -- Set internal timeout flag
            Self.Timeout_Set := False;
            return Self.Next_Position;
         else
            -- Timeout should not occur
            Self.Set_State_Blocking (Wait_Telemetry_Value);
            return Seq_Position (Jump_Position);
         end if;
      end if;
   end Wait_On_Helper;

   function Wait_Helper (Self : in out Instance; Wait_Type : in Seq_Wait_Type.E; Wait_Amount : in Interfaces.Unsigned_32) return Seq_Position is
   begin
      Self.Wake_Time.Seconds := Wait_Amount;

      case Wait_Type is
         when Relative =>
            if Wait_Amount = 0 then
               -- Handles case for relative time being 0... does not block, just continues on
               return Self.Next_Position;
            end if;
            Self.Set_State_Blocking (Wait_Relative);
         when Absolute =>
            if Self.Wake_Time.Seconds <= Self.Most_Recent_Execution_Time.Seconds then
               return Self.Process_Error (Wait);
            end if;
            Self.Set_State_Blocking (Wait_Absolute);
      end case;
      return Self.Next_Position;
   end Wait_Helper;

   function Fetch_Var_Helper (Self : in out Instance; Var_Info : in Var_Record.T; Var : out Packed_Poly_32_Type.T) return Fetch_Status is
      Ignore : Seq_Position;
   begin
      -- Initialize out parameter:
      Var := (Value => [others => 0]);

      -- Set the source to either in sequence data or local var data
      case Var_Info.Var_Type is
         when In_Sequence =>
            Var := (Value => Packed_U32.Serialization.To_Byte_Array ((Value => Var_Info.Id)));
            return Success;
         when Local =>
            -- Move the variable in the locals map into the correct internal
            if Var_Info.Id <= Interfaces.Unsigned_32 (Seq_Local_Id'Last) then
               Var := Self.Local_Variable_Array (Seq_Local_Id (Var_Info.Id));
               return Success;
            else
               Ignore := Self.Process_Error (Variable);
               return Failure;
            end if;
         when Internal =>
            -- Move the value in the encoded internal into the destination internal
            if Var_Info.Id <= 3 then
               declare
                  Get_Status : constant Seq_Status := Get_Internal_Svar (Self, Seq_Internal.E'Val (Var_Info.Id), Var);
               begin
                  pragma Assert (Get_Status = Success);
                  return Success;
               end;
            else
               Ignore := Self.Process_Error (Variable);
               return Failure;
            end if;
         when Global =>
            Ignore := Self.Process_Error (Unimplemented);
            return Failure;
      end case;
   end Fetch_Var_Helper;

   -------------------------
   -- Seq Instructions
   -------------------------

   -- opcode 0 | Set Bit Pattern | U8 - U8 - U16 | Opcode - Pad - Length
   -- Reads a command bit pattern and stores it as type Command.T in the engine instance
   function Cmd_Set_Bit_Pattern (Self : in out Instance) return Seq_Position is
      Instruction : Set_Bit_Record.T;
      Status : constant Seq_Status := Get_Set_Bit_Pattern (Self, Instruction);

      use System.Storage_Elements;
      Bytes : Basic_Types.Byte_Array (0 .. Natural (Instruction.Length) - 1)
         with Import, Convention => Ada, Address => Self.Sequence_Region.Address + Storage_Offset (Self.Next_Position);
      Bytes_Serialized : Natural;
      Command_Serialization_Status : constant Serialization_Status := Command.Serialization.From_Byte_Array (Self.Bit_Pattern, Bytes, Bytes_Serialized);
   begin
      pragma Assert (Status = Success);

      -- Command Serialization Failure
      if Command_Serialization_Status /= Success or else Bytes_Serialized /= Natural (Instruction.Length) then
         return Self.Process_Error (Command_Parse);
      end if;

      Self.Next_Position := @ + Seq_Position (Bytes_Serialized);

      -- Read off the end of the sequence
      if Self.Next_Position > Seq_Position (Self.Seq_Header.Length) then
         return Self.Process_Error (Command_Length);
      end if;

      return Self.Next_Position;
   end Cmd_Set_Bit_Pattern;

   -- opcode 1 | Send Bit Pattern | U8 - U8 - U8 - U8 | Opcode - Pad - Pad - Pad
   -- Sets the engine state to WAIT_COMMAND. The engine caller should check for this state and ask the engine for the command.
   function Cmd_Send_Bit_Pattern (Self : in out Instance) return Seq_Position is
   begin
      Self.Set_State_Blocking (Wait_Command);
      return Self.Position + Seq_Position (Send_Bit_Record.Size_In_Bytes);
   end Cmd_Send_Bit_Pattern;

   -- opcode 2 | Update Bit Pattern | U8 - U8 - U16 - U32 | Opcode - Pad - Offset - Length
   -- Sets parameters in a command argument buffer after set bit pattern is called.
   function Cmd_Update_Bit_Pattern (Self : in out Instance) return Seq_Position is
      Instruction : Update_Bit_Record.T;
      Internal_A : Packed_Poly_32_Type.T;
      Read_Status : constant Seq_Status := Get_Internal_Svar (Self, Seq_Internal.A, Internal_A);
   begin
      pragma Assert (Read_Status = Success);
      if Get_Update_Bit_Pattern (Self, Instruction) = Failure then
         return Self.Process_Error (Update_Bit_Pattern);
      end if;

      declare
         -- Set the argument in the command buffer:
         Set_Status : constant Set_Poly_Type_Status := Set_Poly_Type (
            Dest => Self.Bit_Pattern.Arg_Buffer,
            Offset => Instruction.Offset - Command_Header.Size, -- This must be >= 0 due to type of .Offset
            Size => Instruction.Length, -- This must be >= 1 due to type of .Size
            Value => Internal_A.Value,
            -- We have to enabled truncation for signed types. A negative 32-bit integer needs to be
            -- truncated in order to properly get turned into a negatie 8-bit integer for instance.
            Truncation_Allowed => True
         );
      begin
         case Set_Status is
            when Success => null;
            when Error => return Self.Process_Error (Command_Argument);
            -- Truncation is allowed, this should never be returned
            when Truncation_Error => pragma Assert (False);
         end case;
      end;

      return Self.Next_Position;
   end Cmd_Update_Bit_Pattern;

   -- opcode 3 | Call | U8 - U8 - U16 | Opcode - Pad - Id
   -- Calls a new subsequence and loads it on the engine. Will return back to this sequence.
   function Cmd_Call (Self : in out Instance) return Seq_Position is
      Instruction : Load_New_Seq_Record.T;
      Parse_Status : constant Seq_Status := Get_Load_Instruction (Self, Instruction);
   begin
      pragma Assert (Parse_Status = Success);
      Self.Set_State_Blocking (Wait_Load_New_Sub_Seq);
      Self.Seq_Id_To_Load := Instruction.Id;
      return Self.Next_Position;
   end Cmd_Call;

   -- opcode 4 | Spawn |   U8 - U8 - U16 | Opcode - Engine - Id
   -- Starts a new sequence in another engine, this sequence will not pause necessarily.
   function Cmd_Spawn (Self : in out Instance) return Seq_Position is
      Instruction : Load_New_Seq_Record.T;
      Parse_Status : constant Seq_Status := Get_Load_Instruction (Self, Instruction);
   begin
      pragma Assert (Parse_Status = Success);
      Self.Set_State_Blocking (Wait_Load_New_Seq_Elsewhere);
      Self.Seq_Id_To_Load := Instruction.Id;
      Self.Spawn_Destination := Instruction.Engine;
      return Self.Next_Position;
   end Cmd_Spawn;

   -- opcode 5 | Start |   U8 - U8 - U16 | Opcode - Pad - Id
   -- Overwrites the current running sequence.
   function Cmd_Start (Self : in out Instance) return Seq_Position is
      Instruction : Load_New_Seq_Record.T;
      Parse_Status : constant Seq_Status := Get_Load_Instruction (Self, Instruction);
   begin
      pragma Assert (Parse_Status = Success);
      Self.Set_State_Blocking (Wait_Load_New_Seq_Overwrite);
      Self.Seq_Id_To_Load := Instruction.Id;
      return Self.Next_Position;
   end Cmd_Start;

   -- opcode 6 | Push | U8 - U8 - U8 - U8 | Opcode - Engine - Destination - Pad
   -- Pushes the value in internal variable A to a specified engines argument array at the index corresponding with the destination field.
   function Cmd_Push (Self : in out Instance) return Seq_Position is
      Instruction : Push_Record.T;
      Argument : Packed_Poly_32_Type.T;
      Get_Internal_Status : constant Seq_Status := Get_Internal_Svar (Self, Seq_Internal.A, Argument);
   begin
      pragma Assert (Get_Internal_Status = Success);

      -- Can fail while reading destination, but error would be a miss-index
      if Get_Push (Self, Instruction) = Failure then
         return Self.Process_Error (Variable);
      end if;

      Self.Out_Arguments (Instruction.Destination) := Argument;
      return Self.Next_Position;
   end Cmd_Push;

   -- opcode 7 | Eval | U8 - E8 - U8 - U8 | Opcode - Op - Pad - Pad
   -- Evaluates unsigned value operands from internal A and internal B
   function Cmd_Eval (Self : in out Instance) return Seq_Position is
      Left : Packed_Natural_32.T;
      Right : Packed_Natural_32.T;
      Result : Packed_Natural_32.T;
      Instruction : Eval_Record.T;
      Left_Status : constant Seq_Status := Get_Internal_Natural_32 (Self, Seq_Internal.A, Left);
      Right_Status : constant Seq_Status := Get_Internal_Natural_32 (Self, Seq_Internal.B, Right);
      Set_Status : Seq_Status;
   begin
      pragma Assert (Left_Status = Success);
      pragma Assert (Right_Status = Success);

      -- Can fail on Op field
      if Get_Eval (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Perform operation
      -- Note: These are free to throw exceptions, will get caught at bottom of function. This enforces Ada behaviour of these operations.
      case Instruction.Operation is
         when Addition =>
            Result.Value := Left.Value + Right.Value;
         when Subtraction =>
            Result.Value := Left.Value - Right.Value;
         when Multiplication =>
            Result.Value := Left.Value * Right.Value;
         when Division =>
            Result.Value := Left.Value / Right.Value;
         when Modulus =>
            Result.Value := Left.Value mod Right.Value;
         when Bitwise_Or =>
            Result.Value := Natural_32 (Interfaces.Unsigned_32 (Left.Value) or Interfaces.Unsigned_32 (Right.Value));
         when Bitwise_And =>
            Result.Value := Natural_32 (Interfaces.Unsigned_32 (Left.Value) and Interfaces.Unsigned_32 (Right.Value));
         when Bitwise_Xor =>
            Result.Value := Natural_32 (Interfaces.Unsigned_32 (Left.Value) xor Interfaces.Unsigned_32 (Right.Value));
         when Logical_And =>
            Result.Value := (if Left.Value /= 0 and then Right.Value /= 0 then 1 else 0);
         when Equals =>
            Result.Value := (if Left.Value = Right.Value then 1 else 0);
         when Not_Equals =>
            Result.Value := (if Left.Value /= Right.Value then 1 else 0);
         when Logical_Or =>
            Result.Value := (if Left.Value = 0 and then Right.Value = 0 then 0 else 1);
         when Greater_Than =>
            Result.Value := (if Left.Value > Right.Value then 1 else 0);
         when Less_Than =>
            Result.Value := (if Left.Value < Right.Value then 1 else 0);
         when Less_Than_Equal =>
            Result.Value := (if Left.Value <= Right.Value then 1 else 0);
         when Greater_Than_Equal =>
            Result.Value := (if Left.Value >= Right.Value then 1 else 0);
      end case;

      -- Update Internal Value
      Set_Status := Set_Internal_Natural_32 (Self, Seq_Internal.A, Result);
      pragma Assert (Set_Status = Success);

      return Self.Next_Position;
   exception
      when others =>
         return Self.Process_Error (Eval);
   end Cmd_Eval;

   -- opcode 8 and 9| Fetch Var A and B | U8 - U8 - U8 - U8 - Struct | Opcode - Pad - Pad - Pad - varInfo_T
   -- Stores the encoded variable in local var A or B (either local or constant)
   function Cmd_Fetch_Var (Self : in out Instance; Internal_Dst : in Seq_Internal.E) return Seq_Position is
      Instruction : Fetch_Var_Record.T;
      Seq_Var : Packed_Poly_32_Type.T;
      Set_Status : Seq_Status;
   begin
      if Get_Fetch (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Get the variable:
      case Self.Fetch_Var_Helper (Var_Info => Instruction.Var_Info, Var => Seq_Var) is
         when Success =>
            -- Set the correct internal with the result of the fetch.
            Set_Status := Set_Internal_Svar (Self, Internal_Dst, Seq_Var);
            pragma Assert (Set_Status = Success);
            return Self.Next_Position;
         when Failure =>
            -- Just return the current position. Errors have already been handled.
            return Self.Position;
      end case;
   end Cmd_Fetch_Var;

   -- opcode 10 | Store Var | U8 - U8 - U8 - U8 - Struct | Opcode - Pad - Pad - Pad - varInfo_T
   -- Stores the bit pattern from internal var A into the local variable array at the index corresponding with the variables ID
   function Cmd_Store_Var (Self : in out Instance) return Seq_Position is
      To_Store : Packed_Poly_32_Type.T;
      Instruction : Store_Var_Record.T;
      Get_Status : constant Seq_Status := Get_Internal_Svar (Self, Seq_Internal.A, To_Store);
      Set_Status : Seq_Status; -- Might be used if store var is internal
   begin
      pragma Assert (Get_Status = Success);

      if Get_Store (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      case Instruction.Var_Info.Var_Type is
         when Local =>
            -- Set the local var array
            if Instruction.Var_Info.Id <= Interfaces.Unsigned_32 (Seq_Local_Id'Last) then
               Self.Local_Variable_Array (Seq_Local_Id (Instruction.Var_Info.Id)) := To_Store;
            else
               return Self.Process_Error (Variable);
            end if;
         when Internal =>
            -- Set the correct internal
            if Instruction.Var_Info.Id <= 3 then
               Set_Status := Set_Internal_Svar (Self, Seq_Internal.E'Val (Instruction.Var_Info.Id), To_Store);
               pragma Assert (Set_Status = Success);
            else
               return Self.Process_Error (Variable);
            end if;
         when Global | In_Sequence =>
            return Self.Process_Error (Unimplemented);
      end case;
      return Self.Next_Position;
   end Cmd_Store_Var;

   -- opcode 11/12 | Fetch_TLM_A/B | U8 - U8 - U8 - E8 - Struct | Opcode - Pad - Pad - WaitOn - tlmInfo_T
   function Cmd_Fetch_Tlm (Self : in out Instance; Internal_Dst : in Seq_Internal.E) return Seq_Position is
      Instruction : Fetch_Tlm_Record.T;
   begin
      -- Can fail on isNewRequired and waiton
      if Get_Fetch_Tlm (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Store telemetry metadata
      Self.Telemetry_Request := Instruction.Tlm_Info;
      Self.Telemetry_Destination := Internal_Dst;

      if Instruction.Waiton = False then
         -- This should already be false, but just in case it isn't, override it and set it to false (else component could loop infinitely)
         Self.Telemetry_Request.New_Value_Required := False;
         Self.Set_State_Blocking (Wait_Telemetry_Set);
      end if;

      return Self.Next_Position;
   end Cmd_Fetch_Tlm;

   -- opcode 14 | Wait | U8 - U8 - U8 - U8 - U32 | Opcode - wait_type - Pad - Pad - wait_time
   -- Sets the engine state to the current wait type and tells the engine how long it must wait for.
   function Cmd_Wait (Self : in out Instance) return Seq_Position is
      Instruction : Wait_Record.T;
   begin

      -- Can fail on wait_type
      if Get_Wait (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      return Self.Wait_Helper (Instruction.Wait_Type, Instruction.Wait_Time);
   end Cmd_Wait;

   -- opcode 15 | Goto | U8 - U8 - U16 | Opcode - Pad - Position
   -- Jumps to a new position in the sequence buffer.
   function Cmd_Goto (Self : in out Instance) return Seq_Position is
      Instruction : Goto_Record.T;
      Parse_Status : constant Seq_Status := Get_Goto (Self, Instruction);
   begin
      pragma Assert (Parse_Status = Success);

      if Self.Validate_Jump (Instruction.Position) = Failure then
         return Self.Process_Error (Jump);
      end if;
      return Seq_Position (Instruction.Position);
   end Cmd_Goto;

   -- opcode 16 | Jump If Zero | U8 - U8 - U16 | Opcode - Pad - Position
   -- Jumps to the given position if internal A is equal to 0.
   function Cmd_Jump_If_Zero (Self : in out Instance) return Seq_Position is
      Instruction : Jump_Zero_Record.T;
      To_Check : Packed_U32.T;
      Instr_Status : constant Seq_Status := Get_Jump_Zero (Self, Instruction);
      Get_Status : constant Seq_Status := Get_Internal_U32 (Self, Seq_Internal.A, To_Check);
   begin
      pragma Assert (Instr_Status = Success);
      pragma Assert (Get_Status = Success);

      -- Check if the encoded jump is valid
      if Self.Validate_Jump (Instruction.Position) = Failure then
         return Self.Process_Error (Jump);
      end if;

      -- Check if we should perform the jump
      if To_Check.Value = 0 then
         return Seq_Position (Instruction.Position); -- This cast cannot fail because instruction.Position is at most 16 bits
      else
         return Self.Next_Position;
      end if;
   end Cmd_Jump_If_Zero;

   -- opcode 17 | Jump Not Zero | U8 - U8 - U16 | Opcode - Pad - Position
   -- Jumps to the given position if internal A does not equal 0.
   function Cmd_Jump_Not_Zero (Self : in out Instance) return Seq_Position is
      Instruction : Jump_Zero_Record.T;
      To_Check : Packed_U32.T;
      Instr_Status : constant Seq_Status := Get_Jump_Zero (Self, Instruction);
      Get_Status : constant Seq_Status := Get_Internal_U32 (Self, Seq_Internal.A, To_Check);
   begin
      pragma Assert (Instr_Status = Success);
      pragma Assert (Get_Status = Success);

      -- Check if the encoded jump is valid
      if Self.Validate_Jump (Instruction.Position) = Failure then
         return Self.Process_Error (Jump);
      end if;

      -- Check if we should perform the jump
      if To_Check.Value /= 0 then
         return Seq_Position (Instruction.Position); -- This cast cannot fail because instruction.Position is at most 16 bits
      else
         return Self.Next_Position;
      end if;
   end Cmd_Jump_Not_Zero;

   -- opcode 18 | Jump If Equal | U8 - U8 - U16 - U32 | Opcode - Pad - Position - Value
   -- If the stored value and the value from internal var A are equal this will perform an absolute jump.
   function Cmd_Jump_If_Equal (Self : in out Instance) return Seq_Position is
      Instruction : Jump_Equal_Record.T;
      To_Check : Packed_U32.T;
      Instr_Status : constant Seq_Status := Get_Jump_Equal (Self, Instruction);
      Get_Status : constant Seq_Status := Get_Internal_U32 (Self, Seq_Internal.A, To_Check);
   begin
      pragma Assert (Instr_Status = Success);
      pragma Assert (Get_Status = Success);

      -- Check if the encoded jump is valid
      if Self.Validate_Jump (Instruction.Position) = Failure then
         return Self.Process_Error (Jump);
      end if;

      if To_Check.Value = Instruction.To_Compare then
         return Seq_Position (Instruction.Position);
      else
         return Self.Next_Position;
      end if;
   end Cmd_Jump_If_Equal;

   -- opcode 19 | Jump Not Equal | U8 - U8 - U16 - U32 | Opcode - Pad - Position - Value
   -- If the stored value and the value from internal var A are NOT equal this will perform an absolute jump.
   function Cmd_Jump_Not_Equal (Self : in out Instance) return Seq_Position is
      Instruction : Jump_Equal_Record.T;
      To_Check : Packed_U32.T;
      Instr_Status : constant Seq_Status := Get_Jump_Equal (Self, Instruction);
      Get_Status : constant Seq_Status := Get_Internal_U32 (Self, Seq_Internal.A, To_Check);
   begin
      pragma Assert (Instr_Status = Success);
      pragma Assert (Get_Status = Success);

      -- Check if the encoded jump is valid
      if Self.Validate_Jump (Instruction.Position) = Failure then
         return Self.Process_Error (Jump);
      end if;

      if To_Check.Value /= Instruction.To_Compare then
         return Seq_Position (Instruction.Position);
      else
         return Self.Next_Position;
      end if;
   end Cmd_Jump_Not_Equal;

   -- opcode 20 | Return | U8 - U8 - U8 - U8 | Opcode - Pad - Pad - Pad
   -- Sets the state to return, this is essentially a termination of this sequence, and is handled by the engine.
   function Cmd_Return (Self : in out Instance) return Seq_Position is
   begin
      Self.Set_State_Blocking (Done);
      return Self.Position;
   end Cmd_Return;

   -- opcode 21 | Wait If Zero | U8 - E8 - U16 - U32 | Opcode - waitType - Position - Timeout
   -- Waits if internal A is zero
   function Cmd_Wait_If_Zero (Self : in out Instance) return Seq_Position is
      Instruction : Wait_If_Zero_Record.T;
   begin
      -- Can fail on wait_type
      if Get_Wait_If_Zero (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      return Self.Wait_On_Helper (Instruction.Wait_Type, Instruction.Timeout, Instruction.Position);
   end Cmd_Wait_If_Zero;

   -- Opcode 23 | Kill_Engine | U8 - U8 - U8 - U8 | Opcode - Engine_Start - Num_To_Kill - Pad
   -- Kills a range of engines starting from Engine_Start.
   function Cmd_Kill_Engine (Self : in out Instance) return Seq_Position is
      Instruction : Kill_Eng_Record.T;
   begin
      -- This could fail if the type that defines the number of engines ever has a range smaller than U8
      if Get_Kill_Engine (Self, Instruction) = Failure then
         -- However, this is not reachable if the engine type is full range U8. Leaving this is wise though
         -- even if the code is not reachable, since it could reasonably change in the future.
         return Self.Process_Error (Parse);
      end if;

      Self.Set_State_Blocking (Kill_Engine);
      Self.Kill_Engine_Start := Instruction.Engine_Start;
      Self.Num_Eng_To_Kill := Instruction.Num_To_Kill;
      return Self.Next_Position;
   end Cmd_Kill_Engine;

   -- opcode 27 | Eval FLT | U8 - E8 - U8 - U8 | Opcode - Op - Pad - Pad
   -- Evaluates internal A and B as floating point values.
   function Cmd_Eval_Flt (Self : in out Instance) return Seq_Position is
      Left : Packed_F32.T;
      Right : Packed_F32.T;
      Result : Packed_F32.T;
      Instruction : Eval_Record.T;
      Set_Status : Seq_Status;
   begin
      if Get_Internal_F32 (Self, Seq_Internal.A, Left) = Failure or else Get_Internal_F32 (Self, Seq_Internal.B, Right) = Failure then
         return Self.Process_Error (Float_Value);
      end if;

      -- Can fail on Op field
      if Get_Eval (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Tolerance is expected to have been set in the sequence, compare at your own risk.
      case Instruction.Operation is
         when Addition =>
            Result.Value := Left.Value + Right.Value;
         when Subtraction =>
            Result.Value := Left.Value - Right.Value;
         when Multiplication =>
            Result.Value := Left.Value * Right.Value;
         when Division =>
            Result.Value := Left.Value / Right.Value;
         when Equals =>
            Result.Value := Short_Float (if Left.Value = Right.Value then 1 else 0);
         when Not_Equals =>
            Result.Value := Short_Float (if Left.Value /= Right.Value then 1 else 0);
         when Greater_Than =>
            Result.Value := Short_Float (if Left.Value > Right.Value then 1 else 0);
         when Less_Than =>
            Result.Value := Short_Float (if Left.Value < Right.Value then 1 else 0);
         when Less_Than_Equal =>
            Result.Value := Short_Float (if Left.Value <= Right.Value then 1 else 0);
         when Greater_Than_Equal =>
            Result.Value := Short_Float (if Left.Value >= Right.Value then 1 else 0);
         when Modulus | Bitwise_And | Bitwise_Xor | Bitwise_Or | Logical_Or | Logical_And =>
            return Self.Process_Error (Invalid_Op);
      end case;

      -- Update Internal Value
      Set_Status := Set_Internal_F32 (Self, Seq_Internal.A, Result);
      pragma Assert (Set_Status = Success);

      return Self.Next_Position;
   exception
      when others =>
         return Self.Process_Error (Eval);
   end Cmd_Eval_Flt;

   -- opcode 28 | Cast F to U | U8 - E8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Type casts either internal var
   function Cmd_Cast_F_To_U (Self : in out Instance) return Seq_Position is
      Instruction : Cast_Record.T;
      Src : Packed_F32.T;
      Dst : Packed_U32.T;
      Status : Seq_Status;
   begin
      -- Can fail on Id
      if Get_Cast (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Read Internal
      if Get_Internal_F32 (Self, Instruction.Id, Src) = Failure then
         return Self.Process_Error (Float_Value);
      end if;

      -- Perform the cast (this can fail)
      Dst.Value := Interfaces.Unsigned_32 (Src.Value);

      -- Write Internal
      Status := Set_Internal_U32 (Self, Instruction.Id, Dst);
      pragma Assert (Status = Success);

      return Self.Next_Position;
   exception
      when others =>
         return Self.Process_Error (Cast);
   end Cmd_Cast_F_To_U;

   -- opcode 29 | Cast U to F | U8 - U8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Type casts either internal var
   function Cmd_Cast_U_To_F (Self : in out Instance) return Seq_Position is
      Instruction : Cast_Record.T;
      Src : Packed_U32.T;
      Dst : Packed_F32.T;
      Status : Seq_Status;
   begin
      -- Can fail on Id
      if Get_Cast (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Read Internal
      Status := Get_Internal_U32 (Self, Instruction.Id, Src);
      pragma Assert (Status = Success);

      -- Perform the cast (this cannot fail)
      Dst.Value := Short_Float (Src.Value);

      -- Write Internal
      Status := Set_Internal_F32 (Self, Instruction.Id, Dst);
      pragma Assert (Status = Success);

      return Self.Next_Position;
   end Cmd_Cast_U_To_F;

   -- opcode 30 | Eval S | U8 - U8 - U8 - U8 | Opcode - Op - Pad - Pad
   -- Evaluates signed value operands from internal A and internal B
   function Cmd_Eval_S (Self : in out Instance) return Seq_Position is
      Left : Packed_I32.T;
      Right : Packed_I32.T;
      Result : Packed_I32.T;
      Instruction : Eval_Record.T;
      Left_Status : constant Seq_Status := Get_Internal_I32 (Self, Seq_Internal.A, Left);
      Right_Status : constant Seq_Status := Get_Internal_I32 (Self, Seq_Internal.B, Right);
      Set_Status : Seq_Status;
   begin
      pragma Assert (Left_Status = Success);
      pragma Assert (Right_Status = Success);

      -- Can fail on Op field
      if Get_Eval (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Perform operation
      case Instruction.Operation is
         when Addition =>
            Result.Value := Left.Value + Right.Value;
         when Subtraction =>
            Result.Value := Left.Value - Right.Value;
         when Multiplication =>
            Result.Value := Left.Value * Right.Value;
         when Division =>
            Result.Value := Left.Value / Right.Value;
         when Modulus =>
            Result.Value := Left.Value mod Right.Value;
         when Bitwise_Or | Bitwise_And | Bitwise_Xor =>
            Result := Bitwise_Ops (Left, Right, Instruction.Operation); -- This performs C style bitwise ops on signed types (not usually allowed in Ada)
         when Logical_And =>
            Result.Value := (if Left.Value /= 0 and then Right.Value /= 0 then 1 else 0);
         when Equals =>
            Result.Value := (if Left.Value = Right.Value then 1 else 0);
         when Not_Equals =>
            Result.Value := (if Left.Value /= Right.Value then 1 else 0);
         when Logical_Or =>
            Result.Value := (if Left.Value = 0 and then Right.Value = 0 then 0 else 1); -- This is the same as a (left || right) in C
         when Greater_Than =>
            Result.Value := (if Left.Value > Right.Value then 1 else 0);
         when Less_Than =>
            Result.Value := (if Left.Value < Right.Value then 1 else 0);
         when Less_Than_Equal =>
            Result.Value := (if Left.Value <= Right.Value then 1 else 0);
         when Greater_Than_Equal =>
            Result.Value := (if Left.Value >= Right.Value then 1 else 0);
      end case;

      -- Update Internal Value
      Set_Status := Set_Internal_I32 (Self, Seq_Internal.A, Result);
      pragma Assert (Set_Status = Success);

      return Self.Next_Position;
   exception
      when others =>
         return Self.Process_Error (Eval);
   end Cmd_Eval_S;

   -- opcode 31 | Cast S to U | U8 - U8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Type casts either internal var
   function Cmd_Cast_S_To_U (Self : in out Instance) return Seq_Position is
      Instruction : Cast_Record.T;
      Src : Packed_I32.T;
      Dst : Packed_U32.T;
      Status : Seq_Status;
   begin
      -- Can fail on Id
      if Get_Cast (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Read Internal
      Status := Get_Internal_I32 (Self, Instruction.Id, Src);
      pragma Assert (Status = Success);

      -- Perform the cast (this can fail)
      Dst.Value := Interfaces.Unsigned_32 (Src.Value);

      -- Write Internal
      Status := Set_Internal_U32 (Self, Instruction.Id, Dst);
      pragma Assert (Status = Success);

      return Self.Next_Position;
   exception
      when others =>
         return Self.Process_Error (Cast);
   end Cmd_Cast_S_To_U;

   -- opcode 32 | Cast U to S | U8 - U8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Type casts either internal var
   function Cmd_Cast_U_To_S (Self : in out Instance) return Seq_Position is
      Instruction : Cast_Record.T;
      Src : Packed_U32.T;
      Dst : Packed_I32.T;
      Status : Seq_Status;
   begin
      -- Can fail on Id
      if Get_Cast (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Read Internal
      Status := Get_Internal_U32 (Self, Instruction.Id, Src);
      pragma Assert (Status = Success);

      -- Perform the cast (this can fail)
      Dst.Value := Interfaces.Integer_32 (Src.Value);

      -- Write Internal
      Status := Set_Internal_I32 (Self, Instruction.Id, Dst);
      pragma Assert (Status = Success);

      return Self.Next_Position;
   exception
      when others =>
         return Self.Process_Error (Cast);
   end Cmd_Cast_U_To_S;

   -- opcode 33 | Cast F to S | U8 - U8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Type casts either internal var
   function Cmd_Cast_F_To_S (Self : in out Instance) return Seq_Position is
      Instruction : Cast_Record.T;
      Src : Packed_F32.T;
      Dst : Packed_I32.T;
      Status : Seq_Status;
   begin
      -- Can fail on Id
      if Get_Cast (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Read Internal
      if Get_Internal_F32 (Self, Instruction.Id, Src) = Failure then
         return Self.Process_Error (Float_Value);
      end if;

      -- Perform the cast (this can fail)
      Dst.Value := Interfaces.Integer_32 (Src.Value);

      -- Write Internal
      Status := Set_Internal_I32 (Self, Instruction.Id, Dst);
      pragma Assert (Status = Success);

      return Self.Next_Position;
   exception
      when others =>
         return Self.Process_Error (Cast);
   end Cmd_Cast_F_To_S;

   -- opcode 34 | Cast S to F | U8 - U8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Type casts either internal var
   function Cmd_Cast_S_To_F (Self : in out Instance) return Seq_Position is
      Instruction : Cast_Record.T;
      Src : Packed_I32.T;
      Dst : Packed_F32.T;
      Status : Seq_Status;
   begin
      -- Can fail on Id
      if Get_Cast (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Read Internal
      Status := Get_Internal_I32 (Self, Instruction.Id, Src);
      pragma Assert (Status = Success);

      -- Perform the cast (this cannot fail)
      Dst.Value := Short_Float (Src.Value);

      -- Write Internal
      Status := Set_Internal_F32 (Self, Instruction.Id, Dst);
      pragma Assert (Status = Success);

      return Self.Next_Position;
   end Cmd_Cast_S_To_F;

   -- opcode 35 | Wait On B | U8 - U8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Waits for the time in Internal B
   function Cmd_Wait_On_B (Self : in out Instance) return Seq_Position is
      Instruction : Wait_On_B_Record.T;
      Wait_Time : Packed_U32.T;
      Status : constant Seq_Status := Get_Internal_U32 (Self, Seq_Internal.B, Wait_Time);
   begin
      pragma Assert (Status = Success);

      -- Can fail on Id
      if Get_Wait_On_B (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      return Self.Wait_Helper (Instruction.Wait_Type, Wait_Time.Value);
   end Cmd_Wait_On_B;

   -- opcode 36 | Wait If Zero On B | U8 - E8 - U16 | Opcode - Wait Type - Position
   -- Waits for the time in Internal B if Internal A is zero
   function Cmd_Wait_If_Zero_On_B (Self : in out Instance) return Seq_Position is
      Instruction : Wait_If_Zero_On_B_Record.T;
      Timeout_Seconds : Packed_U32.T;
      Status : constant Seq_Status := Get_Internal_U32 (Self, Seq_Internal.B, Timeout_Seconds);
   begin
      pragma Assert (Status = Success);

      -- Can fail on wait_type
      if Get_Wait_If_Zero_On_B (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      return Self.Wait_On_Helper (Instruction.Wait_Type, Timeout_Seconds.Value, Instruction.Position);
   end Cmd_Wait_If_Zero_On_B;

   -- opcode 37 | Str_Alloc | U8 - U8 - U8 - U8 | Opcode - Id - Pad - Pad
   -- Allocates a new string, this does nothing in Adamant because we do not do dynamic memory allocation.
   function Cmd_Str_Alloc (Self : in out Instance) return Seq_Position is
      Instruction : Str_Alloc_Record.T;
      Status : constant Seq_Status := Get_Str_Alloc (Self, Instruction);
   begin
      pragma Assert (Status = Success);
      return Self.Next_Position;
   end Cmd_Str_Alloc;

   -- opcode 39 | Str_Alloc | U8 - U8 - U8 - U8 - Struct - U8[64] | Opcode - Pad - Pad - Pad - Var_Info - Seq_String
   -- Set the location in the string pool to the encoded string
   -- We are not supporting strings yet...
   -- function Cmd_Str_Set (Self : in out Instance) return Seq_Position is
   --    Instruction : Str_Set_Record.T;
   --    Status : constant Seq_Status := Get_Str_Set (Self, Instruction);
   --    Index : Seq_Local_Id;
   -- begin
   --    pragma Assert (Status = Success);

   --    -- Read id of local string pool to set, then cast to seq_id (can throw error!)
   --    Index := Seq_Local_Id (Instruction.Var_Info.Id);

   --    -- Set the index in the local string pool with the encoded string
   --    Self.Local_String_Pool (Index) := Instruction.Encoded_String;

   --    return Self.Next_Position;
   -- exception
   --    when others =>
   --       return Self.Process_Error (Variable);
   -- end Cmd_Str_Set;

   -- opcode 43 | Print | U8 - U8 - U8 - U8 - U8[64] | Opcode - Type - Pad - Pad - Seq_String
   -- Set the location in the string pool to the encoded string
   function Cmd_Print (Self : in out Instance) return Seq_Position is
      Instruction : Print_Record.T;
   begin
      -- Can fail on print type
      if Get_Print (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Copy the print type included in the instruction into our instance variable which
      -- can the be fetched by the caller.
      Self.String_To_Print := (
         Print_Type => Instruction.Print_Type,
         Encoded_String => [others => 0]    -- Clear this with zeros.
      );
      -- Perform copy of string.
      Safe_Left_Copy (
         Dest => Self.String_To_Print.Encoded_String,
         Src => Instruction.Encoded_String
      );

      -- Debug prints.
      -- Put ("[");
      -- Put (Instruction.Print_Type'Image);
      -- Put ("] ");
      -- Print_String (Instruction.Encoded_String);
      -- New_Line;

      -- Now we need to transition our state to print.
      Self.Set_State_Blocking (Print);

      return Self.Next_Position;
   end Cmd_Print;

   function Cmd_Print_Var (Self : in out Instance) return Seq_Position is
      Instruction : Print_Var_Record.T;
      To_Print : Packed_Poly_32_Type.T;
   begin
      -- Can fail on print type, data type, and fields in var info struct
      if Get_Print_Var (Self, Instruction) = Failure then
         return Self.Process_Error (Parse);
      end if;

      -- Copy the print type included in the instruction into our instance variable which
      -- can the be fetched by the caller.
      Self.String_To_Print := (
         Print_Type => Instruction.Print_Type,
         Encoded_String => [others => 0]    -- Clear this with zeros.
      );

      -- Get the variable:
      case Self.Fetch_Var_Helper (Var_Info => Instruction.Var_Info, Var => To_Print) is
         -- Continue on.
         when Success => null;
         -- Just return the current position. Errors have already been handled.
         when Failure => return Self.Position;
      end case;

      -- Debug prints.
      -- Put ("[");
      -- Put (Instruction.Print_Type'Image);
      -- Put ("] ");

      -- Switch on the data type to print. The following converts the data type to a string representation and
      -- then copies the string representation to the output byte array.
      pragma Warnings (Off, "overlay changes scalar storage order");
      case Instruction.Data_Type is
         when Seq_Data_Type.Unsigned | Seq_Data_Type.Discrete =>
            declare
               Print_Val : Packed_U32.T with Import, Convention => Ada, Address => To_Print'Address;
               Str : constant String := Print_Val.Value'Image;
               Str_Bytes : Basic_Types.Byte_Array (0 .. Str'Length - 1) with Import, Convention => Ada, Address => Str'Address;
            begin
               -- Perform copy of string.
               Safe_Left_Copy (Dest => Self.String_To_Print.Encoded_String, Src => Str_Bytes);
               -- Put (Print_Val.Value'Image);
            end;
         when Seq_Data_Type.Signed =>
            declare
               Print_Val : Packed_I32.T with Import, Convention => Ada, Address => To_Print'Address;
               Str : constant String := Print_Val.Value'Image;
               Str_Bytes : Basic_Types.Byte_Array (0 .. Str'Length - 1) with Import, Convention => Ada, Address => Str'Address;
            begin
               -- Perform copy of string.
               Safe_Left_Copy (Dest => Self.String_To_Print.Encoded_String, Src => Str_Bytes);
               -- Put (Print_Val.Value'Image);
            end;
         when Seq_Data_Type.Float =>
            declare
               Print_Val : Packed_F32.T with Import, Convention => Ada, Address => To_Print'Address;
            begin
               if Print_Val.Value'Valid then
                  declare
                     Str : constant String := Print_Val.Value'Image;
                     Str_Bytes : Basic_Types.Byte_Array (0 .. Str'Length - 1) with Import, Convention => Ada, Address => Str'Address;
                  begin
                     -- Perform copy of string.
                     Safe_Left_Copy (Dest => Self.String_To_Print.Encoded_String, Src => Str_Bytes);
                     -- Put (Print_Val.Value'Image);
                  end;
               else
                  return Self.Process_Error (Float_Value);
               end if;
            end;
      end case;
      pragma Warnings (On, "overlay changes scalar storage order");
      -- New_Line;

      -- Now we need to transition our state to print.
      Self.Set_State_Blocking (Print);

      return Self.Next_Position;
   end Cmd_Print_Var;

   -- We are not supporting strings yet...
   -- function Cmd_Print_Str (Self : in out Instance) return Seq_Position is
   --    Instruction : Print_Str_Record.T;
   --    Status : constant Seq_Status := Get_Print_Str (Self, Instruction);
   --    Index : Seq_Local_Id;
   -- begin
   --    pragma Assert (Status = Success);

   --    -- Read id of local string pool to set, then cast to seq_id (can throw error!)
   --    Index := Seq_Local_Id (Instruction.Var_Info.Id);

   --    -- Execute the print
   --    -- Put (Seq_Runtime.Print_Output, "[");
   --    -- Put (Seq_Runtime.Print_Output, Instruction.Print_Type'Image);
   --    -- Put (Seq_Runtime.Print_Output, "] ");
   --    Put ("[");
   --    Put (Instruction.Print_Type'Image);
   --    Put ("] ");
   --    Print_String (Self.Local_String_Pool (Index));
   --    New_Line;

   --    return Self.Next_Position;
   -- exception
   --    when others =>
   --       return Self.Process_Error (Variable);
   -- end Cmd_Print_Str;

end Seq_Runtime;
