with Packed_U32.Validation;

package body Seq_Runtime.Tester is

   procedure Reset_Instance (Self : in out Instance) is
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 99);
   begin
      Self.Unload;
      Self.Seq_Header := Header;
   end Reset_Instance;

   function Call_Set_Bit_Pattern (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Set_Bit_Pattern;
   end Call_Set_Bit_Pattern;

   function Call_Send_Bit_Pattern (Self : in out Instance) return Seq_Position is
   begin
      return Self.Cmd_Send_Bit_Pattern;
   end Call_Send_Bit_Pattern;

   function Call_Update_Bit_Pattern (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Update_Bit_Pattern;
   end Call_Update_Bit_Pattern;

   function Call_Goto (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Goto;
   end Call_Goto;

   function Call_Call (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Call;
   end Call_Call;

   function Call_Spawn (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Spawn;
   end Call_Spawn;

   function Call_Start (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Start;
   end Call_Start;

   function Call_Push (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Push;
   end Call_Push;

   function Call_Eval (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Eval;
   end Call_Eval;

   function Call_Eval_S (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Eval_S;
   end Call_Eval_S;

   function Call_Fetch_Var (Self : in out Instance; Instruction : in Basic_Types.Byte_Array; Internal_Dst : in Seq_Internal.E) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Fetch_Var (Internal_Dst);
   end Call_Fetch_Var;

   function Call_Store_Var (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Store_Var;
   end Call_Store_Var;

   function Call_Fetch_Tlm (Self : in out Instance; Instruction : in Basic_Types.Byte_Array; Internal_Dst : in Seq_Internal.E) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Fetch_Tlm (Internal_Dst);
   end Call_Fetch_Tlm;

   function Call_Wait (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Wait;
   end Call_Wait;

   function Call_Jump_If_Zero (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Jump_If_Zero;
   end Call_Jump_If_Zero;

   function Call_Jump_Not_Zero (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Jump_Not_Zero;
   end Call_Jump_Not_Zero;

   function Call_Jump_If_Equal (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Jump_If_Equal;
   end Call_Jump_If_Equal;

   function Call_Jump_Not_Equal (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Jump_Not_Equal;
   end Call_Jump_Not_Equal;

   function Call_Return (Self : in out Instance) return Seq_Position is
   begin
      return Self.Cmd_Return;
   end Call_Return;

   function Call_Wait_If_Zero (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Wait_If_Zero;
   end Call_Wait_If_Zero;

   function Call_Eval_Flt (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Eval_Flt;
   end Call_Eval_Flt;

   function Call_Cast_F_To_U (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Cast_F_To_U;
   end Call_Cast_F_To_U;

   function Call_Cast_U_To_F (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Cast_U_To_F;
   end Call_Cast_U_To_F;

   function Call_Cast_S_To_U (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Cast_S_To_U;
   end Call_Cast_S_To_U;

   function Call_Cast_U_To_S (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Cast_U_To_S;
   end Call_Cast_U_To_S;

   function Call_Cast_F_To_S (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Cast_F_To_S;
   end Call_Cast_F_To_S;

   function Call_Cast_S_To_F (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Cast_S_To_F;
   end Call_Cast_S_To_F;

   function Call_Wait_On_B (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Wait_On_B;
   end Call_Wait_On_B;

   function Call_Wait_If_Zero_On_B (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Seq_Header := Header;
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Wait_If_Zero_On_B;
   end Call_Wait_If_Zero_On_B;

   function Call_Str_Alloc (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Str_Alloc;
   end Call_Str_Alloc;

   function Call_Print (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Print;
   end Call_Print;

   function Call_Print_Var (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position is
   begin
      Self.Sequence_Region := Get_Single_Instruction_Sequence (Instruction);
      return Self.Cmd_Print_Var;
   end Call_Print_Var;

-----------------------------
-- Getter/Setter
-----------------------------
   function Check_Internal (Self : in out Instance; Src : in Seq_Internal.E) return Packed_U32.T is
      function Get_Internal_U32 is new Get_Internal (Packed_U32.T, Packed_U32.Validation.Valid);
      Result : Packed_U32.T;
      Ignore : Seq_Status := Get_Internal_U32 (Self, Src, Result);
   begin
      return Result;
   end Check_Internal;

   procedure Update_Internal (Self : in out Instance; Dest : in Seq_Internal.E; Val : in Packed_U32.T) is
      function Set_Internal_U32 is new Set_Internal (Packed_U32.T);
      Status : constant Seq_Status := Set_Internal_U32 (Self, Dest, Val);
   begin
      pragma Assert (Status = Success);
   end Update_Internal;

   procedure Set_Local (Self : in out Instance; Index : Seq_Local_Id; Var : in Packed_Poly_32_Type.T) is
   begin
      Self.Local_Variable_Array (Index) := Var;
   end Set_Local;

   procedure Set_Position (Self : in out Instance; New_Pos : Seq_Position) is
   begin
      Self.Position := New_Pos;
   end Set_Position;

   function Get_Telemetry_Destination (Self : in out Instance) return Seq_Internal.E is
   begin
      return Self.Telemetry_Destination;
   end Get_Telemetry_Destination;

   function Get_Telemetry_Timeout (Self : in out Instance) return Interfaces.Unsigned_32 is
   begin
      return Self.Telemetry_Timeout.Seconds;
   end Get_Telemetry_Timeout;

   procedure Set_State (Self : in out Instance; New_State : in Seq_Runtime_State.E) is
   begin
      Self.State := New_State;
   end Set_State;

   procedure Set_Most_Recent_Exec_Time (Self : in out Instance; Time : in Interfaces.Unsigned_32) is
   begin
      Self.Most_Recent_Execution_Time.Seconds := Time;
   end Set_Most_Recent_Exec_Time;

   procedure Force_Wake (Self : in out Instance) is
   begin
      Self.State := Ready;
   end Force_Wake;

   procedure Ignore_Error (Self : in out Instance) is
   begin
      Self.State := Ready;
   end Ignore_Error;

   procedure Force_Next_Position (Self : in out Instance) is
   begin
      Self.Position := Self.Next_Position;
   end Force_Next_Position;

-----------------------------
-- Private Utility
-----------------------------

   function Get_Single_Instruction_Sequence (Instruction : in Basic_Types.Byte_Array) return Memory_Region.T is
      To_Return : constant Memory_Region.T := (Address => Instruction'Address, Length => Instruction'Length);
   begin
      return To_Return;
   end Get_Single_Instruction_Sequence;

end Seq_Runtime.Tester;
