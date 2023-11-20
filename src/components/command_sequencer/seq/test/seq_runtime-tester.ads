with Packed_U32;
with Interfaces;
with Sequence_Header;
with Packed_Poly_32_Type;

package Seq_Runtime.Tester is

   procedure Reset_Instance (Self : in out Instance);
   function Call_Set_Bit_Pattern (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Send_Bit_Pattern (Self : in out Instance) return Seq_Position;
   function Call_Update_Bit_Pattern (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Goto (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Call (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Spawn (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Start (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Push (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Eval (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Eval_S (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Fetch_Var (Self : in out Instance; Instruction : in Basic_Types.Byte_Array; Internal_Dst : in Seq_Internal.E) return Seq_Position;
   function Call_Store_Var (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Fetch_Tlm (Self : in out Instance; Instruction : in Basic_Types.Byte_Array; Internal_Dst : in Seq_Internal.E) return Seq_Position;
   function Call_Wait (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Jump_If_Zero (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Jump_Not_Zero (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Jump_If_Equal (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Jump_Not_Equal (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Return (Self : in out Instance) return Seq_Position;
   function Call_Wait_If_Zero (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Eval_Flt (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Cast_F_To_U (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Cast_U_To_F (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Cast_S_To_U (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Cast_U_To_S (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Cast_F_To_S (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Cast_S_To_F (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Wait_On_B (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Wait_If_Zero_On_B (Self : in out Instance; Header : Sequence_Header.T; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Str_Alloc (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Print (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;
   function Call_Print_Var (Self : in out Instance; Instruction : in Basic_Types.Byte_Array) return Seq_Position;

   function Check_Internal (Self : in out Instance; Src : in Seq_Internal.E) return Packed_U32.T;
   procedure Update_Internal (Self : in out Instance; Dest : in Seq_Internal.E; Val : in Packed_U32.T);
   procedure Set_Local (Self : in out Instance; Index : Seq_Local_Id; Var : in Packed_Poly_32_Type.T);
   procedure Set_Position (Self : in out Instance; New_Pos : Seq_Position);
   procedure Force_Next_Position (Self : in out Instance);
   function Get_Telemetry_Destination (Self : in out Instance) return Seq_Internal.E;
   function Get_Telemetry_Timeout (Self : in out Instance) return Interfaces.Unsigned_32;
   procedure Set_State (Self : in out Instance; New_State : in Seq_Runtime_State.E);
   procedure Set_Most_Recent_Exec_Time (Self : in out Instance; Time : in Interfaces.Unsigned_32);
   procedure Force_Wake (Self : in out Instance);
   procedure Ignore_Error (Self : in out Instance);

private

      -- Appends a single instruction to a header, and then returns the memory region
   function Get_Single_Instruction_Sequence (Instruction : in Basic_Types.Byte_Array) return Memory_Region.T;

end Seq_Runtime.Tester;
