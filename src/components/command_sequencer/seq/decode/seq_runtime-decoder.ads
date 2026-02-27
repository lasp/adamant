with Memory_Region;
with Ada.Text_IO; use Ada.Text_IO;
with Seq_Config;

package Seq_Runtime.Decoder is

   -- Decoder_Instance type:
   type Decoder_Instance is new Seq_Runtime.Instance with private;

   -- It takes the filename as an argument
   procedure Decode (Self : in out Decoder_Instance; Path : in String; Config_Path : in String := ""; Output : in File_Type := Standard_Output);

private

   function Load_Sequence_In_Memory (Path : in String; Buffer : in Basic_Types.Byte_Array_Access; Sequence : out Memory_Region.T) return Boolean;
   procedure Print_Decode_String (To_Print : in Seq_String; Output : File_Type);

   ---------------------
   -- Seq Instructions
   ---------------------
   function Decode_Set_Bit_Pattern (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Send_Bit_Pattern (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Update_Bit_Pattern (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Call (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Spawn (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Start (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Push (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Eval (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Fetch_Var (Self : in out Decoder_Instance; Internal_Dst : in Seq_Internal.E; Output : in File_Type) return Seq_Position;
   function Decode_Store_Var (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Fetch_Tlm (Self : in out Decoder_Instance; Internal_Dst : in Seq_Internal.E; Output : in File_Type) return Seq_Position;
   function Decode_Wait (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Goto (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Jump_If_Zero (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Jump_Not_Zero (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Jump_If_Equal (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Jump_Not_Equal (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Return (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Wait_If_Zero (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   -- function Decode_Kill_Category             (self : in out Decoder_Instance) return Seq_Position; change to Kill_All except this sequence
   function Decode_Kill_Engine (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   -- function Decode_Kill_Name                   (self : in out Decoder_Instance) return Seq_Position;
   -- function Decode_Subscribe                   (self : in out Decoder_Instance) return Seq_Position;
   -- function Decode_Unsubscribe                (self : in out Decoder_Instance) return Seq_Position;
   function Decode_Eval_Flt (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Cast_F_To_U (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Cast_U_To_F (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Eval_S (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Cast_S_To_U (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Cast_U_To_S (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Cast_F_To_S (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Cast_S_To_F (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Wait_On_B (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Wait_If_Zero_On_B (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Str_Alloc (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   -- function Decode_Str_Dealloc                (self : in out Decoder_Instance; output : in File_Type) return Seq_Position;
   function Decode_Str_Set (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   -- function Decode_Str_Update_Pattern      (self : in out Decoder_Instance; output : in File_Type) return Seq_Position;
   -- function Decode_Str_Copy                     (self : in out Decoder_Instance; output : in File_Type) return Seq_Position;
   -- function Decode_Str_Move                     (self : in out Decoder_Instance; output : in File_Type) return Seq_Position;
   function Decode_Print (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   function Decode_Print_Var (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;
   -- function Decode_Print_Str (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;

   function Decode_Instruction (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position;

   type Decoder_Instance is new Seq_Runtime.Instance with record
      Config : Seq_Config.Instance;
   end record;

end Seq_Runtime.Decoder;
