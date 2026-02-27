with Ada.Sequential_IO;
with Ada.IO_Exceptions;
with Command;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with System.Storage_Elements;
with Serializer_Types; use Serializer_Types;
with Data_Product_Types;

-- Instruction records
with Set_Bit_Record.Validation;
with Send_Bit_Record;
with Update_Bit_Record.Validation;
with Fetch_Var_Record.Validation;
with Store_Var_Record.Validation;
with Fetch_Tlm_Record.Validation;
with Fetch_Tlm_Record.Representation;
with Telemetry_Record.Representation;
with Wait_Record.Validation;
with Goto_Record.Validation;
with Jump_Zero_Record.Validation;
with Jump_Equal_Record.Validation;
with Cast_Record.Validation;
with Eval_Record.Validation;
with Load_New_Seq_Record;
with Load_New_Seq_Record.Validation;
with Push_Record.Validation;
with Wait_If_Zero_Record.Validation;
with Wait_On_B_Record.Validation;
with Wait_If_Zero_On_B_Record;
with Wait_If_Zero_On_B_Record.Validation;
with Sequence_Header.Representation;
with Str_Alloc_Record.Validation;
with Str_Set_Record.Validation;
with Str_Set_Record;
with Print_Record.Validation;
with Print_Var_Record.Validation;
with Kill_Eng_Record.Validation;
with Basic_Types.Representation;

package body Seq_Runtime.Decoder is
   use Seq_Operation;
   use Seq_Opcode;
   use Seq_Wait_Type;
   use Var_Origin;

   -- We can handle up to 512 KB sized sequence.
   Max_Sequence_Size : constant Natural := 524_288;

   function Strip (In_String : in String) return String is
   begin
      return Trim (In_String, Ada.Strings.Both);
   end Strip;

   procedure Decode (Self : in out Decoder_Instance; Path : in String; Config_Path : in String := ""; Output : in File_Type := Standard_Output) is
      Sequence : Memory_Region.T;
      Buffer : Basic_Types.Byte_Array_Access;
   begin
      Buffer := new Basic_Types.Byte_Array (0 .. Max_Sequence_Size - 1);

      -- Attempt to load a sequence into a memory region
      if Load_Sequence_In_Memory (Path, Buffer, Sequence) = False then
         Put_Line (Output, "Sequence " & Path & " does not exist!");
         return;
      end if;

      Put (Output, "Decoding " & Path);
      if Config_Path /= "" then
         Put (Output, " with config " & Config_Path);
         Self.Config.Init (Config_Path);
      end if;
      Put_Line (Output, " ...");

      -- Set the runtime Decoder_Instance data
      Self.Sequence_Region := Sequence;

      -- Parse seq header
      Self.Parse_Sequence_Header;

      -- Print the header:
      Put (Output, "00 [HEADER] ");
      Put_Line (Output, Sequence_Header.Representation.To_Tuple_String (Self.Seq_Header));

      -- Decode all the instructions
      while Self.Position < Seq_Position (Self.Seq_Header.Length) loop
         Self.Position := Decode_Instruction (Self, Output);
      end loop;
      Put_Line (Output, "Done decoding " & Path);
   end Decode;

   function Decode_Instruction (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      This_Opcode : constant Seq_Opcode.E := Self.Get_Opcode_From_Memory;
   begin
      Put (Output, Strip (Self.Position'Image) & " [" & This_Opcode'Image & "] ");
      case This_Opcode is
         when Set_Bit_Pattern =>
            return Decode_Set_Bit_Pattern (Self, Output);
         when Send_Bit_Pattern =>
            return Decode_Send_Bit_Pattern (Self, Output);
         when Update_Bit_Pattern =>
            return Decode_Update_Bit_Pattern (Self, Output);
         when Call =>
            return Decode_Call (Self, Output);
         when Spawn =>
            return Decode_Spawn (Self, Output);
         when Start =>
            return Decode_Start (Self, Output);
         when Push =>
            return Decode_Push (Self, Output);
         when Eval =>
            return Decode_Eval (Self, Output);
         when Fetch_Var_A =>
            return Decode_Fetch_Var (Self, Seq_Internal.A, Output);
         when Fetch_Var_B =>
            return Decode_Fetch_Var (Self, Seq_Internal.B, Output);
         when Store_Var =>
            return Decode_Store_Var (Self, Output);
         when Fetch_Tlm_A =>
            return Decode_Fetch_Tlm (Self, Seq_Internal.A, Output);
         when Fetch_Tlm_B =>
            return Decode_Fetch_Tlm (Self, Seq_Internal.B, Output);
         when Invalid =>
            raise Program_Error with "An Invalid Instruction Was Encountered.";
         when Wait =>
            return Decode_Wait (Self, Output);
         when Seq_Goto =>
            return Decode_Goto (Self, Output);
         when Jump_If_Zero =>
            return Decode_Jump_If_Zero (Self, Output);
         when Jump_Not_Zero =>
            return Decode_Jump_Not_Zero (Self, Output);
         when Jump_If_Equal =>
            return Decode_Jump_If_Equal (Self, Output);
         when Jump_Not_Equal =>
            return Decode_Jump_Not_Equal (Self, Output);
         when Seq_Return =>
            return Decode_Return (Self, Output);
         when Wait_If_Zero =>
            return Decode_Wait_If_Zero (Self, Output);
         -- when Kill_Category =>         return Self.Decode_Kill_Category;
         when Kill_Engine =>
            return Decode_Kill_Engine (Self, Output);
         -- when Kill_Name =>               return Self.Decode_Kill_Name;
         when Subscribe =>
            Put_Line (Output, "Ignore");
            return Self.Position + 4;
         when Unsubscribe =>
            Put_Line (Output, "Ignore");
            return Self.Position + 4;
         when Eval_Flt =>
            return Decode_Eval_Flt (Self, Output);
         when Cast_F_To_U =>
            return Decode_Cast_F_To_U (Self, Output);
         when Cast_U_To_F =>
            return Decode_Cast_U_To_F (Self, Output);
         when Eval_S =>
            return Decode_Eval_S (Self, Output);
         when Cast_S_To_U =>
            return Decode_Cast_S_To_U (Self, Output);
         when Cast_U_To_S =>
            return Decode_Cast_U_To_S (Self, Output);
         when Cast_F_To_S =>
            return Decode_Cast_F_To_S (Self, Output);
         when Cast_S_To_F =>
            return Decode_Cast_S_To_F (Self, Output);
         when Wait_On_B =>
            return Decode_Wait_On_B (Self, Output);
         when Wait_If_Zero_On_B =>
            return Decode_Wait_If_Zero_On_B (Self, Output);
         when Str_Alloc =>
            return Decode_Str_Alloc (Self, Output);
         -- when Str_Dealloc =>            return Self.Decode_Str_Dealloc;
         -- when Str_Set =>
         --    return Decode_Str_Set (Self, Output);
         -- when Str_Update_Bit_Pattern => return Self.Decode_Str_Update_Pattern;
         -- when Str_Copy =>                return Self.Decode_Str_Copy;
         -- when Str_Move =>                return Self.Decode_Str_Move;
         when Print =>
            return Decode_Print (Self, Output);
         when Print_Var =>
            return Decode_Print_Var (Self, Output);
         -- when Print_Str =>
         --    return Decode_Print_Str (Self, Output);
         when others =>
            return raise Program_Error with ("Unhandled Opcode: " & This_Opcode'Image);
      end case;
   end Decode_Instruction;

   function Load_Sequence_In_Memory (Path : in String; Buffer : in Basic_Types.Byte_Array_Access; Sequence : out Memory_Region.T) return Boolean is
      package Io is new Ada.Sequential_Io (Basic_Types.Byte);
      use Io;
      File : Io.File_Type;
      Data : Basic_Types.Byte;
      Sequence_Size : Natural := 0;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Read (File, Data);
         Buffer (Sequence_Size) := Data;
         Sequence_Size := @ + 1;
      end loop;
      Sequence := (Address => (Buffer (0)'Address), Length => Sequence_Size);
      Close (File);
      return True;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return False;
   end Load_Sequence_In_Memory;

   -- Generic instantiation of decoder "get" functions:
   function Get_Set_Bit_Pattern is new Get_Instruction (Set_Bit_Record.T, Set_Bit_Record.Validation.Valid);
   function Get_Update_Bit_Pattern is new Get_Instruction (Update_Bit_Record.T, Update_Bit_Record.Validation.Valid);
   function Get_Call is new Get_Instruction (Load_New_Seq_Record.T, Load_New_Seq_Record.Validation.Valid);
   function Get_Spawn is new Get_Instruction (Load_New_Seq_Record.T, Load_New_Seq_Record.Validation.Valid);
   function Get_Start is new Get_Instruction (Load_New_Seq_Record.T, Load_New_Seq_Record.Validation.Valid);
   function Get_Push is new Get_Instruction (Push_Record.T, Push_Record.Validation.Valid);
   function Get_Eval is new Get_Instruction (Eval_Record.T, Eval_Record.Validation.Valid);
   function Get_Fetch is new Get_Instruction (Fetch_Var_Record.T, Fetch_Var_Record.Validation.Valid);
   function Get_Store is new Get_Instruction (Store_Var_Record.T, Store_Var_Record.Validation.Valid);
   function Get_Fetch_Tlm is new Get_Instruction (Fetch_Tlm_Record.T, Fetch_Tlm_Record.Validation.Valid);
   function Get_Wait is new Get_Instruction (Wait_Record.T, Wait_Record.Validation.Valid);
   function Get_Goto is new Get_Instruction (Goto_Record.T, Goto_Record.Validation.Valid);
   function Get_Jump_Zero is new Get_Instruction (Jump_Zero_Record.T, Jump_Zero_Record.Validation.Valid);
   function Get_Jump_Equal is new Get_Instruction (Jump_Equal_Record.T, Jump_Equal_Record.Validation.Valid);
   function Get_Wait_If_Zero is new Get_Instruction (Wait_If_Zero_Record.T, Wait_If_Zero_Record.Validation.Valid);
   function Get_Cast is new Get_Instruction (Cast_Record.T, Cast_Record.Validation.Valid);
   function Get_Wait_On_B is new Get_Instruction (Wait_On_B_Record.T, Wait_On_B_Record.Validation.Valid);
   function Get_Wait_If_Zero_On_B is new Get_Instruction (Wait_If_Zero_On_B_Record.T, Wait_If_Zero_On_B_Record.Validation.Valid);
   function Get_Kill_Engine is new Get_Instruction (Kill_Eng_Record.T, Kill_Eng_Record.Validation.Valid);
   function Get_Str_Alloc is new Get_Instruction (Str_Alloc_Record.T, Str_Alloc_Record.Validation.Valid);
   function Get_Str_Set is new Get_Instruction (Str_Set_Record.T, Str_Set_Record.Validation.Valid);
   function Get_Print is new Get_Instruction (Print_Record.T, Print_Record.Validation.Valid);
   function Get_Print_Var is new Get_Instruction (Print_Var_Record.T, Print_Var_Record.Validation.Valid);

   procedure Print_Decode_String (To_Print : in Seq_String; Output : File_Type) is
   begin
      for A_Byte of To_Print loop
         exit when A_Byte = 0;

         declare
            Char : Character with Import, Convention => Ada, Address => A_Byte'Address;
         begin
            Put (Output, Char);
         end;
      end loop;
   end Print_Decode_String;

   ---------------------
   -- Seq Instructions
   ---------------------
   function Decode_Set_Bit_Pattern (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Set_Bit_Record.T;
      Status : constant Seq_Status := Get_Set_Bit_Pattern (Seq_Runtime.Instance (Self), Instruction);

      use System.Storage_Elements;
      Bytes : Basic_Types.Byte_Array (1 .. Natural (Instruction.Length)) with Import, Convention => Ada, Address => Self.Sequence_Region.Address + Storage_Offset (Self.Next_Position);
      Bytes_Serialized : Natural;
      Command_Serialization_Status : constant Serialization_Status := Command.Serialization.From_Byte_Array (Self.Bit_Pattern, Bytes, Bytes_Serialized);
      Command_Name : constant String := Self.Config.Get_Command_Name (Self.Bit_Pattern.Header.Id);
   begin
      pragma Assert (Status = Success);

      -- Command Serialization Failure
      if Command_Serialization_Status = Failure or else Bytes_Serialized /= Natural (Instruction.Length) then
         raise Program_Error with "Command Serialization Failure.";
      end if;

      Self.Next_Position := @ + Seq_Position (Bytes_Serialized);

      -- Read off the end of the sequence
      if Self.Next_Position > Seq_Position (Self.Seq_Header.Length) then
         raise Program_Error with "Command Length Error, read off sequence end.";
      end if;

      -- Put_Line (output, Set_Bit_Record.Representation.Image (instruction));
      if Command_Name /= "" then
         Put (Output, "Prepare command " & Command_Name & " (" & Strip (Self.Bit_Pattern.Header.Id'Image) & ")");
      else
         Put (Output, "Prepare command with ID " & Strip (Self.Bit_Pattern.Header.Id'Image));
      end if;

      Put_Line (Output,
         ", Length " & Strip (Self.Bit_Pattern.Header.Arg_Buffer_Length'Image) &
         ", Buffer " & Strip (Basic_Types.Representation.Image (Self.Bit_Pattern.Arg_Buffer (
            Self.Bit_Pattern.Arg_Buffer'First ..
            Self.Bit_Pattern.Arg_Buffer'First + Self.Bit_Pattern.Header.Arg_Buffer_Length - 1
         ))
      ));

      return Self.Next_Position;
   end Decode_Set_Bit_Pattern;

   function Decode_Send_Bit_Pattern (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
   begin
      Put_Line (Output, "Send command and wait for response");
      return Self.Position + Seq_Position (Send_Bit_Record.Size_In_Bytes);
   end Decode_Send_Bit_Pattern;

   function Decode_Update_Bit_Pattern (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Update_Bit_Record.T;
      Parse_Status : constant Seq_Status := Get_Update_Bit_Pattern (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Parse_Status = Success);

      Put_Line (Output, "Copy " & Strip (Instruction.Length'Image) & " bits from INTERNAL.A to command at position " & Strip (Instruction.Offset'Image));

      return Self.Next_Position;
   end Decode_Update_Bit_Pattern;

   function Decode_Call (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Load_New_Seq_Record.T;
      Parse_Status : constant Seq_Status := Get_Call (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Parse_Status = Success);

      Put_Line (Output, "Call sequence " & Strip (Instruction.Id'Image)); -- & ", State Change -> WAIT_LOAD_NEW_SUB_SEQ");

      return Self.Next_Position;
   end Decode_Call;

   function Decode_Spawn (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Load_New_Seq_Record.T;
      Parse_Status : constant Seq_Status := Get_Spawn (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Parse_Status = Success);

      Put (Output, "Spawn sequence " & Strip (Instruction.Id'Image));
      if Instruction.Engine = 255 then
         Put_Line (Output, " on any engine (255)");
      else
         Put_Line (Output, " on engine " & Strip (Instruction.Engine'Image)); -- & ", State Change -> WAIT_LOAD_NEW_SEQ_ELSEWHERE");
      end if;

      return Self.Next_Position;
   end Decode_Spawn;

   function Decode_Start (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Load_New_Seq_Record.T;
      Parse_Status : constant Seq_Status := Get_Start (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Parse_Status = Success);

      Put_Line (Output, "Start sequence " & Strip (Instruction.Id'Image) & " on this engine"); -- & ", State Change -> WAIT_LOAD_NEW_SEQ_OVERWRITE");

      return Self.Next_Position;
   end Decode_Start;

   function Decode_Push (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Push_Record.T;
      Parse_Status : constant Seq_Status := Get_Push (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Parse_Status = Success);

      Put_Line (Output, "Copy INTERNAL.A to ARGUMENT [" & Strip (Instruction.Destination'Image) & "]");

      return Self.Next_Position;
   end Decode_Push;

   function Decode_Eval (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Eval_Record.T;
   begin
      -- Can fail on Op field
      if Get_Eval (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Eval : Invalid field detected";
      end if;

      Put_Line (Output, "Store result of INTERNAL.A <" & Instruction.Operation'Image & "> INTERNAL.B to INTERNAL.A");

      return Self.Next_Position;
   end Decode_Eval;

   function Var_Record_To_Str (Info : in Var_Record.T) return String is
      function Do_Return (Post : in String) return String is
      begin
         return Info.Var_Type'Image & "." & Post;
      end Do_Return;
   begin
      case Info.Var_Type is
         when In_Sequence =>
            return Strip (Info.Id'Image);
         when Local => null;
            return Do_Return ("Variable [" & Strip (Info.Id'Image) & "]");
         when Internal =>
            -- Move the value in the encoded internal into the destination internal
            if Info.Id <= 3 then
               declare
                  Int_Var : constant Seq_Internal.E := Seq_Internal.E'Val (Info.Id);
               begin
                  return Do_Return (Int_Var'Image & " (" & Strip (Info.Id'Image) & ")");
               end;
            else
               return Do_Return ("Unknown (" & Strip (Info.Id'Image) & ")");
            end if;
         when Global =>
            return Do_Return ("Variable [" & Strip (Info.Id'Image) & "]");
      end case;
   end Var_Record_To_Str;

   function Decode_Fetch_Var (Self : in out Decoder_Instance; Internal_Dst : in Seq_Internal.E; Output : in File_Type) return Seq_Position is
      Instruction : Fetch_Var_Record.T;
   begin
      if Get_Fetch (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Fetch_Var : Invalid field detected";
      end if;

      Put_Line (Output, "Copy " & Var_Record_To_Str (Instruction.Var_Info) & " to INTERNAL." &
                     Internal_Dst'Image);

      return Self.Next_Position;
   end Decode_Fetch_Var;

   function Decode_Store_Var (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Store_Var_Record.T;
   begin
      if Get_Store (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Store_Var : Invalid field detected";
      end if;

      Put_Line (Output, "Copy INTERNAL.A to " & Var_Record_To_Str (Instruction.Var_Info));

      return Self.Next_Position;
   end Decode_Store_Var;

   function Decode_Fetch_Tlm (Self : in out Decoder_Instance; Internal_Dst : in Seq_Internal.E; Output : in File_Type) return Seq_Position is
      Instruction : Fetch_Tlm_Record.T;
   begin
      -- Can fail on isNewRequired
      if Get_Fetch_Tlm (Seq_Runtime.Instance (Self), Instruction) = Failure then
         Put_Line (Output, Fetch_Tlm_Record.Representation.Image (Instruction));
         Put_Line (Output, Telemetry_Record.Representation.Image (Instruction.Tlm_Info));
         raise Program_Error with "Invalid field detected";
      end if;

      if Instruction.Waiton = True then
         Put (Output, "Wait on ");
      else
         Put (Output, "Fetch ");
      end if;

      if Instruction.Tlm_Info.New_Value_Required = True then
         Put (Output, "new ");
      end if;

      declare
         Telem_Name : constant String := Self.Config.Get_Telemetry_Name (Data_Product_Types.Data_Product_Id (Instruction.Tlm_Info.Id), Instruction.Tlm_Info.Offset);
      begin
         if Telem_Name /= "" then
            Put (Output, "telemetry " & Telem_Name & " (" & Strip (Instruction.Tlm_Info.Id'Image) & ")");
         else
            Put (Output, "telemetry with ID " & Strip (Instruction.Tlm_Info.Id'Image));
         end if;
      end;
      Put_Line (Output, " and store to INTERNAL." & Internal_Dst'Image);

      return Self.Next_Position;
   end Decode_Fetch_Tlm;

   function Decode_Wait (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Wait_Record.T;
   begin
      -- Can fail on wait_type
      if Get_Wait (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Wait : Invalid field detected";
      end if;

      case Instruction.Wait_Type is
         when Relative =>
            Put_Line (Output, "Relative wait for " & Strip (Instruction.Wait_Time'Image)); -- & ", State Change -> WAIT_RELATIVE");
         when Absolute =>
            Put_Line (Output, "Absolute wait until " & Strip (Instruction.Wait_Time'Image)); -- & ", State Change -> WAIT_ABSOLUTE");
      end case;

      return Self.Next_Position;
   end Decode_Wait;

   function Jump_To_Str (Self : in Decoder_Instance; Position : in Interfaces.Unsigned_16) return String is
      function Do_Return (Post : in String) return String is
      begin
         return "Jump to " & Strip (Position'Image) & Post;
      end Do_Return;
   begin
      if Self.Validate_Jump (Position) = Failure then
         return Do_Return (" [Invalid]");
      elsif Seq_Position (Position) = Seq_Position (Self.Seq_Header.Length) then
         return Do_Return (" [End of Sequence]");
      else
         return Do_Return ("");
      end if;
   end Jump_To_Str;

   function Decode_Goto (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Goto_Record.T;
      Parse_Status : constant Seq_Status := Get_Goto (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Parse_Status = Success);

      Put_Line (Output, Jump_To_Str (Self, Instruction.Position));

      return Self.Next_Position;
   end Decode_Goto;

   function Decode_Jump_If_Zero (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Jump_Zero_Record.T;
      Instr_Status : constant Seq_Status := Get_Jump_Zero (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Instr_Status = Success);

      Put_Line (Output, Jump_To_Str (Self, Instruction.Position) & " if INTERNAL.A = 0");

      return Self.Next_Position;
   end Decode_Jump_If_Zero;

   function Decode_Jump_Not_Zero (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Jump_Zero_Record.T;
      Instr_Status : constant Seq_Status := Get_Jump_Zero (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Instr_Status = Success);

      Put_Line (Output, Jump_To_Str (Self, Instruction.Position) & " if INTERNAL.A /= 0");

      return Self.Next_Position;
   end Decode_Jump_Not_Zero;

   function Decode_Jump_If_Equal (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Jump_Equal_Record.T;
      Instr_Status : constant Seq_Status := Get_Jump_Equal (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Instr_Status = Success);

      Put_Line (Output, Jump_To_Str (Self, Instruction.Position) & " if INTERNAL.A = " & Instruction.To_Compare'Image);

      return Self.Next_Position;
   end Decode_Jump_If_Equal;

   function Decode_Jump_Not_Equal (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Jump_Equal_Record.T;
      Instr_Status : constant Seq_Status := Get_Jump_Equal (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Instr_Status = Success);

      Put_Line (Output, Jump_To_Str (Self, Instruction.Position) & " if INTERNAL.A /= " & Instruction.To_Compare'Image);

      return Self.Next_Position;
   end Decode_Jump_Not_Equal;

   function Decode_Return (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
   begin
      Put_Line (Output, "State Change -> DONE");
      return Self.Position + 4;
   end Decode_Return;

   function Decode_Wait_If_Zero (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Wait_If_Zero_Record.T;
   begin
      if Get_Wait_If_Zero (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Wait_If_Zero : Invalid field detected";
      end if;

      Put (Output, Jump_To_Str (Self, Instruction.Position) & " if INTERNAL.A = 0 with ");

      case Instruction.Wait_Type is
         when Relative =>
            Put_Line (Output, "relative timeout " & Strip (Instruction.Timeout'Image)); -- & ", State Change -> WAIT_TELEMETRY_RELATIVE");
         when Absolute =>
            Put_Line (Output, "absolute timeout " & Strip (Instruction.Timeout'Image)); -- & ", State Change -> WAIT_TELEMETRY_VALUE");
      end case;

      return Self.Next_Position;
   end Decode_Wait_If_Zero;

   function Decode_Eval_Flt (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Eval_Record.T;
   begin
      if Get_Eval (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Eval_Flt : Invalid field detected";
      end if;

      Put_Line (Output, "Store result of INTERNAL.A <" & Instruction.Operation'Image & "> INTERNAL.B to INTERNAL.A");

      return Self.Next_Position;
   end Decode_Eval_Flt;

   function Decode_Cast_F_To_U (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Cast_Record.T;
   begin
      if Get_Cast (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Cast_F_To_U : Invalid field detected";
      end if;

      Put_Line (Output, "Casting internal " & Instruction.Id'Image);

      return Self.Next_Position;
   end Decode_Cast_F_To_U;

   function Decode_Cast_U_To_F (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Cast_Record.T;
   begin
      if Get_Cast (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Cast_U_To_F : Invalid field detected";
      end if;

      Put_Line (Output, "Casting internal " & Instruction.Id'Image);

      return Self.Next_Position;
   end Decode_Cast_U_To_F;

   function Decode_Eval_S (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Eval_Record.T;
   begin
      -- Can fail on Op field
      if Get_Eval (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Eval_S : Invalid field detected";
      end if;

      Put_Line (Output, "Store result of INTERNAL.A <" & Instruction.Operation'Image & "> INTERNAL.B to INTERNAL.A");

      return Self.Next_Position;
   end Decode_Eval_S;

   function Decode_Cast_S_To_U (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Cast_Record.T;
   begin
      if Get_Cast (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Cast_S_To_U : Invalid field detected";
      end if;

      Put_Line (Output, "Cast INTERNAL." & Instruction.Id'Image & " from SIGNED to UNSIGNED");

      return Self.Next_Position;
   end Decode_Cast_S_To_U;

   function Decode_Cast_U_To_S (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Cast_Record.T;
   begin
      if Get_Cast (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Cast_U_To_S : Invalid field detected";
      end if;

      Put_Line (Output, "Cast INTERNAL." & Instruction.Id'Image & " from UNSIGNED to SIGNED");

      return Self.Next_Position;
   end Decode_Cast_U_To_S;

   function Decode_Cast_F_To_S (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Cast_Record.T;
   begin
      if Get_Cast (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Cast_F_To_S : Invalid field detected";
      end if;

      Put_Line (Output, "Cast INTERNAL." & Instruction.Id'Image & " from FLOAT to SIGNED");

      return Self.Next_Position;
   end Decode_Cast_F_To_S;

   function Decode_Cast_S_To_F (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Cast_Record.T;
   begin
      if Get_Cast (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Cast_S_To_F : Invalid field detected";
      end if;

      Put_Line (Output, "Cast INTERNAL." & Instruction.Id'Image & " from SIGNED to FLOAT");

      return Self.Next_Position;
   end Decode_Cast_S_To_F;

   function Decode_Wait_On_B (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Wait_On_B_Record.T;
   begin
      if Get_Wait_On_B (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Wait_On_B : Invalid field detected";
      end if;

      case Instruction.Wait_Type is
         when Relative =>
            Put_Line (Output, "Relative wait for INTERNAL.B"); -- & ", State Change -> WAIT_RELATIVE");
         when Absolute =>
            Put_Line (Output, "Absolute wait until INTERNAL.B"); -- & ", State Change -> WAIT_ABSOLUTE");
      end case;

      return Self.Next_Position;
   end Decode_Wait_On_B;

   function Decode_Wait_If_Zero_On_B (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Wait_If_Zero_On_B_Record.T;
   begin
      if Get_Wait_If_Zero_On_B (Seq_Runtime.Instance (Self), Instruction) = Failure then
         raise Program_Error with "Decode_Wait_If_Zero_On_B : Invalid field detected";
      end if;

      Put (Output, Jump_To_Str (Self, Instruction.Position) & " if INTERNAL.A = 0 with ");

      case Instruction.Wait_Type is
         when Relative =>
            Put (Output, "relative");
         when Absolute =>
            Put (Output, "absolute");
      end case;

      Put_Line (Output, " timeout INTERNAL.B");

      return Self.Next_Position;
   end Decode_Wait_If_Zero_On_B;

   function Decode_Str_Alloc (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Str_Alloc_Record.T;
      Status : constant Seq_Status := Get_Str_Alloc (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Status = Success);
      Put_Line (Output, "Ignore");
      return Self.Next_Position;
   end Decode_Str_Alloc;
   -- function Decode_Str_Dealloc                (Self : in out Decoder_Instance; output : in File_Type) return Seq_Position;

   function Decode_Str_Set (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Str_Set_Record.T;
      Status : constant Seq_Status := Get_Str_Set (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Status = Success);
      Put (Output, "Setting str" & Instruction.Var_Info.Id'Image & " = """);
      Print_Decode_String (Instruction.Encoded_String, Output);
      Put (Output, """");
      New_Line;
      return Self.Next_Position;
   end Decode_Str_Set;

   -- function Decode_Str_Update_Pattern      (Self : in out Decoder_Instance; output : in File_Type) return Seq_Position;
   -- function Decode_Str_Copy                     (Self : in out Decoder_Instance; output : in File_Type) return Seq_Position;
   -- function Decode_Str_Move                     (Self : in out Decoder_Instance; output : in File_Type) return Seq_Position;

   function Decode_Print (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Print_Record.T;
      Status : constant Seq_Status := Get_Print (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Status = Success);

      Put (Output, "Print """);
      Print_Decode_String (Instruction.Encoded_String, Output);
      Put_Line (Output, """ as " & Instruction.Print_Type'Image);

      return Self.Next_Position;
   end Decode_Print;

   function Decode_Print_Var (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Print_Var_Record.T;
   begin
      if Get_Print_Var (Seq_Runtime.Instance (Self), Instruction) = Failure then
         Put_Line (Output, "Invalid field " & Self.Errant_Field'Image);
         raise Program_Error with "Decode_Print_Var : Invalid field detected";
      end if;

      -- Execute the print
      Put_Line (Output, "Print " & Instruction.Data_Type'Image & " (" & Var_Record_To_Str (Instruction.Var_Info) & ") as " & Instruction.Print_Type'Image);

      return Self.Next_Position;
   end Decode_Print_Var;

   -- function Decode_Print_Str (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
   --    Instruction : Print_Str_Record.T;
   --    Status : constant Seq_Status := Get_Print_Str (Self, Instruction);
   --    Index : Seq_Local_Id;
   -- begin
   --    pragma Assert (Status = Success);

   --    -- Read id of local string pool to set, then cast to seq_id (can throw error!)
   --    Index := Seq_Local_Id (Instruction.Var_Info.Id);

   --    -- Execute the print
   --    Put (Output, "[");
   --    Put (Output, Instruction.Print_Type'Image);
   --    Put (Output, "] ");
   --    Put_Line (Output, "String at index:" & Index'Image);

   --    return Self.Next_Position;
   -- end Decode_Print_Str;

   function Decode_Kill_Engine (Self : in out Decoder_Instance; Output : in File_Type) return Seq_Position is
      Instruction : Kill_Eng_Record.T;
      Status : constant Seq_Status := Get_Kill_Engine (Seq_Runtime.Instance (Self), Instruction);
   begin
      pragma Assert (Status = Success);

      Put_Line (Output, "Kill engine " & Strip (Instruction.Engine_Start'Image) & " for " & Strip (Instruction.Num_To_Kill'Image));

      return Self.Next_Position;
   end Decode_Kill_Engine;

end Seq_Runtime.Decoder;
