with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Basic_Types;
with Serializer_Types; use Serializer_Types;

package body Seq_Config is

   type String_Array is array (Natural range 0 .. 9) of Seq_String;

   function Strip_Nul (In_String : in String) return String is
      Str : constant String := Trim (In_String, Ada.Strings.Both);
      Idx : Natural := Str'First;
   begin
      for Char of Str loop
         exit when Char = ASCII.NUL;
         Idx := Idx + 1;
      end loop;
      return Str (Str'First .. Idx - 1);
   end Strip_Nul;

   function Parse_Line (S : in String; Words_Parsed : out Natural) return String_Array is
      F    : Positive;
      L    : Natural := 0;
      I    : Natural := 1;
      Whitespace : constant Character_Set := To_Set (' ');
      To_Return : String_Array := [others => [others => ASCII.NUL]];
      End_Idx : Natural := S'Last;
   begin
      Words_Parsed := Natural'First;

      -- First figure out where a comment might start and
      -- ignor everything after it.
      while I in S'Range loop
         if S (I) = ';' then
            End_Idx := I - 1;
            exit;
         end if;
         I := I + 1;
      end loop;

      -- Iterate through all words:
      I := 1;
      while I in S'First .. End_Idx loop
         -- Parse the next token
         Find_Token (
            Source   => S,
            Set       => Whitespace,
            From      => I,
            Test      => Outside,
            First    => F,
            Last      => L
         );

         -- Exit if we are at end of line:
         exit when L = 0;

         -- Put_Line ("Found word instance at position "
         --                & Natural'Image (F)
         --                & ": '" & S (F .. L) & "'");

         -- Store word in return array:
         To_Return (Words_Parsed) (1 .. L - F + 1) := S (F .. L);
         Words_Parsed := Words_Parsed + 1;
         I := L + 1;
      end loop;

      return To_Return;
   end Parse_Line;

   function Seq_Str_Cmp (L : in Seq_String; R : in String) return Boolean is
   begin
      return L (1 .. R'Length) = R;
   end Seq_Str_Cmp;

   function Get_Telemetry_Key (Id : in Data_Product_Types.Data_Product_Id; Offset : in Interfaces.Unsigned_16) return Interfaces.Unsigned_32 is
      To_Return : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Id) * 65536 + Interfaces.Unsigned_32 (Offset);
   begin
      return To_Return;
   end Get_Telemetry_Key;

   procedure Init (Self : in out Instance; Path : in String) is
      File : File_Type;
      Parsed_Line : String_Array;
      Words_Parsed : Natural := 0;
   begin
      -- Open the file:
      Open (File => File, Mode => In_File, Name => Path);

      -- Iterate through each line of the file:
      while not End_Of_File (File) loop
         -- Put_Line (Get_Line (File));
         declare
            A_Line : constant String := Get_Line (File);
         begin
            -- Parse the line into separate words.
            Parsed_Line := Parse_Line (A_Line, Words_Parsed);

            -- If there are words found lets look continue
            if Words_Parsed > 0 then
               -- Put_Line ("Found line:");
               -- for Word of Parsed_Line loop
               --    exit when Word (1) = ASCII.NUL;
               --    Put_Line (Word);
               -- end loop;

               -- Check first word:
               if Seq_Str_Cmp (Parsed_Line (0), "userCmd") then
                  if Words_Parsed < 4 then
                     Put_Line (Standard_Error, "Malformed command: '" & A_Line & "'");
                     raise Program_Error;
                  end if;

                  if Seq_Str_Cmp (Parsed_Line (2), "pattern") then
                     declare
                        Name : constant Seq_String := Parsed_Line (1);
                        Pattern_Str : constant Seq_String := Parsed_Line (3);
                        Pattern_Idx : Natural := Pattern_Str'First;
                        Command_Bytes : Command.Serialization.Byte_Array := [others => 0];
                        Command_Bytes_Idx : Natural := Command_Bytes'First;
                        The_Command : Command.T := (
                           Header => (
                              Source_Id => 0,
                              Id => 0,
                              Arg_Buffer_Length => 0
                           ),
                           Arg_Buffer => [others => 0]
                        );
                        Stat : Serialization_Status;
                     begin
                        -- Transform pattern string into an an array of bytes
                        while Pattern_Str (Pattern_Idx) /= ASCII.NUL loop
                           Command_Bytes (Command_Bytes_Idx) := Basic_Types.Byte'Value ("16#" & Pattern_Str (Pattern_Idx .. Pattern_Idx + 1) & "#");
                           Command_Bytes_Idx := Command_Bytes_Idx + 1;
                           Pattern_Idx := Pattern_Idx + 2;
                        end loop;

                        -- Deserialize array of bytes into command record
                        Stat := Command.Serialization.From_Byte_Array (Dest => The_Command, Src => Command_Bytes);
                        pragma Assert (Stat = Success);

                        -- Insert command definition into our commands data structure.
                        if Self.Commands.Contains (The_Command.Header.Id) then
                           -- Put_Line (Standard_Error, "Duplicate command found with ID: '" & Command_Types.Command_Id'Image (The_Command.Header.Id) & "'");
                           -- raise Program_Error;
                           exit;
                        else
                           Self.Commands.Include (The_Command.Header.Id, (
                              Name => Name,
                              Parameters => null, -- TODO
                              Command_Def => The_Command
                           ));
                        end if;
                     end;
                  end if;
               elsif Seq_Str_Cmp (Parsed_Line (0), "userTlm") then
                  if Words_Parsed < 4 then
                     Put_Line (Standard_Error, "Malformed telem: '" & A_Line & "'");
                     raise Program_Error;
                  end if;

                  if Seq_Str_Cmp (Parsed_Line (2), "details") then
                     declare
                        Name : constant Seq_String := Parsed_Line (1);
                        Id_Str : constant String := Parsed_Line (3);
                        -- Extract ID and offset
                        Id : constant Data_Product_Types.Data_Product_Id := Data_Product_Types.Data_Product_Id'Value ("16#" & Strip_Nul (Id_Str (Id_Str'First + 3 .. Id_Str'Last)) & "#");
                        Offset : constant Interfaces.Unsigned_16 := Interfaces.Unsigned_16'Value (Strip_Nul (Parsed_Line (5))); -- Need to slice this.
                        Key_To_Store : constant Interfaces.Unsigned_32 := Get_Telemetry_Key (Id, Offset);
                     begin
                        -- Store telemetry definition in telem data structure.
                        if Self.Telemetry.Contains (Key_To_Store) then
                           Put_Line (Standard_Error, "Duplicate telemetry found with ID: '" & Data_Product_Types.Data_Product_Id'Image (Id) & "'");
                           raise Program_Error;
                        else
                           Self.Telemetry.Include (Key_To_Store, (
                              Name => Name,
                              Id => Id
                           ));
                        end if;
                     end;
                  end if;
               elsif Seq_Str_Cmp (Parsed_Line (0), "sigFile") then
                  null;
               else
                  -- Put_Line ("Unrecognized config directive: '" & Parsed_Line (0) & "'");
                  null;
               end if;
            end if;
         end;
      end loop;

      -- Close the file:
      pragma Warnings (Off, """File"" modified by call, but value might not be referenced");
      Close (File);
      pragma Warnings (On, """File"" modified by call, but value might not be referenced");
   end Init;

   function Get_Command_Name (Self : in Instance; Id : in Command_Types.Command_Id) return String is
   begin
      if Self.Commands.Contains (Id) then
         return Strip_Nul (Self.Commands (Id).Name);
      else
         return "";
      end if;
   end Get_Command_Name;

   function Get_Telemetry_Name (Self : in Instance; Id : in Data_Product_Types.Data_Product_Id; Offset : in Interfaces.Unsigned_16) return String is
      To_Look_Up : constant Interfaces.Unsigned_32 := Get_Telemetry_Key (Id, Offset);
   begin
      if Self.Telemetry.Contains (To_Look_Up) then
         return Strip_Nul (Self.Telemetry (To_Look_Up).Name);
      else
         return "";
      end if;
   end Get_Telemetry_Name;

end Seq_Config;
