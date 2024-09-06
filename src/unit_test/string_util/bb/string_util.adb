with Serializer;

package body String_Util is

   function Natural_2_Hex_String (Hex_Int : in Natural; Width : in Positive := 2) return String is
      Ignore : Positive renames Width;
   begin
      return Natural'Image (Hex_Int);
   end Natural_2_Hex_String;

   -- Return string representation of bytes
   function Bytes_To_String (Bytes : in Basic_Types.Byte_Array; Prefix : in String := "["; Postfix : in String := "]") return String is
      Toreturn : String (1 .. Bytes'Length * 4);
      function Get_Number (Index : in Natural) return String is
         Temp : constant String := Natural'Image (Natural (Bytes (Index)));
      begin
         if Temp'Length = 1 then
            return "   " & Temp;
         elsif Temp'Length = 2 then
            return " " & Temp;
         elsif Temp'Length = 3 then
            return Temp;
         else
            return Temp ((Temp'Last - 2) .. Temp'Last);
         end if;
      end Get_Number;
      Cnt : Natural := 0;
   begin
      for Indx in Bytes'Range loop
         Toreturn ((Cnt * 4 + 1) .. (Cnt * 4 + 4)) := " " & Get_Number (Indx);
         Cnt := @ + 1;
      end loop;
      return Prefix & Trim_Both (Toreturn) & Postfix;
   end Bytes_To_String;

   -- Return string representation of records bytes
   function To_Byte_String (R : in T) return String is
      package Tserializer is new Serializer (T);
      Bytestring : constant Tserializer.Byte_Array := Tserializer.To_Byte_Array (R);
   begin
      return Bytes_To_String (Bytestring);
   end To_Byte_String;

   -- Return the string representation of an array
   function To_Array_String (R : in A; Show_Index : in Boolean := True) return String is
      pragma Unreferenced (Image);
      Ignore_R : A renames R;
      Ignore : Boolean renames Show_Index;
   begin
      return "Array string are not supported on this platform!";
   end To_Array_String;

   function Trim_Both (Input : in String) return String is
      -- Variables to store the indices of the first non-space character and the last non-space character
      Start_Index : Natural := Input'First;
      End_Index   : Natural := Input'Last;
   begin
      -- Find the first non-space character
      while Start_Index <= End_Index and then Input (Start_Index) = ' ' loop
         Start_Index := @ + 1;
      end loop;

      -- Find the last non-space character
      while End_Index >= Start_Index and then Input (End_Index) = ' ' loop
         End_Index := @ - 1;
      end loop;

      -- Return the trimmed substring
      return Input (Start_Index .. End_Index);
   end Trim_Both;

   function Pad_Zeros (Source : in String; Num_Characters : in Natural) return String is
      Ignore : Natural renames Num_Characters;
   begin
      return Source;
   end Pad_Zeros;

   function Replace_White_Space (Input_String : in String; Char_Replace : in Character := ' ') return String is
      Output_String : String (Input_String'Range);
   begin
      for Idx in Input_String'Range loop
         if Input_String (Idx) = ' ' or else Input_String (Idx) = ASCII.VT or else Input_String (Idx) = ASCII.HT or else Input_String (Idx) = ASCII.LF then
            Output_String (Idx) := Char_Replace;
         else
            Output_String (Idx) := Input_String (Idx);
         end if;
      end loop;
      return Output_String;
   end Replace_White_Space;

end String_Util;
