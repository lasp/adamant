with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Serializer;

package body String_Util is

   function Natural_2_Hex_String (Hex_Int : in Natural; Width : in Positive := 2) return String is
      Hex_Prefix_Length : constant := 3;
      Hexa : String (1 .. Hex_Prefix_Length + Width + 1);
      Result : String (1 .. Width);
      Start : Natural;
   begin
      Ada.Integer_Text_IO.Put (Hexa, Hex_Int, 16);
      Start := Ada.Strings.Fixed.Index (Source => Hexa, Pattern => "#");
      Ada.Strings.Fixed.Move (Source => Hexa (Start + 1 .. Hexa'Last - 1), Target => Result, Justify => Ada.Strings.Right, Pad => '0');
      return Result;
   end Natural_2_Hex_String;

   -- Return string representation of bytes
   function Bytes_To_String (Bytes : in Basic_Types.Byte_Array; Prefix : in String := "["; Postfix : in String := "]") return String is
      Toreturn : String (1 .. Bytes'Length * 3);
      Cnt : Natural := 0;
   begin
      for Indx in Bytes'Range loop
         Toreturn ((Cnt * 3 + 1) .. (Cnt * 3 + 3)) := " " & String_Util.Natural_2_Hex_String (Natural (Bytes (Indx)), 2);
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
      Str : Unbounded_String; -- Empty unbounded string
   begin
      for Idx in R'First .. (R'Last - 1) loop
         if Show_Index then
            Append (Str, ASCII.LF & Trim_Both (I'Image (Idx)) & " => " & Trim_Both (Image (R (Idx))) & ", ");
         else
            Append (Str, Trim_Both (Image (R (Idx))) & ", ");
         end if;
      end loop;
      if Show_Index then
         Append (Str, ASCII.LF & Trim_Both (I'Image (R'Last)) & " => " & Trim_Both (Image (R (R'Last))) & ASCII.LF);
      else
         Append (Str, Trim_Both (Image (R (R'Last))));
      end if;
      return To_String (Str);
   end To_Array_String;

   function Trim_Both (Input : in String) return String is
   begin
      return Trim (Input, Both);
   end Trim_Both;

   function Pad_Zeros (Source : in String; Num_Characters : in Natural) return String is
   begin
      return Tail (Source, Num_Characters, '0');
   end Pad_Zeros;

   function Replace_White_Space (Input_String : in String; Char_Replace : in Character := ' ') return String is
      Result : constant Unbounded_String := To_Unbounded_String (Input_String);
      Output_String : Unbounded_String;
      Space_Found : Boolean := False;
   begin
      for I in Input_String'Range loop
         if Element (Result, I) = ' ' or else Element (Result, I) = ASCII.VT or else Element (Result, I) = ASCII.HT or else Element (Result, I) = ASCII.LF then
            if not Space_Found then
               Space_Found := True;
               Append (Output_String, Char_Replace);
            end if;
         else
            Space_Found := False;
            Append (Output_String, Input_String (I));
         end if;
      end loop;
      return To_String (Output_String);
   end Replace_White_Space;

end String_Util;
