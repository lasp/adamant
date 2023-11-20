with Basic_Types;

package String_Util is
   -- Given a natural integer, return a hex string representation
   function Natural_2_Hex_String (Hex_Int : in Natural; Width : in Positive := 2) return String;

   -- Given an array of bytes, return a string representing the bytes in hex.
   function Bytes_To_String (Bytes : in Basic_Types.Byte_Array; Prefix : in String := "["; Postfix : in String := "]") return String;

   -- Given any type, return a string representing the bytes in hex.
   generic
      type T is private;
   function To_Byte_String (R : in T) return String;

   -- Given an array, return a string representing each element.
   generic
      type T is private;
      type I is range <>;
      type A is array (I) of T;
      with function Image (Item : in T) return String;
   function To_Array_String (R : in A; Show_Index : in Boolean := True) return String;

   function Trim_Both (Input : in String) return String;
   function Pad_Zeros (Source : in String; Num_Characters : in Natural) return String;
   function Replace_White_Space (Input_String : in String; Char_Replace : in Character := ' ') return String;
end String_Util;
