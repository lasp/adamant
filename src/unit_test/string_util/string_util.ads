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

   -- Given a floating point value, return an image with enough digits to read
   -- back as the same value.
   --
   -- 'Image prints 'Digits digits of the type it is given, rounded at the last
   -- digit. For T that is too few: the largest Long_Float rounds up past the
   -- largest double and reads back as infinity. So this function converts the
   -- value to Long_Long_Float, which does not change it, and takes the image
   -- of that. The only difference is that more digits are printed, 18 with the
   -- compilers of the development machine, where 17 are enough for a 64 bit
   -- float.
   --
   -- With a compiler whose Long_Long_Float is no wider than T, no extra digits
   -- are printed and the result is the same as T'Image.
   generic
      type T is digits <>;
   function Full_Precision_Image (Value : in T) return String;

   function Trim_Both (Input : in String) return String;
   function Pad_Zeros (Source : in String; Num_Characters : in Natural) return String;
   function Replace_White_Space (Input_String : in String; Char_Replace : in Character := ' ') return String;
end String_Util;
