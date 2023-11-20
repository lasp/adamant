with String_Util; use String_Util;

package body Basic_Types.Representation is

   -- Return string representation of record components
   function To_String (R : in Byte_Array; Prefix : String := "") return String is
   begin
      return Prefix & Bytes_To_String (R);
   end To_String;

   function To_Tuple_String (R : in Byte_Array; Ignore : String := ", ") return String is
   begin
      return To_String (R);
   end To_Tuple_String;

   -- Return string representation of record components and bytes
   function Image_With_Prefix (R : in Byte_Array; Prefix : String := "") return String is
   begin
      return To_String (R, Prefix);
   end Image_With_Prefix;

   function Image (R : in Byte_Array) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;

end Basic_Types.Representation;
