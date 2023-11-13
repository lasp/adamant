-- Standard includes:
with String_Util; use String_Util;

-- String representation package for Buffer
package Byte_Array_Pointer.Representation is

   -------------------------------------------------
   -- Common to string functions:
   -------------------------------------------------
   -- Return string showing bytes in record:
   function To_Byte_String is new String_Util.To_Byte_String (Instance);
   -- Display record as string:
   function Image (Self : in Instance) return String;

   -------------------------------------------------
   -- Less commonly used to string functions:
   -------------------------------------------------
   -- Return string representation of record fields in form (field 1, field 2, etc.)
   function To_Tuple_String (Self : in Instance) return String;
   -- Return string representation of record fields and bytes
   function Image_With_Prefix (Self : in Instance; Prefix : String := "") return String;
   -------------------------------------------------

end Byte_Array_Pointer.Representation;
