package Basic_Types.Representation is
   -- Return string representation of record components
   function To_String (R : in Byte_Array; Prefix : String := "") return String;
   -- Return string representation of record components in form (element 1, element 2, etc.)
   function To_Tuple_String (R : in Byte_Array; Ignore : String := ", ") return String;
   -- Return string representation of record components and bytes
   function Image_With_Prefix (R : in Byte_Array; Prefix : String := "") return String;
   -- This function form is required for Smart_Assert, but it is the same as above without the options:
   function Image (R : in Byte_Array) return String;
end Basic_Types.Representation;
