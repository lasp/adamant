package body Byte_Array_Pointer.Representation is

   -- Return string representation of record components
   function To_String (Self : in Instance; Prefix : String := "") return String is
      function Safe_Print return String is
         subtype Safe_Byte_Array_Type is Basic_Types.Byte_Array (0 .. Self.Length - 1);
         Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
      begin
         return Prefix & "Length : Natural = " & Trim_Both (Natural'Image (Self.Length)) & ASCII.LF & Prefix & "Address : System.Address = " & Trim_Both (Bytes_To_String (Safe_Byte_Array));
      end Safe_Print;
   begin
      if Is_Null (Self) then
         return Prefix & "Length : Natural = " & Trim_Both (Natural'Image (Self.Length)) & ASCII.LF & Prefix & "Address : System.Address = (null)";
      else
         return Safe_Print;
      end if;
   end To_String;

   function To_Tuple_String (Self : in Instance) return String is
      function Safe_Print return String is
         subtype Safe_Byte_Array_Type is Basic_Types.Byte_Array (0 .. Self.Length - 1);
         Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;
      begin
         return "(" & "Length = " & Trim_Both (Natural'Image (Self.Length)) & ", " & "Address = " & Trim_Both (Bytes_To_String (Safe_Byte_Array)) & ")";
      end Safe_Print;
   begin
      if Is_Null (Self) then
         return "(" & "Length = " & Trim_Both (Natural'Image (Self.Length)) & ", " & "Address = (null))";
      else
         return Safe_Print;
      end if;
   end To_Tuple_String;

   -- Return string representation of record components and bytes
   function Image_With_Prefix (Self : in Instance; Prefix : String := "") return String is
   begin
      return To_Byte_String (Self) & ASCII.LF & To_String (Self, Prefix);
   end Image_With_Prefix;

   function Image (Self : in Instance) return String is
   begin
      return Image_With_Prefix (Self, "");
   end Image;

end Byte_Array_Pointer.Representation;
