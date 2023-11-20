with Basic_Assertions; use Basic_Assertions;

package body Byte_Array_Pointer.Assertion is

   package body Byte_Array_Pointer_Assert is

      procedure Eq (T1 : in Instance; T2 : in Basic_Types.Byte_Array; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
         subtype Safe_Byte_Array_Type is Basic_Types.Byte_Array (0 .. T1.Length - 1);
         Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => T1.Address;
      begin
         Byte_Array_Assert.Eq (Safe_Byte_Array, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in Instance; T2 : in Basic_Types.Byte_Array; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
         subtype Safe_Byte_Array_Type is Basic_Types.Byte_Array (0 .. T1.Length - 1);
         Safe_Byte_Array : constant Safe_Byte_Array_Type with Import, Convention => Ada, Address => T1.Address;
      begin
         Byte_Array_Assert.Neq (Safe_Byte_Array, T2, Message, Filename, Line);
      end Neq;

   end Byte_Array_Pointer_Assert;

end Byte_Array_Pointer.Assertion;
