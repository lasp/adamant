with GNAT.Source_Info;

package Byte_Array_Pointer.Assertion is

   package Sinfo renames GNAT.Source_Info;

   -- Special assertion package for comparing a byte array pointer against an
   -- actual byte array. This compares the length and the data:
   package Byte_Array_Pointer_Assert is
      procedure Eq (T1 : in Instance; T2 : in Basic_Types.Byte_Array; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in Instance; T2 : in Basic_Types.Byte_Array; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end Byte_Array_Pointer_Assert;

end Byte_Array_Pointer.Assertion;
