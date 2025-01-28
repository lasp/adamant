with GNAT.Source_Info;
with Ada.Real_Time;
with Smart_Assert;
with Sys_Time.Representation;

-- Smart assert for comparing GPS Times
-- Useful when you need to use the >, >=, <, and <= operators.
package Sys_Time.Assertion is
   package Sinfo renames GNAT.Source_Info;

   package Sys_Time_Assert is
      procedure Eq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Ada.Real_Time.Nanoseconds (300); Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Ada.Real_Time.Nanoseconds (300); Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Gt (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Ge (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Lt (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Le (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end Sys_Time_Assert;

   -- Specialized smart assert package for the fields in this record:
   package Seconds_Assert is new Smart_Assert.Basic (Seconds_Type, Sys_Time.Representation.Seconds_Image);
   package Subseconds_Assert is new Smart_Assert.Basic (Subseconds_Type, Sys_Time.Representation.Subseconds_Image);

end Sys_Time.Assertion;
