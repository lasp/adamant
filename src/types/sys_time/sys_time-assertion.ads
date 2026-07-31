with GNAT.Source_Info;
with Ada.Real_Time;
with Smart_Assert;
with Sys_Time.Representation;

-- Smart assert for comparing GPS Times
-- Useful when you need to use the >, >=, <, and <= operators.
package Sys_Time.Assertion is
   package Sinfo renames GNAT.Source_Info;

   use type Ada.Real_Time.Time_Span;

   -- The default tolerance used when comparing two times for (in)equality.
   -- Two times are considered equal when their difference is less than or
   -- equal to the tolerance. The default is the largest of:
   --   1. Half a subsecond LSB - differences below this are representational
   --      rounding of Sys_Time.T itself, not meaningful time differences.
   --   2. 300 nanoseconds - allowance for Sys_Time to Ada.Real_Time
   --      conversion error.
   --   3. One Time_Span_Unit - the runtime's clock resolution, so the
   --      tolerance never rounds to zero on targets with a coarse tick.
   -- Note that Neq with the default tolerance asserts that two times differ
   -- by MORE than the tolerance.
   function Time_Span_Max (Left : in Ada.Real_Time.Time_Span; Right : in Ada.Real_Time.Time_Span) return Ada.Real_Time.Time_Span is
      (if Left > Right then Left else Right);
   Default_Eps : constant Ada.Real_Time.Time_Span :=
      Time_Span_Max (
         Time_Span_Max (
            Ada.Real_Time.To_Time_Span (Duration (0.5 / Long_Long_Float (Subseconds_Type'Modulus))),
            Ada.Real_Time.Nanoseconds (300)
         ),
         Ada.Real_Time.Time_Span_Unit
      );

   package Sys_Time_Assert is
      procedure Eq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Default_Eps; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Default_Eps; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Gt (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Ge (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Lt (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Le (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end Sys_Time_Assert;

   -- Specialized smart assert package for the fields in this record:
   package Seconds_Assert is new Smart_Assert.Basic (Seconds_Type, Sys_Time.Representation.Seconds_Image);
   package Subseconds_Assert is new Smart_Assert.Basic (Subseconds_Type, Sys_Time.Representation.Subseconds_Image);

end Sys_Time.Assertion;
