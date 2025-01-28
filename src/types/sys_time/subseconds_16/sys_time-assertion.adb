with Sys_Time.Arithmetic;
with Delta_Time.Arithmetic;

-- Smart assert for comparing system times
-- Useful when you need to use the >, >=, <, and <= operators.
package body Sys_Time.Assertion is
   use Ada.Real_Time;
   use Sys_Time.Arithmetic;

   procedure Sys_Time_Call_Assert is new Smart_Assert.Call_Assert (Sys_Time.T, Sys_Time.Representation.Image);

   -- Check time equality with a tolerance because of small conversion errors
   function Sys_Time_Equal (Time1 : Sys_Time.T; Time2 : Sys_Time.T; Eps : Time_Span) return Boolean is
      -- absolute value of time difference
      Time_Diff : Ada.Real_Time.Time_Span;
   begin
      Time_Diff := abs (Time1 - Time2);

      -- Check if difference is less than the tolerance
      if Time_Diff <= Eps then
         return True;
      else
         return False;
      end if;
   end Sys_Time_Equal;

   package body Sys_Time_Assert is
      procedure Eq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Ada.Real_Time.Nanoseconds (300); Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
         use Delta_Time.Arithmetic;
         Td : Delta_Time.T;
         Ignore : Sys_Time_Status := To_Delta_Time (Eps, Td);
         Message_Text : constant String :=
            "Difference (nsec): " & Integer'Image ((T1 - T2) / Nanoseconds (1)) & ASCII.LF & "eps: (Subseconds: " & Td.Subseconds'Image & " Nanoseconds: " & Integer'Image (Eps / Nanoseconds (1)) & ") " & ASCII.LF & Message;
      begin
         -- Check equality using Sys_Time_Equal
         Sys_Time_Call_Assert (Sys_Time_Equal (T1, T2, Eps), T1, T2, "=", Message_Text, Filename, Line);
      end Eq;
      procedure Neq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Ada.Real_Time.Nanoseconds (300); Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
         use Delta_Time.Arithmetic;
         Td : Delta_Time.T;
         Ignore : Sys_Time_Status := To_Delta_Time (Eps, Td);
         Message_Text : constant String :=
            "Difference (nsec): " & Integer'Image ((T1 - T2) / Nanoseconds (1)) & ASCII.LF & "eps: (Subseconds: " & Td.Subseconds'Image & " Nanoseconds: " & Integer'Image (Eps / Nanoseconds (1)) & ") " & ASCII.LF & Message;
      begin
         -- Check equality using Sys_Time_Equal
         Sys_Time_Call_Assert (not Sys_Time_Equal (T1, T2, Eps), T1, T2, "/=", Message_Text, Filename, Line);
      end Neq;
      procedure Gt (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Sys_Time_Call_Assert ((T1 > T2), T1, T2, ">", Message, Filename, Line);
      end Gt;
      procedure Ge (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Sys_Time_Call_Assert ((T1 >= T2), T1, T2, ">=", Message, Filename, Line);
      end Ge;
      procedure Lt (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Sys_Time_Call_Assert ((T1 < T2), T1, T2, "<", Message, Filename, Line);
      end Lt;
      procedure Le (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Sys_Time_Call_Assert ((T1 <= T2), T1, T2, "<=", Message, Filename, Line);
      end Le;
   end Sys_Time_Assert;

end Sys_Time.Assertion;
