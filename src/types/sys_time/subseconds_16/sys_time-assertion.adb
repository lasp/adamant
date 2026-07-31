with Sys_Time.Arithmetic;

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

   -- Diagnostic text for a failed comparison. The difference and tolerance
   -- are rendered in seconds via To_Duration.
   function Failure_Message (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Time_Span; Message : in String) return String is
      ("Difference (s): " & Duration'Image (To_Duration (T1 - T2)) & ASCII.LF &
          "Eps (s): " & Duration'Image (To_Duration (Eps)) & ASCII.LF & Message);

   package body Sys_Time_Assert is
      procedure Eq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Default_Eps; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Only construct the diagnostic text when the assertion fails: it is
         -- not needed on success, and building it eagerly would run the time
         -- arithmetic on every call.
         if Sys_Time_Equal (T1, T2, Eps) then
            Sys_Time_Call_Assert (True, T1, T2, "=", Message, Filename, Line);
         else
            Sys_Time_Call_Assert (False, T1, T2, "=", Failure_Message (T1, T2, Eps, Message), Filename, Line);
         end if;
      end Eq;
      procedure Neq (T1 : in Sys_Time.T; T2 : in Sys_Time.T; Eps : in Ada.Real_Time.Time_Span := Default_Eps; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Only construct the diagnostic text when the assertion fails, as in
         -- Eq above.
         if Sys_Time_Equal (T1, T2, Eps) then
            Sys_Time_Call_Assert (False, T1, T2, "/=", Failure_Message (T1, T2, Eps, Message), Filename, Line);
         else
            Sys_Time_Call_Assert (True, T1, T2, "/=", Message, Filename, Line);
         end if;
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
