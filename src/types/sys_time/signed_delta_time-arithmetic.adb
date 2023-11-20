with Ada.Unchecked_Conversion;
with Delta_Time.Arithmetic;

-- Arithmetic package for Delta_Time
package body Signed_Delta_Time.Arithmetic is

   use Sys_Time.Arithmetic;
   use Delta_Time.Arithmetic;

   -- Conversion functions between time delta and sys_time. They have the same representation
   -- so this conversion should be fine. The two have different semantics, but using this conversion
   -- allows us to reuse code from the more complicated, but very well tested, Sys_Time.Arithmetic package.
   function Delta_Time_To_Sys_Time is new Ada.Unchecked_Conversion (Source => Delta_Time.T, Target => Sys_Time.T);

   function To_Signed_Delta_Time (Arg_In : in Time_Span; Arg_Out : out Signed_Delta_Time.T) return Sys_Time_Status is
      Out_Delta : Delta_Time.T;
      Status : constant Sys_Time_Status := To_Delta_Time (Arg_In, Out_Delta);
   begin
      if Arg_In >= Ada.Real_Time.Time_Span_Zero then
         Arg_Out := (Seconds => Out_Delta.Seconds, Subseconds => Out_Delta.Subseconds, Sign => Positive_Delta);
      else
         Arg_Out := (Seconds => Out_Delta.Seconds, Subseconds => Out_Delta.Subseconds, Sign => Negative_Delta);
      end if;

      return Status;
   exception
      when Constraint_Error => return Overflow;
   end To_Signed_Delta_Time;

   -- Convert a Delta_Time to an Ada.Real_Time.Time_Span
   function To_Time_Span (Arg : in Signed_Delta_Time.T) return Time_Span is
      -- Create a zero time
      In_Delta : constant Delta_Time.T := (Seconds => Arg.Seconds, Subseconds => Arg.Subseconds);
      Zero_Time : constant Time := Time_Of (0, Microseconds (0));
   begin
      case Arg.Sign is
         when Positive_Delta =>
            return To_Time (Delta_Time_To_Sys_Time (In_Delta)) - Zero_Time;
         when Negative_Delta =>
            return Zero_Time - To_Time (Delta_Time_To_Sys_Time (In_Delta));
      end case;
   end To_Time_Span;

end Signed_Delta_Time.Arithmetic;
