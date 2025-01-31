with Ada.Unchecked_Conversion;

-- Arithmetic package for Delta_Time
package body Delta_Time.Arithmetic is

   use Sys_Time.Arithmetic;

   -- Conversion functions between time delta and sys_time. They have the same representation
   -- so this conversion should be fine. The two have different semantics, but using this conversion
   -- allows us to reuse code from the more complicated, but very well tested, Sys_Time.Arithmetic package.
   function Delta_Time_To_Sys_Time is new Ada.Unchecked_Conversion (Source => Delta_Time.T, Target => Sys_Time.T);
   function Sys_Time_To_Delta_Time is new Ada.Unchecked_Conversion (Source => Sys_Time.T, Target => Delta_Time.T);

   function To_Delta_Time (Arg_In : in Time_Span; Arg_Out : out Delta_Time.T) return Sys_Time_Status is
      -- Convert from time span to an absolute time, time spans can be negative, sys_time and Delta_Time cannot
      The_Time : Time;
      Out_Time : Sys_Time.T;
      -- Convert from time to Sys_Time
      Status : Sys_Time_Status;
   begin
      if Arg_In = Time_Span_First then
         -- Special case for Time_Span_First, which would overflow abs if made positive. In this
         -- case we round to the nearest positive time span value.
         The_Time := Time_Of (Seconds_Count (0), Time_Span_Last);
      else
         -- Convert from time span to an absolute time, time spans can be negative, sys_time and Delta_Time cannot
         The_Time := Time_Of (Seconds_Count (0), abs (Arg_In));
      end if;

      -- Convert from time to Sys_Time
      Status := To_Sys_Time (The_Time, Out_Time);

      if Status = Success then
         -- Convert to Delta_Time and return status
         Arg_Out := Sys_Time_To_Delta_Time (Out_Time);
      end if;

      return Status;
   exception
      when Constraint_Error => return Overflow;
   end To_Delta_Time;

   -- Convert a Delta_Time to an Ada.Real_Time.Time_Span
   function To_Time_Span (Arg : in Delta_Time.T) return Time_Span is
      -- Create a zero time
      Zero_Time : constant Time := Time_Of (0, Microseconds (0));
   begin
      return To_Time (Delta_Time_To_Sys_Time (Arg)) - Zero_Time;
   end To_Time_Span;

end Delta_Time.Arithmetic;
