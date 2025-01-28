with Ada.Real_Time; use Ada.Real_Time;
with Sys_Time.Arithmetic;

-- Arithmetic package for Delta_Time
package Delta_Time.Arithmetic is

   -- Smallest and largest possible Delta_Times
   Max_Delta_Time : constant Delta_Time.T := (Seconds => Seconds_Type'Last, Subseconds => Subseconds_Type'Last);
   Zero_Delta_Time : constant Delta_Time.T := (Seconds => Seconds_Type'First, Subseconds => Subseconds_Type'First);

   -- Convert an Ada.Real_Time.Time_Span to a Delta_Time
   function To_Delta_Time (Arg_In : in Time_Span; Arg_Out : out Delta_Time.T) return Sys_Time.Arithmetic.Sys_Time_Status;

   -- Convert a Delta_Time to an Ada.Real_Time.Time_Span
   function To_Time_Span (Arg : in Delta_Time.T) return Time_Span;

   -- Usage Note:
   -- For any other operations, just work with Time_Spans and then
   -- convert back to Delta_Time if necessary.

end Delta_Time.Arithmetic;
