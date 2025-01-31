with Ada.Real_Time; use Ada.Real_Time;

-- Arithmetic package for Sys_Time
package Sys_Time.Arithmetic is

   -- Smallest and largest possible Sys_Times
   Sys_Time_Max : constant Sys_Time.T := (Seconds => Seconds_Type'Last, Subseconds => Subseconds_Type'Last);
   Sys_Time_Zero : constant Sys_Time.T := (Seconds => Seconds_Type'First, Subseconds => Subseconds_Type'First);
   Sys_Time_One : constant Sys_Time.T := (Seconds => 1, Subseconds => Subseconds_Type'First);

   -- Return status types:
   type Sys_Time_Status is (Success, Underflow, Overflow);
   for Sys_Time_Status use (Success => 0, Underflow => 1, Overflow => 2);

   -- Convert an Ada.Real_Time.Time to a Sys_Time
   -- This can result in an under_flow if the Time sent is negative. In that case an under_flow status will be sent and with a time of zero which is the earliest time available
   function To_Sys_Time (Arg_In : in Time; Arg_Out : out Sys_Time.T) return Sys_Time_Status;

   -- Convert a Sys_Time to an Ada.Real_Time.Time
   function To_Time (Arg : in Sys_Time.T) return Time;

   -- Add a system time and a time span and return a system time
   function Add (Left : in Sys_Time.T; Right : in Time_Span; Result : out Sys_Time.T) return Sys_Time_Status;
   function Add (Left : in Time_Span; Right : in Sys_Time.T; Result : out Sys_Time.T) return Sys_Time_Status;

   -- Subtract one time from another and return a time span, or subtract a time_span from a time and return a Subtract_Time_Span_Status
   -- Result is returned in Result : Sys_Time.T because subtracting a Time_Span from a Sys_Time could result in a Under_Flow meaning a time less than 0
   -- This is not representable in a Sys_Time so a status must be returned and check when using this function
   function Subtract (Left : Sys_Time.T; Right : Time_Span; Result : out Sys_Time.T) return Sys_Time_Status;

   function "-" (Left : Sys_Time.T; Right : Sys_Time.T) return Time_Span with
      Global => null;

      -- Less than check
   function "<" (Left, Right : Sys_Time.T) return Boolean with
      Global => null;

      -- Less than or equal to check
   function "<=" (Left, Right : Sys_Time.T) return Boolean with
      Global => null;

      -- Greater than check
   function ">" (Left, Right : Sys_Time.T) return Boolean with
      Global => null;

      -- Greater than or equal to check
   function ">=" (Left, Right : Sys_Time.T) return Boolean with
      Global => null;

end Sys_Time.Arithmetic;
