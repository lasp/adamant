-- Target-aware test-time clock image. The host body returns the
-- current calendar time formatted via Ada.Calendar.Formatting.Image;
-- the bareboard runtime ships Ada.Calendar but not
-- Ada.Calendar.Formatting, so the bareboard body returns an empty
-- string. Used by the test scaffold to stamp logged events ("at
-- YYYY-MM-DD HH:MM:SS") on Linux while staying compilable cross.
package Test_Clock is

   -- Return " at <time-image>" on hosts that have
   -- Ada.Calendar.Formatting; "" on bareboard. The leading " at " is
   -- baked in so callers can append unconditionally and the bareboard
   -- log just omits the suffix.
   function Now_Image return String;

   -- Image of a previously-recorded duration in seconds. On hosts with
   -- Ada.Calendar.Formatting this just trims and concatenates; on
   -- bareboard the return value is the same shape but built with
   -- String_Util alone (no calendar dependency).
   function Image_Duration (D : in Duration) return String;

end Test_Clock;
