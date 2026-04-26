with String_Util;

package body Test_Clock is

   -- Bareboard has Ada.Calendar but not Ada.Calendar.Formatting, so
   -- there is no Image function for Time. Returning "" lets the same
   -- Init_Logging call sites compile and emit "Beginning log for X"
   -- without a timestamp suffix.
   function Now_Image return String is
   begin
      return "";
   end Now_Image;

   function Image_Duration (D : in Duration) return String is
   begin
      return String_Util.Trim_Both (Duration'Image (D));
   end Image_Duration;

end Test_Clock;
