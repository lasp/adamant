with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with String_Util;

package body Test_Clock is

   function Now_Image return String is
   begin
      return " at " & String_Util.Trim_Both (Image (Clock));
   end Now_Image;

   function Image_Duration (D : in Duration) return String is
   begin
      return String_Util.Trim_Both (Duration'Image (D));
   end Image_Duration;

end Test_Clock;
