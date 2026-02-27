with Ada.Directories; use Ada.Directories;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with String_Util;

package body File_Logger is

   procedure Open (Self : in out Instance; File_Directory : in String) is
   begin
      -- Check that the directory exists first. If not, create it before creating the file
      if not Exists (Containing_Directory (File_Directory)) then
         Create_Directory (Containing_Directory (File_Directory));
      end if;

      -- Assert that the file is not yet open before attempting to open it
      pragma Assert (not Is_Open (Self.File), Simple_Name (File_Directory) & " is already open");
      -- Open the file we are going to write
      Create (Self.File, Out_File, File_Directory);
   end Open;

   procedure Log (Self : in Instance; String_To_Write : in String) is
      Utc_Epoch : constant Ada.Calendar.Time := Time_Of (Year => 1_970, Month => 1, Day => 1, Hour => 0, Minute => 0, Second => 0, Sub_Second => 0.0, Leap_Second => False);
      Time_Difference : constant Duration := (Clock - Utc_Epoch);
   begin
      -- Make sure the log is open before attempting to write to it
      if Is_Open (Self.File) then
         -- Then write what was passed in plus the UTC time
         Put_Line (Self.File, String_Util.Trim_Both (Duration'Image (Time_Difference)) & " " & String_To_Write);
      end if;
   end Log;

   procedure Close (Self : in out Instance) is
   begin
      if Is_Open (Self.File) then
         -- Close the file after logging is complete.
         Close (Self.File);
      end if;
   end Close;

end File_Logger;
