--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Base Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Includes:
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar.Formatting;
with String_Util;
{% if component and not component.generic %}
with Safe_Deallocator;
{% endif %}

package body {{ name }} is

   ---------------------------------------
   -- Test Logging:
   ---------------------------------------

   procedure Log (Self : in out Base_Instance; String_To_Log : in String) is
   begin
      -- Make sure we have a log file to log to.
      Self.Logger.Log (String_Util.Trim_Both (String_To_Log));
   end Log;

   -- Initialize the logging for the log file ensuring the correct directory is available
   procedure Init_Logging (Self : in out Base_Instance; File_Name : in String) is
      use Ada.Calendar.Formatting;
      -- The path returned by the Command_Name is the path to the test.elf, so back out to the build directory
      Log_Dir : constant String := Containing_Directory (Containing_Directory (Containing_Directory (Ada.Command_Line.Command_Name))) & "/log/" & File_Name & ".log";
   begin
      -- Save the time to calculate the duration of the test later on
      Self.Start_Test_Time := Clock;
      -- Create the log file for the test
      Self.Logger.Open (Log_Dir);
      Self.Log ("    Beginning log for " & File_Name & " at " & String_Util.Trim_Both (Image (Clock)));
   end Init_Logging;

   procedure End_Logging (Self : in out Base_Instance; File_Name : in String) is
      use Ada.Calendar.Formatting;
      -- Now calculate the duration
      Test_Duration : constant Duration := (Clock - Self.Start_Test_Time);
   begin
      -- Close the log that was used during this test.
      Self.Log ("    Ending log for " & File_Name & " at " & String_Util.Trim_Both (Image (Clock)) & " and took " & String_Util.Trim_Both (Duration'Image (Test_Duration)) & " seconds to run.");
      Self.Logger.Close;
   end End_Logging;

   ------------------------------------------------------------
   -- Fixtures:
   ------------------------------------------------------------

   overriding procedure Set_Up (Self : in out Base_Instance) is
      Test_String : constant String := To_String (Test_Name_List (Self.Test_Name_Index));
   begin
      -- Use the helper function to get the name of the test to setup
      Self.Init_Logging (Test_String);
      -- Log that we are starting to setup
      Self.Log ("    Starting Set_Up for test " & Test_String);
{% if component and not component.generic %}
      -- Dynamically allocate the component tester:
      Self.Tester := new Component.{{ component.name }}.Implementation.Tester.Instance;
      -- Link the log access type to the logger in the reciprocal
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);
{% endif %}
      -- Call up to the implementation setup
      Base_Instance'Class (Self).Set_Up_Test;
      Self.Log ("    Finishing Set_Up for test " & Test_String);
   end Set_Up;

   overriding procedure Tear_Down (Self : in out Base_Instance) is
{% if component and not component.generic %}
      procedure Free_Tester is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Component.{{ component.name }}.Implementation.Tester.Instance,
         Name => Component.{{ component.name }}.Implementation.Tester.Instance_Access
      );
{% endif %}
      Closing_Test : constant String := To_String (Test_Name_List (Self.Test_Name_Index));
   begin
      -- Log the tear down
      Self.Log ("    Starting Tear_Down for test " & Closing_Test);
      -- Call up to the implementation for any tear down
      Base_Instance'Class (Self).Tear_Down_Test;
      -- End the logging for the current test
      Self.Log ("    Finishing Tear_Down for test " & Closing_Test);
      Self.End_Logging (Closing_Test);
      -- Increment counter for the next test name in the list and pass the log to close back up to the tear down (or component unit test)
      Self.Test_Name_Index := @ + 1;
{% if component and not component.generic %}
      -- Delete tester:
      Free_Tester (Self.Tester);
{% endif %}
   end Tear_Down;
end {{ name }};
