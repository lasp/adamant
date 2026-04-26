--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Base Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Includes:
with Ada.Calendar; use Ada.Calendar;
with String_Util;
with Test_Clock;

package body {{ name }} is
{% if component and not component.generic %}

   -- Instantiate tester allocator, uses heap for Linux but not for
   -- bareboard runtimes.
   package Tester_Alloc is new Tester_Allocator
     (Tester_Inst   => Component.{{ component.name }}.Implementation.Tester.Instance,
      Tester_Access => Component.{{ component.name }}.Implementation.Tester.Instance_Access);
{% endif %}

   ---------------------------------------
   -- Test Logging:
   ---------------------------------------

   procedure Log (Self : in out Base_Instance; String_To_Log : in String) is
   begin
      -- Make sure we have a log file to log to.
      Self.Logger.Log (String_Util.Trim_Both (String_To_Log));
   end Log;

   -- Initialize the logging for the log file ensuring the correct directory is available.
   -- File_Name on the host File_Logger body resolves it to a real on-disk path, while
   -- the bareboard variant routes Log to UART and ignores File_Name's path role.
   procedure Init_Logging (Self : in out Base_Instance; File_Name : in String) is
   begin
      -- Save the time to calculate the duration of the test later on
      Self.Start_Test_Time := Clock;
      -- Create the log file for the test
      Self.Logger.Open (File_Name);
      Self.Log ("    Beginning log for " & File_Name & Test_Clock.Now_Image);
   end Init_Logging;

   procedure End_Logging (Self : in out Base_Instance; File_Name : in String) is
      -- Now calculate the duration
      Test_Duration : constant Duration := Clock - Self.Start_Test_Time;
   begin
      -- Close the log that was used during this test.
      Self.Log ("    Ending log for " & File_Name
                & Test_Clock.Now_Image
                & " and took "
                & Test_Clock.Image_Duration (Test_Duration)
                & " seconds to run.");
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
      -- Acquire a Tester via the target-aware allocator (heap on Linux,
      -- static on bareboard) and wire its logger to ours.
      Self.Tester := Tester_Alloc.Allocate;
      -- Link the log access type to the logger in the reciprocal
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);
{% endif %}
      -- Call up to the implementation setup
      Base_Instance'Class (Self).Set_Up_Test;
      Self.Log ("    Finishing Set_Up for test " & Test_String);
   end Set_Up;

   overriding procedure Tear_Down (Self : in out Base_Instance) is
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
      -- Release the tester (heap free on Linux, no-op on bareboard).
      Tester_Alloc.Free (Self.Tester);
{% endif %}
   end Tear_Down;
end {{ name }};
