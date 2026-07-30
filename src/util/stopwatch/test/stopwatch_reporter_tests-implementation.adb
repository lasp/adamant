--------------------------------------------------------------------------------
-- Stopwatch_Reporter Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Stopwatch.Reporter;
with Ada.Real_Time; use Ada.Real_Time;
with Task_Timing_Report;
with Delta_Time.Arithmetic; use Delta_Time.Arithmetic;
with Sys_Time.Arithmetic;

package body Stopwatch_Reporter_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helpers:
   -------------------------------------------------------------------------

   -- Fabricate a measurement with known wall and execution durations, as if
   -- Start and Stop had produced it, and accumulate it. The CPU time cannot
   -- be dilated deterministically in a unit test, so the stored results are
   -- set directly:
   procedure Accumulate_Measurement (Timer : in out Stopwatch.Reporter.Instance; Wall_Time : in Time_Span; Execution_Time : in Time_Span; Max_Wall_Time_Updated : out Boolean; Max_Execution_Time_Updated : out Boolean) is
   begin
      Timer.Last_Wall_Time := Wall_Time;
      Timer.Last_Execution_Time := Execution_Time;
      Timer.Accumulate (Max_Wall_Time_Updated => Max_Wall_Time_Updated, Max_Execution_Time_Updated => Max_Execution_Time_Updated);
   end Accumulate_Measurement;

   -- Same as above for tests that do not check the maximum-updated indications:
   procedure Accumulate_Measurement (Timer : in out Stopwatch.Reporter.Instance; Wall_Time : in Time_Span; Execution_Time : in Time_Span) is
      Ignore_Max_Wall, Ignore_Max_Execution : Boolean;
   begin
      Accumulate_Measurement (Timer, Wall_Time, Execution_Time, Max_Wall_Time_Updated => Ignore_Max_Wall, Max_Execution_Time_Updated => Ignore_Max_Execution);
   end Accumulate_Measurement;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Start_Stop (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Timer : Stopwatch.Reporter.Instance;
      Start_Time : constant Time := Clock;
   begin
      -- Start and stop with the wall times supplied externally. The wall
      -- measurement must match the supplied times exactly:
      Timer.Start (Wall_Start_Time => Start_Time);
      Timer.Stop (Wall_Stop_Time => Start_Time + Milliseconds (100));
      Assert (Timer.Last_Wall_Time = Milliseconds (100), "Expected an exact 100 ms wall measurement from externally supplied times.");
      -- The CPU measurement is real; it can only be checked for sanity:
      Assert (Timer.Last_Execution_Time >= Time_Span_Zero, "Expected a nonnegative execution time measurement.");

      -- Start and stop from the clock. Both measurements are real, so they
      -- can only be checked for sanity:
      Timer.Start;
      Timer.Stop;
      Assert (Timer.Last_Wall_Time >= Time_Span_Zero, "Expected a nonnegative wall time measurement.");
      Assert (Timer.Last_Execution_Time >= Time_Span_Zero, "Expected a nonnegative execution time measurement.");

      -- Nothing has been accumulated, so the maximums must be untouched:
      Assert (Timer.Max_Wall_Time = Time_Span_Zero, "Expected no accumulation from Start/Stop alone.");
      Assert (Timer.Recent_Max_Wall_Time = Time_Span_Zero, "Expected no accumulation from Start/Stop alone.");
   end Test_Start_Stop;

   overriding procedure Test_Accumulation (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Timer : Stopwatch.Reporter.Instance;
      Max_Wall_Updated, Max_Execution_Updated : Boolean;
   begin
      -- First measurement sets every value and updates both maximums:
      Accumulate_Measurement (Timer, Wall_Time => Milliseconds (10), Execution_Time => Milliseconds (5), Max_Wall_Time_Updated => Max_Wall_Updated, Max_Execution_Time_Updated => Max_Execution_Updated);
      Assert (Max_Wall_Updated, "Expected the maximum wall time to be updated by the first measurement.");
      Assert (Max_Execution_Updated, "Expected the maximum execution time to be updated by the first measurement.");
      Assert (Timer.Recent_Max_Wall_Time = Milliseconds (10), "Expected recent max wall time of 10 ms.");
      Assert (Timer.Max_Wall_Time = Milliseconds (10), "Expected max wall time of 10 ms.");
      Assert (Timer.Recent_Max_Execution_Time = Milliseconds (5), "Expected recent max execution time of 5 ms.");
      Assert (Timer.Max_Execution_Time = Milliseconds (5), "Expected max execution time of 5 ms.");

      -- Smaller wall time but larger execution time; only the execution
      -- maximums update:
      Accumulate_Measurement (Timer, Wall_Time => Milliseconds (8), Execution_Time => Milliseconds (6), Max_Wall_Time_Updated => Max_Wall_Updated, Max_Execution_Time_Updated => Max_Execution_Updated);
      Assert (not Max_Wall_Updated, "Expected the maximum wall time to not be updated by a smaller measurement.");
      Assert (Max_Execution_Updated, "Expected the maximum execution time to be updated by a larger measurement.");
      Assert (Timer.Recent_Max_Wall_Time = Milliseconds (10), "Expected recent max wall time to remain 10 ms.");
      Assert (Timer.Max_Wall_Time = Milliseconds (10), "Expected max wall time to remain 10 ms.");
      Assert (Timer.Recent_Max_Execution_Time = Milliseconds (6), "Expected recent max execution time of 6 ms.");
      Assert (Timer.Max_Execution_Time = Milliseconds (6), "Expected max execution time of 6 ms.");
   end Test_Accumulation;

   overriding procedure Test_Reports (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Timer : Stopwatch.Reporter.Instance;
      Report : Task_Timing_Report.T;
      Expected : Task_Timing_Report.T;
      Ignore : Sys_Time.Arithmetic.Sys_Time_Status;
      use type Task_Timing_Report.T;
   begin
      -- Accumulate a measurement, then reset the recent maximums and
      -- accumulate a smaller second, so that Max and Recent_Max differ:
      Accumulate_Measurement (Timer, Wall_Time => Milliseconds (10), Execution_Time => Milliseconds (5));
      Timer.Reset_Recent_Max;
      Accumulate_Measurement (Timer, Wall_Time => Milliseconds (4), Execution_Time => Milliseconds (2));

      -- Check the accumulated report:
      Ignore := To_Delta_Time (Milliseconds (10), Expected.Max.Wall_Time);
      Ignore := To_Delta_Time (Milliseconds (5), Expected.Max.Execution_Time);
      Ignore := To_Delta_Time (Milliseconds (4), Expected.Recent_Max.Wall_Time);
      Ignore := To_Delta_Time (Milliseconds (2), Expected.Recent_Max.Execution_Time);
      Report := Timer.Report;
      Assert (Report = Expected, "Expected the accumulated report to hold the maximum and recent maximum values.");

      -- Fabricate one more (smaller) measurement without accumulating and
      -- check the last-measurement report:
      Timer.Last_Wall_Time := Milliseconds (3);
      Timer.Last_Execution_Time := Milliseconds (1);
      Ignore := To_Delta_Time (Milliseconds (3), Expected.Recent_Max.Wall_Time);
      Ignore := To_Delta_Time (Milliseconds (1), Expected.Recent_Max.Execution_Time);
      Report := Timer.Report_Last;
      Assert (Report = Expected, "Expected the last-measurement report to hold the maximum and last values.");
   end Test_Reports;

   overriding procedure Test_Reset (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Timer : Stopwatch.Reporter.Instance;
   begin
      Accumulate_Measurement (Timer, Wall_Time => Milliseconds (10), Execution_Time => Milliseconds (5));

      -- Resetting the recent maximums must preserve everything else:
      Timer.Reset_Recent_Max;
      Assert (Timer.Recent_Max_Wall_Time = Time_Span_Zero, "Expected the recent max wall time to be reset.");
      Assert (Timer.Recent_Max_Execution_Time = Time_Span_Zero, "Expected the recent max execution time to be reset.");
      Assert (Timer.Max_Wall_Time = Milliseconds (10), "Expected the max wall time to be preserved.");
      Assert (Timer.Max_Execution_Time = Milliseconds (5), "Expected the max execution time to be preserved.");
      Assert (Timer.Last_Wall_Time = Milliseconds (10), "Expected the last wall time to be preserved.");
      Assert (Timer.Last_Execution_Time = Milliseconds (5), "Expected the last execution time to be preserved.");

      -- A full reset must clear everything:
      Timer.Reset;
      Assert (Timer.Recent_Max_Wall_Time = Time_Span_Zero, "Expected the recent max wall time to be reset.");
      Assert (Timer.Recent_Max_Execution_Time = Time_Span_Zero, "Expected the recent max execution time to be reset.");
      Assert (Timer.Max_Wall_Time = Time_Span_Zero, "Expected the max wall time to be reset.");
      Assert (Timer.Max_Execution_Time = Time_Span_Zero, "Expected the max execution time to be reset.");
      Assert (Timer.Last_Wall_Time = Time_Span_Zero, "Expected the last wall time to be reset.");
      Assert (Timer.Last_Execution_Time = Time_Span_Zero, "Expected the last execution time to be reset.");
   end Test_Reset;

end Stopwatch_Reporter_Tests.Implementation;
