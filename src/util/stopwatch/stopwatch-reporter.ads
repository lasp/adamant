with Task_Timing_Report;
with Ada.Task_Identification;

-- A paired wall-clock and CPU-execution stopwatch for timing a section of
-- code. The reporter accumulates recent-maximum and all-time-maximum (high
-- water mark) values for both timers, and produces reports of the
-- accumulated values as a Task_Timing_Report.T, suitable for publishing as
-- a data product.
package Stopwatch.Reporter with SPARK_Mode => On is

   type Instance is tagged record
      -- The underlying stopwatch pair. These are exposed so that users may
      -- manipulate the start and stop times directly for cases the Start and
      -- Stop subprograms below do not cover.
      Wall_Timer : Wall_Timer_Instance;
      Cpu_Timer : Cpu_Timer_Instance;
      -- The results of the most recent Stop:
      Last_Wall_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      Last_Execution_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      -- The accumulated maximum values:
      Recent_Max_Wall_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      Max_Wall_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      Recent_Max_Execution_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      Max_Execution_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
   end record;

   -- Start both the wall and CPU timers now. The wall timer is started
   -- first, so that the timing bookkeeping itself is excluded from the CPU
   -- measurement:
   procedure Start (Self : in out Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State));
   -- Start both timers, with the wall timer's start time supplied by the
   -- caller instead of read from the clock. This is useful to backdate the
   -- wall measurement, for example to the timestamp of an incoming tick so
   -- that queue latency is included in the measurement:
   procedure Start (Self : in out Instance; Wall_Start_Time : in Ada.Real_Time.Time)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State));
   -- Stop both timers now and store the measurement results. The CPU timer
   -- is stopped first, so that the timing bookkeeping itself is excluded
   -- from the CPU measurement:
   procedure Stop (Self : in out Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State));
   -- Stop both timers, with the wall timer's stop time supplied by the
   -- caller instead of read from the clock. Note that the provided wall stop
   -- time is necessarily acquired before the CPU timer stops; if that
   -- acquisition is expensive, use the split subprograms below instead:
   procedure Stop (Self : in out Instance; Wall_Stop_Time : in Ada.Real_Time.Time)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State));
   -- The two halves of Stop, exposed so that the CPU timer can be stopped
   -- before the wall clock stop time is acquired, when that acquisition is
   -- itself expensive (e.g. a system time fetched through a connector). Call
   -- Stop_Cpu_Timer first, acquire the wall stop time, and then call
   -- Stop_Wall_Timer. Each stops its timer and stores that timer's
   -- measurement result:
   procedure Stop_Cpu_Timer (Self : in out Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State));
   procedure Stop_Wall_Timer (Self : in out Instance; Wall_Stop_Time : in Ada.Real_Time.Time)
      with Global => null;
   -- Fold the results of the most recent Stop into the recent-maximum and
   -- maximum accumulators:
   procedure Accumulate (Self : in out Instance)
      with Global => null;
   -- Same as above, but additionally reports whether either all-time maximum
   -- value was updated, which is useful for issuing time-exceeded events:
   procedure Accumulate (Self : in out Instance; Max_Wall_Time_Updated : out Boolean; Max_Execution_Time_Updated : out Boolean)
      with Global => null;
   -- Produce a report of the currently accumulated values:
   function Report (Self : in Instance) return Task_Timing_Report.T
      with Global => null;
   -- Produce a report with the Recent_Max fields holding the results of the
   -- most recent Stop instead of the recent maximums. This is useful for
   -- per-operation reporting, where each report carries the timing of the
   -- operation just performed alongside the all-time maximums:
   function Report_Last (Self : in Instance) return Task_Timing_Report.T
      with Global => null;
   -- Reset only the recent-maximum values. This is intended to be called
   -- after each report is published so that the recent maximums cover a
   -- single reporting period:
   procedure Reset_Recent_Max (Self : in out Instance)
      with Global => null;
   -- Reset all stored values, including the all-time maximums. Any
   -- in-progress measurement (a Start without a Stop) is also discarded:
   procedure Reset (Self : out Instance)
      with Global => null;

end Stopwatch.Reporter;
