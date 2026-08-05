with Task_Timing_Report;
with Ada.Task_Identification;

-- A paired wall-clock and CPU-execution stopwatch for timing a section of
-- code. The reporter accumulates recent-maximum and all-time-maximum (high
-- water mark) values for both timers, and produces reports of the
-- accumulated values as a Task_Timing_Report.T, suitable for publishing as
-- a data product.
package Stopwatch.Reporter with SPARK_Mode => On is

   use type Ada.Real_Time.Time_Span;

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

   -- Ghost predicates used to express the postconditions below. Ghost code
   -- is for proof only; it is not compiled into the executable. Ada requires
   -- these to be declared before the contracts that reference them; their
   -- definitions are tucked away in the private part at the bottom of this
   -- file:
   function Results_Unchanged (Left : in Instance; Right : in Instance) return Boolean with Ghost;
   function Accumulators_Unchanged (Left : in Instance; Right : in Instance) return Boolean with Ghost;
   function Recent_Max_Is_Zero (Self : in Instance) return Boolean with Ghost;
   function Is_Reset (Self : in Instance) return Boolean with Ghost;
   function Recent_Max_Reset (Result : in Instance; Prior : in Instance) return Boolean with Ghost;
   function Maximums_Accumulated (Result : in Instance; Prior : in Instance) return Boolean with Ghost;

   -- Start both the wall and CPU timers now. The wall timer is started
   -- first, so that the timing bookkeeping itself is excluded from the CPU
   -- measurement:
   procedure Start (Self : in out Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Results_Unchanged (Self, Self'Old) and then
                   Accumulators_Unchanged (Self, Self'Old) and then
                   Self.Wall_Timer.Stop_Time = Self.Wall_Timer.Stop_Time'Old and then
                   Self.Cpu_Timer.Stop_Time = Self.Cpu_Timer.Stop_Time'Old;
   -- Start both timers, with the wall timer's start time supplied by the
   -- caller instead of read from the clock. This is useful to backdate the
   -- wall measurement, for example to the timestamp of an incoming tick so
   -- that queue latency is included in the measurement:
   procedure Start (Self : in out Instance; Wall_Start_Time : in Ada.Real_Time.Time)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Results_Unchanged (Self, Self'Old) and then
                   Accumulators_Unchanged (Self, Self'Old) and then
                   Self.Wall_Timer.Start_Time = Wall_Start_Time and then
                   Self.Wall_Timer.Stop_Time = Self.Wall_Timer.Stop_Time'Old and then
                   Self.Cpu_Timer.Stop_Time = Self.Cpu_Timer.Stop_Time'Old;
   -- Stop both timers now and store the measurement results. The CPU timer
   -- is stopped first, so that the timing bookkeeping itself is excluded
   -- from the CPU measurement:
   procedure Stop (Self : in out Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Accumulators_Unchanged (Self, Self'Old) and then
                   Self.Last_Wall_Time = Self.Wall_Timer.Result and then
                   Self.Last_Execution_Time = Self.Cpu_Timer.Result and then
                   Self.Wall_Timer.Start_Time = Self.Wall_Timer.Start_Time'Old and then
                   Self.Cpu_Timer.Start_Time = Self.Cpu_Timer.Start_Time'Old;
   -- Stop both timers, with the wall timer's stop time supplied by the
   -- caller instead of read from the clock. Note that the provided wall stop
   -- time is necessarily acquired before the CPU timer stops; if that
   -- acquisition is expensive, use the split subprograms below instead:
   procedure Stop (Self : in out Instance; Wall_Stop_Time : in Ada.Real_Time.Time)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Accumulators_Unchanged (Self, Self'Old) and then
                   Self.Wall_Timer.Stop_Time = Wall_Stop_Time and then
                   Self.Last_Wall_Time = Self.Wall_Timer.Result and then
                   Self.Last_Execution_Time = Self.Cpu_Timer.Result and then
                   Self.Wall_Timer.Start_Time = Self.Wall_Timer.Start_Time'Old and then
                   Self.Cpu_Timer.Start_Time = Self.Cpu_Timer.Start_Time'Old;
   -- The two halves of Stop, exposed so that the CPU timer can be stopped
   -- before the wall clock stop time is acquired, when that acquisition is
   -- itself expensive (e.g. a system time fetched through a connector). Call
   -- Stop_Cpu_Timer first, acquire the wall stop time, and then call
   -- Stop_Wall_Timer. Each stops its timer and stores that timer's
   -- measurement result:
   procedure Stop_Cpu_Timer (Self : in out Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Accumulators_Unchanged (Self, Self'Old) and then
                   Self.Last_Execution_Time = Self.Cpu_Timer.Result and then
                   Self.Cpu_Timer.Start_Time = Self.Cpu_Timer.Start_Time'Old and then
                   Self.Wall_Timer = Self.Wall_Timer'Old and then
                   Self.Last_Wall_Time = Self.Last_Wall_Time'Old;
   procedure Stop_Wall_Timer (Self : in out Instance; Wall_Stop_Time : in Ada.Real_Time.Time)
      with Global => null,
           Post => Accumulators_Unchanged (Self, Self'Old) and then
                   Self.Wall_Timer.Stop_Time = Wall_Stop_Time and then
                   Self.Last_Wall_Time = Self.Wall_Timer.Result and then
                   Self.Wall_Timer.Start_Time = Self.Wall_Timer.Start_Time'Old and then
                   Self.Cpu_Timer = Self.Cpu_Timer'Old and then
                   Self.Last_Execution_Time = Self.Last_Execution_Time'Old;
   -- Fold the results of the most recent Stop into the recent-maximum and
   -- maximum accumulators. The max-folding math is captured by the
   -- Maximums_Accumulated ghost predicate defined in the private part:
   procedure Accumulate (Self : in out Instance)
      with Global => null,
           Post => Maximums_Accumulated (Self, Self'Old);
   -- Same as above, but additionally reports whether either all-time maximum
   -- value was updated, which is useful for issuing time-exceeded events:
   procedure Accumulate (Self : in out Instance; Max_Wall_Time_Updated : out Boolean; Max_Execution_Time_Updated : out Boolean)
      with Global => null,
           Post => Maximums_Accumulated (Self, Self'Old) and then
                   Max_Wall_Time_Updated = (Self.Last_Wall_Time > Self'Old.Max_Wall_Time) and then
                   Max_Execution_Time_Updated = (Self.Last_Execution_Time > Self'Old.Max_Execution_Time);
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
      with Global => null,
           Post => Recent_Max_Reset (Self, Self'Old);
   -- Reset all stored values, including the all-time maximums. Any
   -- in-progress measurement (a Start without a Stop) is also discarded:
   procedure Reset (Self : out Instance)
      with Global => null,
           Post => Is_Reset (Self);

private

   -- Definitions of the ghost predicates declared above.
   --
   -- The stored results of the most recent Stop are the same in both
   -- instances:
   function Results_Unchanged (Left : in Instance; Right : in Instance) return Boolean is
      (Left.Last_Wall_Time = Right.Last_Wall_Time and then
       Left.Last_Execution_Time = Right.Last_Execution_Time);

   -- The accumulated recent-maximum and maximum values are the same in both
   -- instances:
   function Accumulators_Unchanged (Left : in Instance; Right : in Instance) return Boolean is
      (Left.Recent_Max_Wall_Time = Right.Recent_Max_Wall_Time and then
       Left.Max_Wall_Time = Right.Max_Wall_Time and then
       Left.Recent_Max_Execution_Time = Right.Recent_Max_Execution_Time and then
       Left.Max_Execution_Time = Right.Max_Execution_Time);

   -- Both recent-maximum values are zero:
   function Recent_Max_Is_Zero (Self : in Instance) return Boolean is
      (Self.Recent_Max_Wall_Time = Ada.Real_Time.Time_Span_Zero and then
       Self.Recent_Max_Execution_Time = Ada.Real_Time.Time_Span_Zero);

   -- Every field holds its default (freshly initialized) value:
   function Is_Reset (Self : in Instance) return Boolean is
      (Self.Wall_Timer.Start_Time = Ada.Real_Time.Time_First and then
       Self.Wall_Timer.Stop_Time = Ada.Real_Time.Time_First and then
       Self.Cpu_Timer.Start_Time = Ada.Execution_Time.CPU_Time_First and then
       Self.Cpu_Timer.Stop_Time = Ada.Execution_Time.CPU_Time_First and then
       Self.Last_Wall_Time = Ada.Real_Time.Time_Span_Zero and then
       Self.Last_Execution_Time = Ada.Real_Time.Time_Span_Zero and then
       Self.Recent_Max_Wall_Time = Ada.Real_Time.Time_Span_Zero and then
       Self.Max_Wall_Time = Ada.Real_Time.Time_Span_Zero and then
       Self.Recent_Max_Execution_Time = Ada.Real_Time.Time_Span_Zero and then
       Self.Max_Execution_Time = Ada.Real_Time.Time_Span_Zero);

   -- Result is Prior with both recent-maximum values reset to zero and
   -- every other field unchanged:
   function Recent_Max_Reset (Result : in Instance; Prior : in Instance) return Boolean is
      (Recent_Max_Is_Zero (Result) and then
       Results_Unchanged (Result, Prior) and then
       Result.Wall_Timer = Prior.Wall_Timer and then
       Result.Cpu_Timer = Prior.Cpu_Timer and then
       Result.Max_Wall_Time = Prior.Max_Wall_Time and then
       Result.Max_Execution_Time = Prior.Max_Execution_Time);

   -- Result is Prior with the most recent measurement folded into the
   -- recent-maximum and maximum accumulators: each maximum becomes the
   -- larger of its prior value and the measurement, and nothing else
   -- changes:
   function Maximums_Accumulated (Result : in Instance; Prior : in Instance) return Boolean is
      (Results_Unchanged (Result, Prior) and then
       Result.Wall_Timer = Prior.Wall_Timer and then
       Result.Cpu_Timer = Prior.Cpu_Timer and then
       Result.Recent_Max_Wall_Time = (if Result.Last_Wall_Time > Prior.Recent_Max_Wall_Time then Result.Last_Wall_Time else Prior.Recent_Max_Wall_Time) and then
       Result.Max_Wall_Time = (if Result.Last_Wall_Time > Prior.Max_Wall_Time then Result.Last_Wall_Time else Prior.Max_Wall_Time) and then
       Result.Recent_Max_Execution_Time = (if Result.Last_Execution_Time > Prior.Recent_Max_Execution_Time then Result.Last_Execution_Time else Prior.Recent_Max_Execution_Time) and then
       Result.Max_Execution_Time = (if Result.Last_Execution_Time > Prior.Max_Execution_Time then Result.Last_Execution_Time else Prior.Max_Execution_Time));

end Stopwatch.Reporter;
