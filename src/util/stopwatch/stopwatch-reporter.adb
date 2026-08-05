with Delta_Time.Arithmetic;
with Sys_Time.Arithmetic;

package body Stopwatch.Reporter with SPARK_Mode => On is

   procedure Start (Self : in out Instance) is
      -- SPARK requires a volatile function like Clock to be read into an
      -- object directly rather than passed as an actual. The wall time is
      -- sampled before the CPU timer starts, preserving the wall-first
      -- ordering:
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      Self.Start (Wall_Start_Time => Now);
   end Start;

   procedure Start (Self : in out Instance; Wall_Start_Time : in Ada.Real_Time.Time) is
   begin
      Self.Wall_Timer.Start_Time := Wall_Start_Time;
      Self.Cpu_Timer.Start;
   end Start;

   procedure Stop (Self : in out Instance) is
   begin
      -- Stop the CPU timer first so the wall clock read below is excluded
      -- from the CPU measurement. SPARK requires a volatile function like
      -- Clock to be read into an object directly rather than passed as an
      -- actual, so the read gets a declare block after the CPU stop:
      Self.Stop_Cpu_Timer;
      declare
         Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      begin
         Self.Stop_Wall_Timer (Wall_Stop_Time => Now);
      end;
   end Stop;

   procedure Stop (Self : in out Instance; Wall_Stop_Time : in Ada.Real_Time.Time) is
   begin
      Self.Stop_Cpu_Timer;
      Self.Stop_Wall_Timer (Wall_Stop_Time => Wall_Stop_Time);
   end Stop;

   procedure Stop_Cpu_Timer (Self : in out Instance) is
   begin
      Self.Cpu_Timer.Stop;
      Self.Last_Execution_Time := Self.Cpu_Timer.Result;
   end Stop_Cpu_Timer;

   procedure Stop_Wall_Timer (Self : in out Instance; Wall_Stop_Time : in Ada.Real_Time.Time) is
   begin
      Self.Wall_Timer.Stop_Time := Wall_Stop_Time;
      Self.Last_Wall_Time := Self.Wall_Timer.Result;
   end Stop_Wall_Timer;

   procedure Accumulate (Self : in out Instance) is
      Ignore_Max_Wall, Ignore_Max_Execution : Boolean;
   begin
      Self.Accumulate (Max_Wall_Time_Updated => Ignore_Max_Wall, Max_Execution_Time_Updated => Ignore_Max_Execution);
   end Accumulate;

   procedure Accumulate (Self : in out Instance; Max_Wall_Time_Updated : out Boolean; Max_Execution_Time_Updated : out Boolean) is
      use Ada.Real_Time;

      -- Fold a measured value into a recent-maximum and maximum pair,
      -- reporting whether the maximum was updated:
      procedure Update_Maximums (Value : in Time_Span; Recent_Max : in out Time_Span; Max : in out Time_Span; Updated : out Boolean)
         with Global => null,
              Post => Recent_Max = (if Value > Recent_Max'Old then Value else Recent_Max'Old) and then
                      Max = (if Value > Max'Old then Value else Max'Old) and then
                      Updated = (Value > Max'Old)
      is
      begin
         Updated := False;
         if Value > Recent_Max then
            Recent_Max := Value;
         end if;
         if Value > Max then
            Max := Value;
            Updated := True;
         end if;
      end Update_Maximums;
   begin
      Update_Maximums (Self.Last_Wall_Time, Self.Recent_Max_Wall_Time, Self.Max_Wall_Time, Max_Wall_Time_Updated);
      Update_Maximums (Self.Last_Execution_Time, Self.Recent_Max_Execution_Time, Self.Max_Execution_Time, Max_Execution_Time_Updated);
   end Accumulate;

   -- Convert a set of wall and execution time measurements into a
   -- Task_Timing_Report.T:
   function To_Report (Max_Wall_Time : in Ada.Real_Time.Time_Span; Max_Execution_Time : in Ada.Real_Time.Time_Span; Recent_Wall_Time : in Ada.Real_Time.Time_Span; Recent_Execution_Time : in Ada.Real_Time.Time_Span) return Task_Timing_Report.T is
      use Delta_Time.Arithmetic;
      To_Return : Task_Timing_Report.T;
      -- The conversion saturates on overflow; the status is intentionally
      -- not consulted:
      Ignore : Sys_Time.Arithmetic.Sys_Time_Status;
   begin
      To_Delta_Time (Max_Wall_Time, To_Return.Max.Wall_Time, Ignore);
      To_Delta_Time (Max_Execution_Time, To_Return.Max.Execution_Time, Ignore);
      To_Delta_Time (Recent_Wall_Time, To_Return.Recent_Max.Wall_Time, Ignore);
      To_Delta_Time (Recent_Execution_Time, To_Return.Recent_Max.Execution_Time, Ignore);
      return To_Return;
   end To_Report;

   function Report (Self : in Instance) return Task_Timing_Report.T is
      (To_Report (Max_Wall_Time => Self.Max_Wall_Time, Max_Execution_Time => Self.Max_Execution_Time, Recent_Wall_Time => Self.Recent_Max_Wall_Time, Recent_Execution_Time => Self.Recent_Max_Execution_Time));

   function Report_Last (Self : in Instance) return Task_Timing_Report.T is
      (To_Report (Max_Wall_Time => Self.Max_Wall_Time, Max_Execution_Time => Self.Max_Execution_Time, Recent_Wall_Time => Self.Last_Wall_Time, Recent_Execution_Time => Self.Last_Execution_Time));

   procedure Reset_Recent_Max (Self : in out Instance) is
      use Ada.Real_Time;
   begin
      Self.Recent_Max_Wall_Time := Time_Span_Zero;
      Self.Recent_Max_Execution_Time := Time_Span_Zero;
   end Reset_Recent_Max;

   procedure Reset (Self : out Instance) is
   begin
      Self := (others => <>);
   end Reset;

end Stopwatch.Reporter;
