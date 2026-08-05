with Ada.Execution_Time;
with Ada.Real_Time;
with Ada.Task_Identification;

package Stopwatch with SPARK_Mode => On is

   use type Ada.Execution_Time.CPU_Time;
   use type Ada.Real_Time.Time;

   type Cpu_Timer_Instance is tagged record
      Start_Time, Stop_Time : Ada.Execution_Time.CPU_Time := Ada.Execution_Time.CPU_Time_First;
   end record;

   function Start return Cpu_Timer_Instance
      with Volatile_Function, Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Start'Result.Stop_Time = Ada.Execution_Time.CPU_Time_First;
   procedure Start (Self : in out Cpu_Timer_Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Self.Stop_Time = Self.Stop_Time'Old;
   procedure Stop (Self : in out Cpu_Timer_Instance)
      with Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State)),
           Post => Self.Start_Time = Self.Start_Time'Old;
   function Result (Self : in Cpu_Timer_Instance) return Ada.Real_Time.Time_Span
      with Global => null;

   type Wall_Timer_Instance is tagged record
      Start_Time, Stop_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   end record;

   function Start return Wall_Timer_Instance
      with Volatile_Function, Global => Ada.Real_Time.Clock_Time,
           Post => Start'Result.Stop_Time = Ada.Real_Time.Time_First;
   procedure Start (Self : in out Wall_Timer_Instance)
      with Global => Ada.Real_Time.Clock_Time,
           Post => Self.Stop_Time = Self.Stop_Time'Old;
   procedure Stop (Self : in out Wall_Timer_Instance)
      with Global => Ada.Real_Time.Clock_Time,
           Post => Self.Start_Time = Self.Start_Time'Old;
   function Result (Self : in Wall_Timer_Instance) return Ada.Real_Time.Time_Span
      with Global => null;

end Stopwatch;
