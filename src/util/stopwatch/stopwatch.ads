with Ada.Execution_Time;
with Ada.Real_Time;

package Stopwatch is

   type Cpu_Timer_Instance is tagged record
      Start_Time, Stop_Time : Ada.Execution_Time.CPU_Time := Ada.Execution_Time.CPU_Time_First;
   end record;

   function Start return Cpu_Timer_Instance;
   procedure Start (Self : in out Cpu_Timer_Instance);
   procedure Stop (Self : in out Cpu_Timer_Instance);
   function Result (Self : in Cpu_Timer_Instance) return Ada.Real_Time.Time_Span;

   type Wall_Timer_Instance is tagged record
      Start_Time, Stop_Time : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   end record;

   function Start return Wall_Timer_Instance;
   procedure Start (Self : in out Wall_Timer_Instance);
   procedure Stop (Self : in out Wall_Timer_Instance);
   function Result (Self : in Wall_Timer_Instance) return Ada.Real_Time.Time_Span;

end Stopwatch;
