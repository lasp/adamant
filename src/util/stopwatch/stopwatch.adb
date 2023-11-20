package body Stopwatch is

   function Start return Cpu_Timer_Instance is
      To_Return : Cpu_Timer_Instance;
   begin
      To_Return.Start_Time := Ada.Execution_Time.Clock;
      return To_Return;
   end Start;

   procedure Start (Self : in out Cpu_Timer_Instance) is
   begin
      Self.Start_Time := Ada.Execution_Time.Clock;
   end Start;

   procedure Stop (Self : in out Cpu_Timer_Instance) is
   begin
      Self.Stop_Time := Ada.Execution_Time.Clock;
   end Stop;

   function Result (Self : in Cpu_Timer_Instance) return Ada.Real_Time.Time_Span is
      use Ada.Execution_Time;
   begin
      return Self.Stop_Time - Self.Start_Time;
   end Result;

   function Start return Wall_Timer_Instance is
      To_Return : Wall_Timer_Instance;
   begin
      To_Return.Start_Time := Ada.Real_Time.Clock;
      return To_Return;
   end Start;

   procedure Start (Self : in out Wall_Timer_Instance) is
   begin
      Self.Start_Time := Ada.Real_Time.Clock;
   end Start;

   procedure Stop (Self : in out Wall_Timer_Instance) is
   begin
      Self.Stop_Time := Ada.Real_Time.Clock;
   end Stop;

   function Result (Self : in Wall_Timer_Instance) return Ada.Real_Time.Time_Span is
      use Ada.Real_Time;
   begin
      return Self.Stop_Time - Self.Start_Time;
   end Result;

end Stopwatch;
