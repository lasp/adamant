package body Stopwatch with SPARK_Mode => On is

   use type Ada.Task_Identification.Task_Id;

   -- Read the CPU-execution clock for the calling task. SPARK only permits
   -- a call to a volatile function (one whose result depends on external
   -- state, like a clock) as the sole initializer of a standalone object,
   -- so the subprograms below read the clock as "Now : constant ... :="
   -- and copy from there; this also pins down exactly when the clock is
   -- sampled. Current_Task is read into a constant first for the same
   -- reason: letting it evaluate as Ada.Execution_Time.Clock's default
   -- parameter would place a volatile function call in an interfering
   -- context (SPARK RM 7.1.3(9)):
   function Cpu_Now return Ada.Execution_Time.CPU_Time
      with Inline => True, Volatile_Function, Global => (Input => (Ada.Real_Time.Clock_Time, Ada.Task_Identification.Tasking_State))
   is
      Id : constant Ada.Task_Identification.Task_Id := Ada.Task_Identification.Current_Task;
      pragma Assume (Id /= Ada.Task_Identification.Null_Task_Id,
         "Current_Task returns the identity of the calling task, which is never the null id (Ada RM C.7.1).");
      Now : constant Ada.Execution_Time.CPU_Time := Ada.Execution_Time.Clock (Id);
   begin
      return Now;
   end Cpu_Now;

   function Start return Cpu_Timer_Instance is
      To_Return : Cpu_Timer_Instance;
   begin
      To_Return.Start;
      return To_Return;
   end Start;

   procedure Start (Self : in out Cpu_Timer_Instance) is
      -- SPARK only permits a call to a volatile function like a clock read
      -- as the sole initializer of a standalone object, so the time is read
      -- into a constant and copied from there:
      Now : constant Ada.Execution_Time.CPU_Time := Cpu_Now;
   begin
      Self.Start_Time := Now;
   end Start;

   procedure Stop (Self : in out Cpu_Timer_Instance) is
      -- SPARK only permits a call to a volatile function like a clock read
      -- as the sole initializer of a standalone object, so the time is read
      -- into a constant and copied from there:
      Now : constant Ada.Execution_Time.CPU_Time := Cpu_Now;
   begin
      Self.Stop_Time := Now;
   end Stop;

   function Result (Self : in Cpu_Timer_Instance) return Ada.Real_Time.Time_Span is
      use Ada.Execution_Time;
   begin
      return Self.Stop_Time - Self.Start_Time;
   end Result;

   function Start return Wall_Timer_Instance is
      To_Return : Wall_Timer_Instance;
   begin
      To_Return.Start;
      return To_Return;
   end Start;

   procedure Start (Self : in out Wall_Timer_Instance) is
      -- SPARK only permits a call to a volatile function like a clock read
      -- as the sole initializer of a standalone object, so the time is read
      -- into a constant and copied from there:
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      Self.Start_Time := Now;
   end Start;

   procedure Stop (Self : in out Wall_Timer_Instance) is
      -- SPARK only permits a call to a volatile function like a clock read
      -- as the sole initializer of a standalone object, so the time is read
      -- into a constant and copied from there:
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      Self.Stop_Time := Now;
   end Stop;

   function Result (Self : in Wall_Timer_Instance) return Ada.Real_Time.Time_Span is
      use Ada.Real_Time;
   begin
      return Self.Stop_Time - Self.Start_Time;
      pragma Annotate (GNATprove, Intentional, "range check",
         "Time and Time_Span are both 64-bit under GNAT, so there is no wider type to subtract in, and the difference of two arbitrary "
         & "Times can mathematically exceed the Time_Span bounds; the prover reasons over the full type ranges, so the check is "
         & "unprovable without constraining the inputs, which callers (whose stamps come from the clock) could not discharge either. "
         & "Both stamps here are reads of the same monotonic clock taken within a single mission runtime; overflowing this "
         & "subtraction would take two stamps ~292 years apart at nanosecond resolution.");
   end Result;

end Stopwatch;
