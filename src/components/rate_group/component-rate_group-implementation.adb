--------------------------------------------------------------------------------
-- Rate_Group Component Implementation Body
--------------------------------------------------------------------------------

with Sys_Time.Arithmetic;
with Delta_Time.Arithmetic;
with Stopwatch;
with Task_Timing_Report;

package body Component.Rate_Group.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization function is used to set the number of ticks that the component should wait before producing the timing report data product.
   --
   -- Init Parameters:
   -- Ticks_Per_Timing_Report : Interfaces.Unsigned_16 - The period (in ticks) that the component should wait before sending a timing report data product. A value of zero prevents the component from sending the data product.
   -- Timing_Report_Delay_Ticks : Interfaces.Unsigned_16 - The number of ticks the component waits before calculating and sending a timing report data product. It is common for the first few executions of a rate group to have execution times longer than normal due to startup logic. In this case, it is often desirable to ignore these cycles in the timing report, especially for the maximum values.
   -- Issue_Time_Exceeded_Events : Boolean - If set to True, an event will be issued any time the maximum execution or wall clock time of the component is exceeded. If set to False, these events will never be issued. The same information is accessible via the component's data products, so enabling the event may become a redundant annoyance.
   --
   overriding procedure Init (Self : in out Instance; Ticks_Per_Timing_Report : in Interfaces.Unsigned_16 := 1; Timing_Report_Delay_Ticks : in Interfaces.Unsigned_16 := 3; Issue_Time_Exceeded_Events : in Boolean := False) is
   begin
      Self.Ticks_Per_Timing_Report := Ticks_Per_Timing_Report;
      Self.Issue_Time_Exceeded_Events := Issue_Time_Exceeded_Events;
      Self.Timing_Report_Delay_Ticks := Timing_Report_Delay_Ticks;
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a periodic Tick from an external component, usually a Ticker or Tick_Divider component.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
      use Sys_Time.Arithmetic;
      use Delta_Time.Arithmetic;

      -- Local vars:
      Cycle_Time : Time_Span;
      Execution_Time : Time_Span;
      Stop_Wall_Time : Sys_Time.T;
      Event_Time : Delta_Time.T;
      Ignore : Sys_Time_Status;
      Sw : Stopwatch.Wall_Timer_Instance;
      Sw_Cpu : Stopwatch.Cpu_Timer_Instance;
   begin
      -- Start execution timer:
      Sw_Cpu.Start;

      -- Invoke all members of this rate group:
      for Index in Self.Connector_Tick_T_Send'Range loop
         -- If the connector is connected, invoke it:
         if Self.Is_Tick_T_Send_Connected (Index) then
            -- Send the tick and check the result:
            Self.Tick_T_Send (Index, Arg);
         end if;
      end loop;

      -- Pet the watchdog to indicate this subprogram is executing.
      Self.Pet_T_Send_If_Connected ((Count => Arg.Count));

      -- Stop execution timer:
      Sw_Cpu.Stop;
      Stop_Wall_Time := Self.Sys_Time_T_Get;

      if Self.Ticks_Since_Startup >= Self.Timing_Report_Delay_Ticks then
         -- Compute time differences for execution timer
         -- and wall clock timer.
         --
         -- The wall clock timer measures
         -- the time starting at the time stamp of the incoming
         -- Tick.T, ending at the end of the execution of this
         -- rate group.
         --
         -- The execution timer only measures the time this task
         -- was actually executing on the CPU during this cycle.
         --
         Sw.Start_Time := To_Time (Arg.Time);
         Sw.Stop_Time := To_Time (Stop_Wall_Time);
         Cycle_Time := Sw.Result;
         Execution_Time := Sw_Cpu.Result;

         -- Store max times and report any update:
         if Cycle_Time > Self.Recent_Max_Cycle_Time then
            Self.Recent_Max_Cycle_Time := Cycle_Time;
         end if;
         if Execution_Time > Self.Recent_Max_Execution_Time then
            Self.Recent_Max_Execution_Time := Execution_Time;
         end if;
         if Cycle_Time > Self.Max_Cycle_Time then
            Self.Max_Cycle_Time := Cycle_Time;
            if Self.Issue_Time_Exceeded_Events then
               Ignore := To_Delta_Time (Self.Max_Cycle_Time, Event_Time);
               Self.Event_T_Send_If_Connected (Self.Events.Max_Cycle_Time_Exceeded (Stop_Wall_Time, (Time_Delta => Event_Time, Count => Arg.Count)));
            end if;
         end if;
         if Execution_Time > Self.Max_Execution_Time then
            Self.Max_Execution_Time := Execution_Time;
            if Self.Issue_Time_Exceeded_Events then
               Ignore := To_Delta_Time (Self.Max_Execution_Time, Event_Time);
               Self.Event_T_Send_If_Connected (Self.Events.Max_Execution_Time_Exceeded (Stop_Wall_Time, (Time_Delta => Event_Time, Count => Arg.Count)));
            end if;
         end if;

         -- If the Ticks_Per_Timing_Report is greater than zero, then we need to send out a
         -- data product periodically.
         if Self.Ticks_Per_Timing_Report > 0 then
            -- Increment the number of ticks.
            Self.Num_Ticks := @ + 1;

            -- If we are at the period then send out the data product:
            if Self.Num_Ticks >= Self.Ticks_Per_Timing_Report then
               declare
                  Timing_Report : Task_Timing_Report.T;
               begin
                  -- Convert Time_Spans to the Delta_Time.T's stored in the data product type:
                  Ignore := To_Delta_Time (Self.Max_Cycle_Time, Timing_Report.Max.Wall_Time);
                  Ignore := To_Delta_Time (Self.Max_Execution_Time, Timing_Report.Max.Execution_Time);
                  Ignore := To_Delta_Time (Self.Recent_Max_Cycle_Time, Timing_Report.Recent_Max.Wall_Time);
                  Ignore := To_Delta_Time (Self.Recent_Max_Execution_Time, Timing_Report.Recent_Max.Execution_Time);

                  -- Send the data product:
                  Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Timing_Report (Stop_Wall_Time, Timing_Report));

                  -- Reset the recent cycle and execution times:
                  Self.Recent_Max_Cycle_Time := Microseconds (0);
                  Self.Recent_Max_Execution_Time := Microseconds (0);

                  -- Reset the number of ticks:
                  Self.Num_Ticks := 0;
               end;
            end if;
         end if;
      else
         Self.Ticks_Since_Startup := @ + 1;
      end if;

      -- Check for cycle slip.
      -- If something has been put on our input queue, then we know that we have slipped
      -- a cycle. The only thing that can be put on our queue is a Tick. If a tick has
      -- been put on our queue before we have finished executing the last Tick then
      -- our rate group has slipped. Sad day :(
      if Self.Queue.Num_Elements > 0 then
         Self.Num_Cycle_Slips := @ + 1;
         Self.Event_T_Send_If_Connected (Self.Events.Cycle_Slip (Stop_Wall_Time, (Slipped_Tick => Arg, Num_Slips => Self.Num_Cycle_Slips)));
      end if;

   end Tick_T_Recv_Async;

   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Incoming_Tick_Dropped (Self.Sys_Time_T_Get, Arg));
   end Tick_T_Recv_Async_Dropped;

   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue in an external component:
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Index : in Tick_T_Send_Index; Arg : in Tick.T) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Component_Has_Full_Queue (Self.Sys_Time_T_Get, (Dropped_Tick => Arg, Index => Index)));
   end Tick_T_Send_Dropped;

end Component.Rate_Group.Implementation;
