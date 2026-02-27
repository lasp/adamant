--------------------------------------------------------------------------------
-- Rate_Group Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
with Ada.Real_Time; use Ada.Real_Time;

-- The Rate Group component is a queued component which invokes Tick connectors attached to it whenever it receives a Tick in. The tick in is intended to be periodic, allowing the component to control the execution of other components at a periodic rate. All components attached to the invoker connector of this component are said to be in a rate group, since they all execute at the same rate. Components are executed in the order they are attached to the components invoker connector. The execution of all attached connectors is expected to complete before another incoming Tick is put on the Rate Group component's queue. If the execution runs long, a cycle slip event is reported.
--
-- Note that this component is designed to be Active in an assembly. In this way the Rate Group will provide a task on which Passive components can execute.
--
package Component.Rate_Group.Implementation is

   type Instance is new Rate_Group.Base_Instance with private;

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
   overriding procedure Init (Self : in out Instance; Ticks_Per_Timing_Report : in Interfaces.Unsigned_16 := 1; Timing_Report_Delay_Ticks : in Interfaces.Unsigned_16 := 3; Issue_Time_Exceeded_Events : in Boolean := False);

private

   -- The component class instance record:
   type Instance is new Rate_Group.Base_Instance with record
      -- Time spans for keeping track of maximum
      -- execution data:
      Issue_Time_Exceeded_Events : Boolean := False;
      Max_Cycle_Time : Time_Span := Microseconds (0);
      Max_Execution_Time : Time_Span := Microseconds (0);
      Recent_Max_Cycle_Time : Time_Span := Microseconds (0);
      Recent_Max_Execution_Time : Time_Span := Microseconds (0);
      -- Timing report data:
      Ticks_Per_Timing_Report : Unsigned_16 := 1;
      Timing_Report_Delay_Ticks : Unsigned_16 := 1;
      Num_Ticks : Unsigned_16 := 0;
      Ticks_Since_Startup : Unsigned_16 := 0;
      -- Cycle slip count:
      Num_Cycle_Slips : Unsigned_16 := 0;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a periodic Tick from an external component, usually a Ticker or Tick_Divider component.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T);
   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Index : in Tick_T_Send_Index; Arg : in Tick.T);
   -- This procedure is called when a Pet_T_Send message is dropped due to a full queue.
   overriding procedure Pet_T_Send_Dropped (Self : in out Instance; Arg : in Pet.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

end Component.Rate_Group.Implementation;
