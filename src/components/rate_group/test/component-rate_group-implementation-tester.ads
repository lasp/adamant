--------------------------------------------------------------------------------
-- Rate_Group Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Rate_Group_Reciprocal;
with Sys_Time;
with Printable_History;
with Tick.Representation;
with Pet.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Data_Product;
with Task_Timing_Report.Representation;
with Event;
with Cycle_Slip_Param.Representation;
with Time_Exceeded.Representation;
with Full_Queue_Param.Representation;

-- The Rate Group component is a queued component which invokes Tick connectors attached to it whenever it receives a Tick in. The tick in is intended to be periodic, allowing the component to control the execution of other components at a periodic rate. All components attached to the invoker connector of this component are said to be in a rate group, since they all execute at the same rate. Components are executed in the order they are attached to the components invoker connector. The execution of all attached connectors is expected to complete before another incoming Tick is put on the Rate Group component's queue. If the execution runs long, a cycle slip event is reported.
--
-- Note that this component is designed to be Active in an assembly. In this way the Rate Group will provide a task on which Passive components can execute.
package Component.Rate_Group.Implementation.Tester is

   use Component.Rate_Group_Reciprocal;
   -- Invoker connector history packages:
   package Tick_T_Recv_Sync_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);
   package Pet_T_Recv_Sync_History_Package is new Printable_History (Pet.T, Pet.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Cycle_Slip_History_Package is new Printable_History (Cycle_Slip_Param.T, Cycle_Slip_Param.Representation.Image);
   package Max_Cycle_Time_Exceeded_History_Package is new Printable_History (Time_Exceeded.T, Time_Exceeded.Representation.Image);
   package Max_Execution_Time_Exceeded_History_Package is new Printable_History (Time_Exceeded.T, Time_Exceeded.Representation.Image);
   package Component_Has_Full_Queue_History_Package is new Printable_History (Full_Queue_Param.T, Full_Queue_Param.Representation.Image);
   package Incoming_Tick_Dropped_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);

   -- Data product history packages:
   package Timing_Report_History_Package is new Printable_History (Task_Timing_Report.T, Task_Timing_Report.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Rate_Group_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Rate_Group.Implementation.Instance;
      Seconds_Delta : Unsigned_32 := 1;
      Subseconds_Delta : Unsigned_32 := 0;
      -- Connector histories:
      Tick_T_Recv_Sync_History : Tick_T_Recv_Sync_History_Package.Instance;
      Pet_T_Recv_Sync_History : Pet_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Cycle_Slip_History : Cycle_Slip_History_Package.Instance;
      Max_Cycle_Time_Exceeded_History : Max_Cycle_Time_Exceeded_History_Package.Instance;
      Max_Execution_Time_Exceeded_History : Max_Execution_Time_Exceeded_History_Package.Instance;
      Component_Has_Full_Queue_History : Component_Has_Full_Queue_History_Package.Instance;
      Incoming_Tick_Dropped_History : Incoming_Tick_Dropped_History_Package.Instance;
      -- Data product histories:
      Timing_Report_History : Timing_Report_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Tick_T_Send_Dropped : Boolean := False;
      Tick_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural; Tick_T_Send_Count : in Connector_Count_Type);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This unconstrained arrayed connector is connected to downstream components which require a Tick to be scheduled. Each index of the array will be called in sequence until all connected components have been called for that cycle.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- The pet send connector. This is used to service a software watchdog component.
   overriding procedure Pet_T_Recv_Sync (Self : in out Instance; Arg : in Pet.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Execution ran long on this cycle.
   overriding procedure Cycle_Slip (Self : in out Instance; Arg : in Cycle_Slip_Param.T);
   -- A new maximum cycle time was reached. The event parameter is a Tick type with the maximum cycle time as the Time and the cycle count where the maximum cycle was achieved as the Count.
   overriding procedure Max_Cycle_Time_Exceeded (Self : in out Instance; Arg : in Time_Exceeded.T);
   -- A new maximum execution time was reached. The event parameter is a Tick type with the maximum cycle time as the Time and the cycle count where the maximum cycle was achieved as the Count.
   overriding procedure Max_Execution_Time_Exceeded (Self : in out Instance; Arg : in Time_Exceeded.T);
   -- The rate group tried to put a Tick on a component's queue, but the queue was full, so the Tick was dropped.
   overriding procedure Component_Has_Full_Queue (Self : in out Instance; Arg : in Full_Queue_Param.T);
   -- The rate group component's queue is full, so it cannot store the tick coming in. This usually means the rate group is cycle slipping and not running as fast as it needs to.
   overriding procedure Incoming_Tick_Dropped (Self : in out Instance; Arg : in Tick.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Rate Group component.
   -- Data relating timing performance of the component.
   overriding procedure Timing_Report (Self : in out Instance; Arg : in Task_Timing_Report.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Rate_Group.Implementation.Tester;
