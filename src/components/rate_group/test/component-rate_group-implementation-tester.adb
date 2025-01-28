--------------------------------------------------------------------------------
-- Rate_Group Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Rate_Group.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural; Tick_T_Send_Count : in Connector_Count_Type) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size, Tick_T_Send_Count => Tick_T_Send_Count);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Tick_T_Recv_Sync_History.Init (Depth => 100);
      Self.Pet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Cycle_Slip_History.Init (Depth => 100);
      Self.Max_Cycle_Time_Exceeded_History.Init (Depth => 100);
      Self.Max_Execution_Time_Exceeded_History.Init (Depth => 100);
      Self.Component_Has_Full_Queue_History.Init (Depth => 100);
      Self.Incoming_Tick_Dropped_History.Init (Depth => 100);
      -- Data product histories:
      Self.Timing_Report_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Tick_T_Recv_Sync_History.Destroy;
      Self.Pet_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Cycle_Slip_History.Destroy;
      Self.Max_Cycle_Time_Exceeded_History.Destroy;
      Self.Max_Execution_Time_Exceeded_History.Destroy;
      Self.Component_Has_Full_Queue_History.Destroy;
      Self.Incoming_Tick_Dropped_History.Destroy;
      -- Data product histories:
      Self.Timing_Report_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Tick_T_Send (From_Index => 1, To_Component => Self'Unchecked_Access, Hook => Self.Tick_T_Recv_Sync_Access);
      -- Disconnect one of these to test disconnected behavior:
      -- Self.Component_Instance.Attach_Tick_T_Send (From_Index => 2, To_Component => Self'Unchecked_Access, Hook => Self.Tick_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Tick_T_Send (From_Index => 3, To_Component => Self'Unchecked_Access, Hook => Self.Tick_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Pet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Pet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This unconstrained arrayed connector is connected to downstream components which require a Tick to be scheduled. Each index of the array will be called in sequence until all connected components have been called for that cycle.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Increment the system time to simulate an executing component:
      Self.System_Time := (@.Seconds + Self.Seconds_Delta, @.Subseconds + Sys_Time.Subseconds_Type (Self.Subseconds_Delta));
      -- Push the argument onto the test history for looking at later:
      Self.Tick_T_Recv_Sync_History.Push (Arg);
   end Tick_T_Recv_Sync;

   -- The pet send connector. This is used to service a software watchdog component.
   overriding procedure Pet_T_Recv_Sync (Self : in out Instance; Arg : in Pet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pet_T_Recv_Sync_History.Push (Arg);
   end Pet_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      if not Self.Expect_Tick_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Tick_T_Send was called!");
      else
         Self.Tick_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Tick_T_Send_Dropped := False;
      end if;
   end Tick_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Execution ran long on this cycle.
   overriding procedure Cycle_Slip (Self : in out Instance; Arg : in Cycle_Slip_Param.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Cycle_Slip_History.Push (Arg);
   end Cycle_Slip;

   -- A new maximum cycle time was reached. The event parameter is a Tick type with the maximum cycle time as the Time and the cycle count where the maximum cycle was achieved as the Count.
   overriding procedure Max_Cycle_Time_Exceeded (Self : in out Instance; Arg : in Time_Exceeded.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Max_Cycle_Time_Exceeded_History.Push (Arg);
   end Max_Cycle_Time_Exceeded;

   -- A new maximum execution time was reached. The event parameter is a Tick type with the maximum cycle time as the Time and the cycle count where the maximum cycle was achieved as the Count.
   overriding procedure Max_Execution_Time_Exceeded (Self : in out Instance; Arg : in Time_Exceeded.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Max_Execution_Time_Exceeded_History.Push (Arg);
   end Max_Execution_Time_Exceeded;

   -- The rate group tried to put a Tick on a component's queue, but the queue was full, so the Tick was dropped.
   overriding procedure Component_Has_Full_Queue (Self : in out Instance; Arg : in Full_Queue_Param.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Component_Has_Full_Queue_History.Push (Arg);
   end Component_Has_Full_Queue;

   -- The rate group component's queue is full, so it cannot store the tick coming in. This usually means the rate group is cycle slipping and not running as fast as it needs to.
   overriding procedure Incoming_Tick_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Incoming_Tick_Dropped_History.Push (Arg);
   end Incoming_Tick_Dropped;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Rate Group component.
   -- Data relating timing performance of the component.
   overriding procedure Timing_Report (Self : in out Instance; Arg : in Task_Timing_Report.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Timing_Report_History.Push (Arg);
   end Timing_Report;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

end Component.Rate_Group.Implementation.Tester;
