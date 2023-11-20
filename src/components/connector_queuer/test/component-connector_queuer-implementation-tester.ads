--------------------------------------------------------------------------------
-- Connector_Queuer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Connector_Queuer_Reciprocal;
with Sys_Time;
with Printable_History;
with History;
with Sys_Time.Representation;
with Event.Representation;
with Event;

-- This is a generic component that can be used to queue (as in recv_async) the call to an input connector. The component allows a queue to be added in front of the a synchronous connector in any component. Adding a queue to a component not designed with thread-safety in mind can serve as a multi-tasking safe synchronization point for multiple callers. When the T_Recv_Async connector is called, the data is queued. Based on the priority of this component, the data will be safely dequeued in the future and the T_Send connector will be called. The queue is implemented using the standard Adamant queue, and thus calls are serviced in FIFO order. The queue protection mechanism effectively makes all downstream connector calls of this component thread-safe. The advantage of this component is that deploying it appropriately in an assembly can provide thread-safety and priority-tuned FIFO execution to components which are not designed to be thread-safe in and of themselves.
generic
package Component.Connector_Queuer.Implementation.Tester is

   package Connector_Queuer_Package is new Component.Connector_Queuer_Reciprocal (T, Serialized_Length);
   -- Invoker connector history packages:
   package T_Recv_Sync_History_Package is new History (T);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Dropped_Message_History_Package is new Printable_History (Natural, Natural'Image);

   -- Component class instance:
   type Instance is new Connector_Queuer_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Connector_Queuer.Implementation.Instance;
      -- Connector histories:
      T_Recv_Sync_History : T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Dropped_Message_History : Dropped_Message_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_T_Send_Dropped : Boolean := False;
      T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invoker connector. Calls originating from this connector are serviced from the component's queue and thus will be executed in FIFO order in a thread-safe, atomic manner.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The queue overflowed and the incoming data was dropped.
   overriding procedure Dropped_Message (Self : in out Instance);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Connector_Queuer.Implementation.Tester;
