--------------------------------------------------------------------------------
-- Connector_Delayer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Connector_Delayer_Reciprocal;
with Printable_History;
with History;
with Sys_Time.Representation;
with Event.Representation;
with Event;

-- This component is similar to the Connector Queuer component except that it
-- delays (via sleep) the transmission of the sent data for a configurable (at
-- initialization) amount of time prior. When configured to sleep for zero
-- microseconds, this component behaves identically to the Connector Queuer. This
-- component can be used for a variety of purposes, such as 1) serving as an
-- alarm, transmitting data N us after receipt or 2) spacing out the transmission
-- of data, sending out new data every N us.
-- Like the Connector Queuer, this component serves as a multi-tasking safe
-- synchronization point for multiple callers. When the T_Recv_Async connector is
-- called, the data is queued. Based on the priority of this component, the data
-- will be safely dequeued in the future, the component will then sleep for the
-- configurable amount of microseconds, and then, finally, the T_Send connector
-- will be called. The queue is implemented using the standard Adamant queue, and
-- thus calls are serviced in FIFO order. The queue protection mechanism
-- effectively makes all downstream connector calls of this component thread-safe.
generic
package Component.Connector_Delayer.Implementation.Tester is

   package Connector_Delayer_Package is new Component.Connector_Delayer_Reciprocal (T, Serialized_Length);
   -- Invoker connector history packages:
   package T_Recv_Sync_History_Package is new History (T);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Dropped_Message_History_Package is new Printable_History (Natural, Natural'Image);

   -- Component class instance:
   type Instance is new Connector_Delayer_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Connector_Delayer.Implementation.Instance;
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
   -- The generic invoker connector. Calls originating from this connector are
   -- serviced from the component's queue and thus will be executed in FIFO order in
   -- a thread-safe, atomic manner.
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

end Component.Connector_Delayer.Implementation.Tester;
