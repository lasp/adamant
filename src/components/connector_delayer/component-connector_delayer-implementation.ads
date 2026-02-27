--------------------------------------------------------------------------------
-- Connector_Delayer Component Implementation Spec
--------------------------------------------------------------------------------

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
package Component.Connector_Delayer.Implementation is

   -- The component class instance record:
   type Instance is new Connector_Delayer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Configure how long the component will delay prior to sending out any queued
   -- data.
   --
   -- Init Parameters:
   -- Delay_Us : Natural - The amount of time to delay prior to transmission in
   -- microseconds. The delay time begins right after the element is dequeued from
   -- the Adamant queue.
   --
   overriding procedure Init (Self : in out Instance; Delay_Us : in Natural);

private

   -- The component class instance record:
   type Instance is new Connector_Delayer.Base_Instance with record
      Delay_Us : Natural := 0;
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
   -- The generic invokee connector.
   overriding procedure T_Recv_Async (Self : in out Instance; Arg : in T);
   -- This procedure is called when a T_Recv_Async message is dropped due to a full queue.
   overriding procedure T_Recv_Async_Dropped (Self : in out Instance; Arg : in T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

end Component.Connector_Delayer.Implementation;
