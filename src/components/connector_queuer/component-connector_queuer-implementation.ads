--------------------------------------------------------------------------------
-- Connector_Queuer Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Interfaces;
with Protected_Variables;

-- This is a generic component that can be used to queue (as in recv_async) the call to an input connector. The component allows a queue to be added in front of the a synchronous connector in any component. Adding a queue to a component not designed with thread-safety in mind can serve as a multi-tasking safe synchronization point for multiple callers. When the T_Recv_Async connector is called, the data is queued. Based on the priority of this component, the data will be safely dequeued in the future and the T_Send connector will be called. The queue is implemented using the standard Adamant queue, and thus calls are serviced in FIFO order. The queue protection mechanism effectively makes all downstream connector calls of this component thread-safe. The advantage of this component is that deploying it appropriately in an assembly can provide thread-safety and priority-tuned FIFO execution to components which are not designed to be thread-safe in and of themselves.
generic
package Component.Connector_Queuer.Implementation is

   -- The component class instance record:
   type Instance is new Connector_Queuer.Base_Instance with private;

private

   -- Instantiate protected 16 bit counter. The dropped message count is updated
   -- from the calling task's context when the queue is full, so a protected
   -- counter is used to remain safe across concurrent callers.
   package Sixteen_Counter is new Protected_Variables.Generic_Protected_Counter (Interfaces.Unsigned_16);

   -- The component class instance record:
   type Instance is new Connector_Queuer.Base_Instance with record
      Dropped_Message_Count : Sixteen_Counter.Counter;
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
   overriding procedure Set_Up (Self : in out Instance);

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
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

end Component.Connector_Queuer.Implementation;
