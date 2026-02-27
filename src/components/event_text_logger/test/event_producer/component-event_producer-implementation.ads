--------------------------------------------------------------------------------
-- Event_Producer Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Tick;

-- A simple component which produces events for testing the event to text logger component.
package Component.Event_Producer.Implementation is

   -- The component class instance record:
   type Instance is new Event_Producer.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Event_Producer.Base_Instance with record
      Count : Natural := 0;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick to regulate the execution of the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

end Component.Event_Producer.Implementation;
