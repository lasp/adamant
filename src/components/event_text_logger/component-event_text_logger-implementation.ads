--------------------------------------------------------------------------------
-- Event_Text_Logger Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- Invokee Connector Includes:
with Event;

-- The Event Text Logger component receives events on an asynchronous queue and prints them either to the terminal or to a file as it receives them. The print statements for events are generated per the assembly that the Event Text Logger is attached to. By pointing the generator to a particular assembly model, you enable it to recognize and print certain events that are present in that assembly.
--
package Component.Event_Text_Logger.Implementation is

   -- The component class instance record:
   -- The discriminant for this component takes an access of type Event_To_Text_Function. The function provided should translate an event to a string for any given event in an assembly. A package with this function implementation is autocoded for each assembly, and can be passed into the Event Text Logger to configure it for a given assembly.
   --
   -- Discriminant Parameters:
   -- Event_To_Text : Event_To_Text_Function_Access - An access to an event to text function.
   --
   type Instance (Event_To_Text : Event_To_Text_Function_Access) is new Event_Text_Logger.Base_Instance with private;

private

   -- The component class instance record:
   -- The discriminant for this component takes an access of type Event_To_Text_Function. The function provided should translate an event to a string for any given event in an assembly. A package with this function implementation is autocoded for each assembly, and can be passed into the Event Text Logger to configure it for a given assembly.
   --
   -- Discriminant Parameters:
   -- Event_To_Text : Event_To_Text_Function_Access - An access to an event to text function.
   --
   type Instance (Event_To_Text : Event_To_Text_Function_Access) is new Event_Text_Logger.Base_Instance with record
      null; -- No internal state
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Events are received asynchronously on this connector:
   overriding procedure Event_T_Recv_Async (Self : in out Instance; Arg : in Event.T);
   -- This procedure is called when a Event_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Event_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Event.T);

end Component.Event_Text_Logger.Implementation;
