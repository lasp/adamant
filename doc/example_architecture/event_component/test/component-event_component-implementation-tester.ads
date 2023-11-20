--------------------------------------------------------------------------------
-- Event_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Event_Component_Reciprocal;
with Sys_Time;
with Printable_History;
with Sys_Time.Representation;
with Event.Representation;
with Event;
with Tick.Representation;
with Packed_U16.Representation;

-- This is the event component, which sends events.
package Component.Event_Component.Implementation.Tester is

   -- Invoker connector history packages:
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);

   -- Event history packages:
   package Tick_Received_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);
   package Ten_More_Ticks_Received_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package First_Tick_Received_History_Package is new Printable_History (Natural, Natural'Image);

   -- Component class instance:
   type Instance is new Component.Event_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Event_Component.Implementation.Instance;
      -- Connector histories:
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Tick_Received_History : Tick_Received_History_Package.Instance;
      Ten_More_Ticks_Received_History : Ten_More_Ticks_Received_History_Package.Instance;
      First_Tick_Received_History : First_Tick_Received_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to send out an event.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    A set of events for the Event Component.
   -- A tick was received by the component.
   overriding procedure Tick_Received (Self : in out Instance; Arg : in Tick.T);
   -- This event is produced for every 10 ticks and includes the total number of ticks received by the component.
   overriding procedure Ten_More_Ticks_Received (Self : in out Instance; Arg : in Packed_U16.T);
   -- This event is produced only when the first tick is received.
   overriding procedure First_Tick_Received (Self : in out Instance);

end Component.Event_Component.Implementation.Tester;
