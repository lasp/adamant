--------------------------------------------------------------------------------
-- Tick_Listener Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- Invokee Connector Includes:
with Packed_Natural;
with Tick;

-- This is the Tick Listener component. It is attached to a tick and provides a connector which will give the caller a count. The count includes the number of times the tick has been received since the last invocation of the connector. This component should be made passive in order to function properly. This component is a useful substitute for the Interrupt Listener component when you want to simulate interrupts with software ticks.
package Component.Tick_Listener.Implementation is

   -- The component class instance record:
   type Instance is new Tick_Listener.Base_Instance with private;

private

   -- This tick handler simply updates an internal count that a task can query and
   -- reset. This is useful for letting a task know that tick has occurred without
   -- blocking the task (as the above handler does).
   protected type Tick_Counter is
      -- Function which returns the current count without modifying it:
      function Get_Count return Natural;
      procedure Get_Count_And_Reset (The_Count : out Natural);
      procedure Reset_Count;
      procedure Increment_Count;
   private
      -- Internal count variable.
      Count : Natural := Natural'First; -- This variable will only count to Natural'Last, as implemented in the adb
   end Tick_Counter;

   -- The component class instance record:
   type Instance is new Tick_Listener.Base_Instance with record
      The_Counter : Tick_Counter;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The interrupt counter get connection.
   overriding function Get_Tick_Count (Self : in out Instance) return Packed_Natural.T;
   -- The tick receive connection.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

end Component.Tick_Listener.Implementation;
