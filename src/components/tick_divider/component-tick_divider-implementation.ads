--------------------------------------------------------------------------------
-- Tick_Divider Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Interfaces;

-- The Tick Divider has an invokee connector meant to be called at a periodic
-- rate. This invokee connector will usually be connected to a component which
-- services a periodic hardware tick, or the Ticker component, which simulates a
-- hardware tick. The Tick Divider takes this periodic rate and divides it into
-- subrates which are divisions of the original rate. These divisors are provided
-- via an init routine. Ticks are forwarded out the Tick_T_Send arrayed output
-- connector according to the provided divisors. The priority of the output ticks
-- is determined by the order of the connections in the connector array. The first
-- connector in the array has the highest priority, and should be connected to the
-- output that needs to run first.
-- The component supports two tick counting modes: Internal mode (default) uses an
-- internal counter that increments with each received tick and rolls over at a
-- calculated maximum to ensure proper divisor alignment. Tick_Counter mode uses
-- the Count field from incoming ticks directly for division calculations,
-- allowing external control of the counting sequence and supporting non-
-- sequential tick patterns.
package Component.Tick_Divider.Implementation is

   type Instance is new Tick_Divider.Base_Instance with private;

   -- This initialization function is used to set the divider values for the
   -- component. An entry of 0 disables that connector from ever being invoked.
   --
   -- Init Parameters:
   -- Dividers : Divider_Array_Type_Access - An access to an array of dividers used
   -- to determine the tick rate of each Tick_T_Send connector.
   -- Tick_Source : Tick_Source_Type - Configures whether to use internal counter or
   -- incoming tick's Count field for division.
   --
   overriding procedure Init (Self : in out Instance; Dividers : in not null Divider_Array_Type_Access; Tick_Source : in Tick_Source_Type := Internal);

private

   -- The component class instance record:
   type Instance is new Tick_Divider.Base_Instance with record
      Dividers : Divider_Array_Type_Access;
      Count : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'First; -- This is rolled over manually in adb.
      Max_Count : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'First;
      Tick_Source : Tick_Source_Type := Internal;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a periodic Tick from an external component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Index : in Tick_T_Send_Index; Arg : in Tick.T);

end Component.Tick_Divider.Implementation;
