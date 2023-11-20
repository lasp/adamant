--------------------------------------------------------------------------------
-- Ticker Component Implementation Spec
--------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;

-- Standard Includes:
-- This is the ticker component.
package Component.Ticker.Implementation is

   -- The component class instance record:
   -- The ticker component is instantiated with the desired tick period in microseconds.
   --
   -- Discriminant Parameters:
   -- period_us : Positive - The tick period in microseconds
   --
   type Instance (Period_Us : Positive) is new Ticker.Base_Instance with private;

private

   -- The component class instance record:
   -- The ticker component is instantiated with the desired tick period in microseconds.
   --
   -- Discriminant Parameters:
   -- period_us : Positive - The tick period in microseconds
   --
   type Instance (Period_Us : Positive) is new Ticker.Base_Instance with record
      Period : Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period_Us);
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Microseconds (Period_Us);
      Count : Unsigned_32 := 0;
      First : Boolean := True;
   end record;

   -- Cycle function that gets called by task:
   overriding procedure Cycle (Self : in out Instance);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T) is null;
end Component.Ticker.Implementation;
