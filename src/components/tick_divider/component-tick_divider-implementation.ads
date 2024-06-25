--------------------------------------------------------------------------------
-- Tick_Divider Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- The Tick Divider component is a simple component which has an invokee port meant to be called at a periodic rate. This invokee port will usually be connected to a component which services a periodic hardware tick, or the Ticker component, which simulates a hardware tick. The Tick Divider takes this periodic rate and divides it into subrates which are divisions of the original rate. These divisors are provided via an init routine.
--
package Component.Tick_Divider.Implementation is

   type Instance is new Tick_Divider.Base_Instance with private;

   -- Initialization function to set the divider values for the component
   -- An entry of 0 disables that connector from ever being invoked.
   overriding procedure Init (Self : in out Instance; Dividers : in not null Divider_Array_Type_Access);

private

   -- The component class instance record:
   type Instance is new Tick_Divider.Base_Instance with record
      Dividers : Divider_Array_Type_Access;
      Count : Natural := Natural'First; -- This is rolled over manually in adb.
      Max_Count : Natural := Natural'First;
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
