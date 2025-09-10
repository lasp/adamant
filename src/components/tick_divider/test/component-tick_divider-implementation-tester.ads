--------------------------------------------------------------------------------
-- Tick_Divider Component Tester Spec
--------------------------------------------------------------------------------

-- Standard Includes:
with Component.Tick_Divider_Reciprocal;
with History;
with Sys_Time;

-- Invokee Connector Includes:
with Tick;
with Event;

-- Events Includes:
with Td_Full_Queue_Param;

-- The Tick Divider component is a simple component which has an invokee port meant to be called at a periodic rate. This invokee port will usually be connected to a component which services a periodic hardware tick, or the Ticker component, which simulates a hardware tick. The Tick Divider takes this periodic rate and divides it into subrates which are divisions of the original rate. These divisors are provided via an init routine.
--
package Component.Tick_Divider.Implementation.Tester is

   -- Invoker connector history packages:
   package Tick_T_Recv_Sync_History_Package is new History (Tick.T);
   package Event_T_Recv_Sync_History_Package is new History (Event.T);
   package Sys_Time_T_Return_History_Package is new History (Sys_Time.T);

   -- Event history packages:
   package Component_Has_Full_Queue_History_Package is new History (Td_Full_Queue_Param.T);

   -- Component class instance:
   type Instance is new Component.Tick_Divider_Reciprocal.Base_Instance with record
      Component_Instance : aliased Component.Tick_Divider.Implementation.Instance;
      -- Connector histories:
      Tick_T_Recv_Sync_History : Tick_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Component_Has_Full_Queue_History : Component_Has_Full_Queue_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Tick_T_Send_Count : in Connector_Index_Type);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This unconstrained arrayed connector is connected to downstream components which require a Tick to be scheduled. Each index of the array will be called at a rate equal to the rate at which the Tick_Recv_Sync connector is called, divided by the divisor provided during initialization.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The tick divider tried to put a Tick on a component's queue, but the queue was full, so the Tick was dropped.
   overriding procedure Component_Has_Full_Queue (Self : in out Instance; Arg : Td_Full_Queue_Param.T);

   ---------------------------------------
   -- Custom functions for whitebox testing:
   ---------------------------------------
   not overriding function Check_Counts (Self : in Instance; Count : Interfaces.Unsigned_32; Max_Count : Interfaces.Unsigned_32) return Boolean;

end Component.Tick_Divider.Implementation.Tester;
