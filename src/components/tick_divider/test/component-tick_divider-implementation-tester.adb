--------------------------------------------------------------------------------
-- Tick_Divider Component Tester Body
--------------------------------------------------------------------------------

package body Component.Tick_Divider.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Tick_T_Send_Count : in Connector_Index_Type) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Tick_T_Send_Count);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Tick_T_Recv_Sync_History.Init (Depth => 30);
      Self.Event_T_Recv_Sync_History.Init (Depth => 10);
      Self.Sys_Time_T_Return_History.Init (Depth => 10);
      -- Event histories:
      Self.Component_Has_Full_Queue_History.Init (Depth => 10);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Tick_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Component_Has_Full_Queue_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Tick_T_Send (1, Self'Unchecked_Access, Self.Tick_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Tick_T_Send (2, Self'Unchecked_Access, Self.Tick_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Tick_T_Send (3, Self'Unchecked_Access, Self.Tick_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (Self'Unchecked_Access, Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Tick_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This unconstrained arrayed connector is connected to downstream components which require a Tick to be scheduled. Each index of the array will be called at a rate equal to the rate at which the Tick_Recv_Sync connector is called, divided by the divisor provided during initialization.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Tick_T_Recv_Sync_History.Push (Arg);
   end Tick_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The tick divider tried to put a Tick on a component's queue, but the queue was full, so the Tick was dropped.
   overriding procedure Component_Has_Full_Queue (Self : in out Instance; Arg : Td_Full_Queue_Param.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Component_Has_Full_Queue_History.Push (Arg);
   end Component_Has_Full_Queue;

   not overriding function Check_Counts (Self : in Instance; Count : Interfaces.Unsigned_32; Max_Count : Interfaces.Unsigned_32) return Boolean is
   begin
      return Count = Self.Component_Instance.Count and then Max_Count = Self.Component_Instance.Max_Count;
   end Check_Counts;

end Component.Tick_Divider.Implementation.Tester;
