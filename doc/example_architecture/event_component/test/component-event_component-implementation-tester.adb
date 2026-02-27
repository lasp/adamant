--------------------------------------------------------------------------------
-- Event_Component Component Tester Body
--------------------------------------------------------------------------------

package body Component.Event_Component.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Init (Depth => 20);
      Self.Event_T_Recv_Sync_History.Init (Depth => 20);
      -- Event histories:
      Self.Tick_Received_History.Init (Depth => 20);
      Self.Ten_More_Ticks_Received_History.Init (Depth => 20);
      Self.First_Tick_Received_History.Init (Depth => 20);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Tick_Received_History.Destroy;
      Self.Ten_More_Ticks_Received_History.Destroy;
      Self.First_Tick_Received_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Event_T_Send (Self'Unchecked_Access, Self.Event_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Tick_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- This connector is used to send out an event.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    A set of events for the Event Component.
   -- A tick was received by the component.
   overriding procedure Tick_Received (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Tick_Received_History.Push (Arg);
   end Tick_Received;

   -- This event is produced for every 10 ticks and includes the total number of ticks received by the component.
   overriding procedure Ten_More_Ticks_Received (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ten_More_Ticks_Received_History.Push (Arg);
   end Ten_More_Ticks_Received;

   -- This event is produced only when the first tick is received.
   overriding procedure First_Tick_Received (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.First_Tick_Received_History.Push (Arg);
   end First_Tick_Received;

end Component.Event_Component.Implementation.Tester;
