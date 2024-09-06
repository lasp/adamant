--------------------------------------------------------------------------------
-- Connector_Queuer Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Connector_Queuer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Dropped_Message_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Dropped_Message_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Attach_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The generic invoker connector. Calls originating from this connector are serviced from the component's queue and thus will be executed in FIFO order in a thread-safe, atomic manner.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.T_Recv_Sync_History.Push (Arg);
   end T_Recv_Sync;

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

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T) is
      Ignore : T renames Arg;
   begin
      if not Self.Expect_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when T_Send was called!");
      else
         Self.T_Send_Dropped_Count := @ + 1;
         Self.Expect_T_Send_Dropped := False;
      end if;
   end T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The queue overflowed and the incoming data was dropped.
   overriding procedure Dropped_Message (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Message_History.Push (Arg);
   end Dropped_Message;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

end Component.Connector_Queuer.Implementation.Tester;
