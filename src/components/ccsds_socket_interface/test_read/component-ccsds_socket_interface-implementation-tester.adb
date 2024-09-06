--------------------------------------------------------------------------------
-- Ccsds_Socket_Interface Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Ccsds_Socket_Interface.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Init (Depth => 10);
      Self.Event_T_Recv_Sync_History.Init (Depth => 10);
      Self.Sys_Time_T_Return_History.Init (Depth => 10);
      -- Event histories:
      Self.Socket_Connected_History.Init (Depth => 10);
      Self.Socket_Not_Connected_History.Init (Depth => 10);
      Self.Packet_Send_Failed_History.Init (Depth => 10);
      Self.Packet_Recv_Failed_History.Init (Depth => 10);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Socket_Connected_History.Destroy;
      Self.Socket_Not_Connected_History.Destroy;
      Self.Packet_Send_Failed_History.Destroy;
      Self.Packet_Recv_Failed_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Ccsds_Space_Packet_T_Send (Self'Unchecked_Access, Self.Ccsds_Space_Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (Self'Unchecked_Access, Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Attach_Ccsds_Space_Packet_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Ccsds_Space_Packet_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- On this connector the Socket Interface Component sends any data it received from the socket.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Push (Arg);
   end Ccsds_Space_Packet_T_Recv_Sync;

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

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      Ignore : Ccsds_Space_Packet.T renames Arg;
   begin
      if not Self.Expect_Ccsds_Space_Packet_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Ccsds_Space_Packet_T_Send was called!");
      else
         Self.Ccsds_Space_Packet_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Ccsds_Space_Packet_T_Send_Dropped := False;
      end if;
   end Ccsds_Space_Packet_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The socket was successfully connected on the host and port provided.
   overriding procedure Socket_Connected (Self : in out Instance; Arg : Socket_Address.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Socket_Connected_History.Push (Arg);
   end Socket_Connected;

   -- The socket connection failed on the host and port provided
   overriding procedure Socket_Not_Connected (Self : in out Instance; Arg : Socket_Address.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Socket_Not_Connected_History.Push (Arg);
   end Socket_Not_Connected;

   -- Failed to send a packet over the socket because it has an invalid CCSDS header.
   overriding procedure Packet_Send_Failed (Self : in out Instance; Arg : Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Send_Failed_History.Push (Arg);
   end Packet_Send_Failed;

   -- Failed to receive a packet over the socket because it has an invalid CCSDS header.
   overriding procedure Packet_Recv_Failed (Self : in out Instance; Arg : Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Recv_Failed_History.Push (Arg);
   end Packet_Recv_Failed;

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

end Component.Ccsds_Socket_Interface.Implementation.Tester;
