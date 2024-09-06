--------------------------------------------------------------------------------
-- Ccsds_Subpacket_Extractor Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Ccsds_Subpacket_Extractor.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Init (Depth => 15);
      Self.Event_T_Recv_Sync_History.Init (Depth => 15);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 15);
      Self.Sys_Time_T_Return_History.Init (Depth => 15);
      -- Event histories:
      Self.Invalid_Received_Packet_Length_History.Init (Depth => 15);
      Self.Invalid_Extracted_Packet_Length_History.Init (Depth => 15);
      Self.Dropped_Trailing_Bytes_History.Init (Depth => 15);
      Self.Dropped_Packet_History.Init (Depth => 15);
      -- Packet histories:
      Self.Error_Packet_History.Init (Depth => 15);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Invalid_Received_Packet_Length_History.Destroy;
      Self.Invalid_Extracted_Packet_Length_History.Destroy;
      Self.Dropped_Trailing_Bytes_History.Destroy;
      Self.Dropped_Packet_History.Destroy;
      -- Packet histories:
      Self.Error_Packet_History.Destroy;

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
      Self.Component_Instance.Attach_Packet_T_Send (Self'Unchecked_Access, Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Attach_Ccsds_Space_Packet_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Ccsds_Space_Packet_T_Recv_Sync_Access);
      Self.Attach_Ccsds_Space_Packet_T_Send_2 (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Ccsds_Space_Packet_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The ccsds packet send connector.
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

   -- Error packets are sent out of this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

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
   -- This procedure is called when a Ccsds_Space_Packet_T_Send_2 message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_2_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      Ignore : Ccsds_Space_Packet.T renames Arg;
   begin
      if not Self.Expect_Ccsds_Space_Packet_T_Send_2_Dropped then
         pragma Assert (False, "The component's queue filled up when Ccsds_Space_Packet_T_Send_2 was called!");
      else
         Self.Ccsds_Space_Packet_T_Send_2_Dropped_Count := @ + 1;
         Self.Expect_Ccsds_Space_Packet_T_Send_2_Dropped := False;
      end if;
   end Ccsds_Space_Packet_T_Send_2_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A packet was received with a length that is too large or too small.
   overriding procedure Invalid_Received_Packet_Length (Self : in out Instance; Arg : Invalid_Packet_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Received_Packet_Length_History.Push (Arg);
   end Invalid_Received_Packet_Length;

   -- A packet was extracted with a length that is too large.
   overriding procedure Invalid_Extracted_Packet_Length (Self : in out Instance; Arg : Invalid_Packet_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Extracted_Packet_Length_History.Push (Arg);
   end Invalid_Extracted_Packet_Length;

   -- Some remaining bytes were found at the end of a packet that are too small to be a CCSDS packet.
   overriding procedure Dropped_Trailing_Bytes (Self : in out Instance; Arg : Packed_Byte.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Trailing_Bytes_History.Push (Arg);
   end Dropped_Trailing_Bytes;

   -- The component's queue overflowed and a packet with the following header was dropped.
   overriding procedure Dropped_Packet (Self : in out Instance; Arg : Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Packet_History.Push (Arg);
   end Dropped_Packet;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the CCSDS Subpacket Extractor component.
   -- This packet contains a CCSDS packet that was dropped due to error.
   overriding procedure Error_Packet (Self : in out Instance; Arg : Ccsds_Space_Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Error_Packet_History.Push (Arg);
   end Error_Packet;

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

end Component.Ccsds_Subpacket_Extractor.Implementation.Tester;
