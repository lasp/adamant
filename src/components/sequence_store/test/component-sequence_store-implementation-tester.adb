--------------------------------------------------------------------------------
-- Sequence_Store Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Sequence_Store.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Sequence_Slot_Active_History.Init (Depth => 100);
      Self.Invalid_Sequence_Crc_History.Init (Depth => 100);
      Self.Invalid_Sequence_Length_History.Init (Depth => 100);
      Self.Writing_Sequence_To_Slot_History.Init (Depth => 100);
      Self.Wrote_Sequence_To_Slot_History.Init (Depth => 100);
      Self.Checked_Slot_Validity_History.Init (Depth => 100);
      Self.Checking_All_Slot_Validity_History.Init (Depth => 100);
      Self.Cannot_Activate_Duplicate_Sequence_Id_History.Init (Depth => 100);
      Self.Invalid_Slot_Number_History.Init (Depth => 100);
      Self.Activated_Slot_History.Init (Depth => 100);
      Self.Deactivated_Slot_History.Init (Depth => 100);
      Self.Dumping_Slot_Summary_History.Init (Depth => 100);
      Self.Dumped_Slot_Summary_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Command_Dropped_History.Init (Depth => 100);
      Self.Region_Store_Dropped_History.Init (Depth => 100);
      -- Packet histories:
      Self.Slot_Summaries_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Sequence_Slot_Active_History.Destroy;
      Self.Invalid_Sequence_Crc_History.Destroy;
      Self.Invalid_Sequence_Length_History.Destroy;
      Self.Writing_Sequence_To_Slot_History.Destroy;
      Self.Wrote_Sequence_To_Slot_History.Destroy;
      Self.Checked_Slot_Validity_History.Destroy;
      Self.Checking_All_Slot_Validity_History.Destroy;
      Self.Cannot_Activate_Duplicate_Sequence_Id_History.Destroy;
      Self.Invalid_Slot_Number_History.Destroy;
      Self.Activated_Slot_History.Destroy;
      Self.Deactivated_Slot_History.Destroy;
      Self.Dumping_Slot_Summary_History.Destroy;
      Self.Dumped_Slot_Summary_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Command_Dropped_History.Destroy;
      Self.Region_Store_Dropped_History.Destroy;
      -- Packet histories:
      Self.Slot_Summaries_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sequence_Store_Memory_Region_Release_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Sequence_Store_Memory_Region_Release_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Sequence_Store_Memory_Region_Store_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Sequence_Store_Memory_Region_Store_T_Recv_Async_Access);
      Self.Attach_Sequence_Store_Memory_Region_Fetch_T_Request (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Sequence_Store_Memory_Region_Fetch_T_Service_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- After a memory region is stored, it is released via a call to this connector. A status is also returned, so the downstream component can determine if the sequence store operation was successful or not.
   overriding procedure Sequence_Store_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Push (Arg);
   end Sequence_Store_Memory_Region_Release_T_Recv_Sync;

   -- The packet connector. This produces a summary of the sequence store slots and what is contained within each.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

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
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      if not Self.Expect_Command_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_T_Send was called!");
      else
         Self.Command_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_T_Send_Dropped := False;
      end if;
   end Command_T_Send_Dropped;

   -- This procedure is called when a Sequence_Store_Memory_Region_Store_T_Send message is dropped due to a full queue.
   overriding procedure Sequence_Store_Memory_Region_Store_T_Send_Dropped (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T) is
      Ignore : Sequence_Store_Memory_Region_Store.T renames Arg;
   begin
      if not Self.Expect_Sequence_Store_Memory_Region_Store_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Sequence_Store_Memory_Region_Store_T_Send was called!");
      else
         Self.Sequence_Store_Memory_Region_Store_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Sequence_Store_Memory_Region_Store_T_Send_Dropped := False;
      end if;
   end Sequence_Store_Memory_Region_Store_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Sequence Store component.
   -- The destination slot is currently active so cannot be written to.
   overriding procedure Sequence_Slot_Active (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Slot_Active_History.Push (Arg);
   end Sequence_Slot_Active;

   -- The sequence to store has an invalid CRC and cannot be stored.
   overriding procedure Invalid_Sequence_Crc (Self : in out Instance; Arg : in Invalid_Sequence_Crc_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Sequence_Crc_History.Push (Arg);
   end Invalid_Sequence_Crc;

   -- The sequence to store has a length that will not fit within the destination slot.
   overriding procedure Invalid_Sequence_Length (Self : in out Instance; Arg : in Invalid_Sequence_Length_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Sequence_Length_History.Push (Arg);
   end Invalid_Sequence_Length;

   -- A command sequence memory region was received. It will be validated and written to the destination slot.
   overriding procedure Writing_Sequence_To_Slot (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Writing_Sequence_To_Slot_History.Push (Arg);
   end Writing_Sequence_To_Slot;

   -- A command sequence was successfully written to the destination slot.
   overriding procedure Wrote_Sequence_To_Slot (Self : in out Instance; Arg : in Slot_Written_Summary.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Wrote_Sequence_To_Slot_History.Push (Arg);
   end Wrote_Sequence_To_Slot;

   -- The validity of a particular slot was checked and the results are included in this event.
   overriding procedure Checked_Slot_Validity (Self : in out Instance; Arg : in Packed_Slot_Validity.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Checked_Slot_Validity_History.Push (Arg);
   end Checked_Slot_Validity;

   -- The validity for all slots within the sequence store.
   overriding procedure Checking_All_Slot_Validity (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Checking_All_Slot_Validity_History.Push (Arg);
   end Checking_All_Slot_Validity;

   -- A duplicate sequence ID cannot be activated. All active sequence IDs must be unique.
   overriding procedure Cannot_Activate_Duplicate_Sequence_Id (Self : in out Instance; Arg : in Packed_Sequence_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Cannot_Activate_Duplicate_Sequence_Id_History.Push (Arg);
   end Cannot_Activate_Duplicate_Sequence_Id;

   -- Slot number does not exist and is out of range for the available slots in the component.
   overriding procedure Invalid_Slot_Number (Self : in out Instance; Arg : in Packed_Slot_Number.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Slot_Number_History.Push (Arg);
   end Invalid_Slot_Number;

   -- Slot was activated.
   overriding procedure Activated_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Activated_Slot_History.Push (Arg);
   end Activated_Slot;

   -- Slot was deactivated.
   overriding procedure Deactivated_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Deactivated_Slot_History.Push (Arg);
   end Deactivated_Slot;

   -- Starting to produce a packet with a summary of the contents of the sequence store slots.
   overriding procedure Dumping_Slot_Summary (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumping_Slot_Summary_History.Push (Arg);
   end Dumping_Slot_Summary;

   -- Produced a packet with a summary of the contents of the sequence store slots.
   overriding procedure Dumped_Slot_Summary (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumped_Slot_Summary_History.Push (Arg);
   end Dumped_Slot_Summary;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Dropped_History.Push (Arg);
   end Command_Dropped;

   -- A sequence store memory region was dropped due to a full queue.
   overriding procedure Region_Store_Dropped (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Region_Store_Dropped_History.Push (Arg);
   end Region_Store_Dropped;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Sequence Store component.
   -- This packet contains a summary of what is contained in each Sequence Store slot.
   overriding procedure Slot_Summaries (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Slot_Summaries_History.Push (Arg);
   end Slot_Summaries;

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

end Component.Sequence_Store.Implementation.Tester;
