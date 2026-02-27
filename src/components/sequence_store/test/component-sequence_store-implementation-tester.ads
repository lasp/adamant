--------------------------------------------------------------------------------
-- Sequence_Store Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Sequence_Store_Reciprocal;
with Sys_Time;
with Printable_History;
with Command_Response.Representation;
with Sequence_Store_Memory_Region_Release.Representation;
with Packet.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Sequence_Store_Memory_Region_Store.Representation;
with Invalid_Sequence_Crc_Info.Representation;
with Invalid_Sequence_Length_Info.Representation;
with Slot_Written_Summary.Representation;
with Packed_Slot_Validity.Representation;
with Packed_Sequence_Id.Representation;
with Packed_Slot_Number.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;

-- The Sequence Store component is responsible for storing and managing access to a set of memory regions (slots) which each hold a single sequence. The managed memory regions are usually located in nonvolatile storage and can be read or written to via this component.
package Component.Sequence_Store.Implementation.Tester is

   use Component.Sequence_Store_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Sequence_Store_Memory_Region_Release_T_Recv_Sync_History_Package is new Printable_History (Sequence_Store_Memory_Region_Release.T, Sequence_Store_Memory_Region_Release.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Sequence_Slot_Active_History_Package is new Printable_History (Sequence_Store_Memory_Region_Store.T, Sequence_Store_Memory_Region_Store.Representation.Image);
   package Invalid_Sequence_Crc_History_Package is new Printable_History (Invalid_Sequence_Crc_Info.T, Invalid_Sequence_Crc_Info.Representation.Image);
   package Invalid_Sequence_Length_History_Package is new Printable_History (Invalid_Sequence_Length_Info.T, Invalid_Sequence_Length_Info.Representation.Image);
   package Writing_Sequence_To_Slot_History_Package is new Printable_History (Sequence_Store_Memory_Region_Store.T, Sequence_Store_Memory_Region_Store.Representation.Image);
   package Wrote_Sequence_To_Slot_History_Package is new Printable_History (Slot_Written_Summary.T, Slot_Written_Summary.Representation.Image);
   package Checked_Slot_Validity_History_Package is new Printable_History (Packed_Slot_Validity.T, Packed_Slot_Validity.Representation.Image);
   package Checking_All_Slot_Validity_History_Package is new Printable_History (Natural, Natural'Image);
   package Cannot_Activate_Duplicate_Sequence_Id_History_Package is new Printable_History (Packed_Sequence_Id.T, Packed_Sequence_Id.Representation.Image);
   package Invalid_Slot_Number_History_Package is new Printable_History (Packed_Slot_Number.T, Packed_Slot_Number.Representation.Image);
   package Activated_Slot_History_Package is new Printable_History (Packed_Slot_Number.T, Packed_Slot_Number.Representation.Image);
   package Deactivated_Slot_History_Package is new Printable_History (Packed_Slot_Number.T, Packed_Slot_Number.Representation.Image);
   package Dumping_Slot_Summary_History_Package is new Printable_History (Natural, Natural'Image);
   package Dumped_Slot_Summary_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Region_Store_Dropped_History_Package is new Printable_History (Sequence_Store_Memory_Region_Store.T, Sequence_Store_Memory_Region_Store.Representation.Image);

   -- Packet history packages:
   package Slot_Summaries_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Sequence_Store_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Sequence_Store.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Sequence_Store_Memory_Region_Release_T_Recv_Sync_History : Sequence_Store_Memory_Region_Release_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Sequence_Slot_Active_History : Sequence_Slot_Active_History_Package.Instance;
      Invalid_Sequence_Crc_History : Invalid_Sequence_Crc_History_Package.Instance;
      Invalid_Sequence_Length_History : Invalid_Sequence_Length_History_Package.Instance;
      Writing_Sequence_To_Slot_History : Writing_Sequence_To_Slot_History_Package.Instance;
      Wrote_Sequence_To_Slot_History : Wrote_Sequence_To_Slot_History_Package.Instance;
      Checked_Slot_Validity_History : Checked_Slot_Validity_History_Package.Instance;
      Checking_All_Slot_Validity_History : Checking_All_Slot_Validity_History_Package.Instance;
      Cannot_Activate_Duplicate_Sequence_Id_History : Cannot_Activate_Duplicate_Sequence_Id_History_Package.Instance;
      Invalid_Slot_Number_History : Invalid_Slot_Number_History_Package.Instance;
      Activated_Slot_History : Activated_Slot_History_Package.Instance;
      Deactivated_Slot_History : Deactivated_Slot_History_Package.Instance;
      Dumping_Slot_Summary_History : Dumping_Slot_Summary_History_Package.Instance;
      Dumped_Slot_Summary_History : Dumped_Slot_Summary_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Command_Dropped_History : Command_Dropped_History_Package.Instance;
      Region_Store_Dropped_History : Region_Store_Dropped_History_Package.Instance;
      -- Packet histories:
      Slot_Summaries_History : Slot_Summaries_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Sequence_Store_Memory_Region_Store_T_Send_Dropped : Boolean := False;
      Sequence_Store_Memory_Region_Store_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- After a memory region is stored, it is released via a call to this connector. A status is also returned, so the downstream component can determine if the sequence store operation was successful or not.
   overriding procedure Sequence_Store_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Release.T);
   -- The packet connector. This produces a summary of the sequence store slots and what is contained within each.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Sequence_Store_Memory_Region_Store_T_Send message is dropped due to a full queue.
   overriding procedure Sequence_Store_Memory_Region_Store_T_Send_Dropped (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Sequence Store component.
   -- The destination slot is currently active so cannot be written to.
   overriding procedure Sequence_Slot_Active (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T);
   -- The sequence to store has an invalid CRC and cannot be stored.
   overriding procedure Invalid_Sequence_Crc (Self : in out Instance; Arg : in Invalid_Sequence_Crc_Info.T);
   -- The sequence to store has a length that will not fit within the destination slot.
   overriding procedure Invalid_Sequence_Length (Self : in out Instance; Arg : in Invalid_Sequence_Length_Info.T);
   -- A command sequence memory region was received. It will be validated and written to the destination slot.
   overriding procedure Writing_Sequence_To_Slot (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T);
   -- A command sequence was successfully written to the destination slot.
   overriding procedure Wrote_Sequence_To_Slot (Self : in out Instance; Arg : in Slot_Written_Summary.T);
   -- The validity of a particular slot was checked and the results are included in this event.
   overriding procedure Checked_Slot_Validity (Self : in out Instance; Arg : in Packed_Slot_Validity.T);
   -- The validity for all slots within the sequence store.
   overriding procedure Checking_All_Slot_Validity (Self : in out Instance);
   -- A duplicate sequence ID cannot be activated. All active sequence IDs must be unique.
   overriding procedure Cannot_Activate_Duplicate_Sequence_Id (Self : in out Instance; Arg : in Packed_Sequence_Id.T);
   -- Slot number does not exist and is out of range for the available slots in the component.
   overriding procedure Invalid_Slot_Number (Self : in out Instance; Arg : in Packed_Slot_Number.T);
   -- Slot was activated.
   overriding procedure Activated_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T);
   -- Slot was deactivated.
   overriding procedure Deactivated_Slot (Self : in out Instance; Arg : in Packed_Slot_Number.T);
   -- Starting to produce a packet with a summary of the contents of the sequence store slots.
   overriding procedure Dumping_Slot_Summary (Self : in out Instance);
   -- Produced a packet with a summary of the contents of the sequence store slots.
   overriding procedure Dumped_Slot_Summary (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A sequence store memory region was dropped due to a full queue.
   overriding procedure Region_Store_Dropped (Self : in out Instance; Arg : in Sequence_Store_Memory_Region_Store.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Sequence Store component.
   -- This packet contains a summary of what is contained in each Sequence Store slot.
   overriding procedure Slot_Summaries (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Sequence_Store.Implementation.Tester;
