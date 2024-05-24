--------------------------------------------------------------------------------
-- Ccsds_Command_Depacketizer Component Tester Body
--------------------------------------------------------------------------------

package body Component.Ccsds_Command_Depacketizer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Invalid_Packet_Checksum_History.Init (Depth => 100);
      Self.Invalid_Packet_Type_History.Init (Depth => 100);
      Self.Packet_Too_Small_History.Init (Depth => 100);
      Self.Packet_Too_Large_History.Init (Depth => 100);
      Self.No_Secondary_Header_History.Init (Depth => 100);
      Self.Counts_Reset_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Rejected_Packet_Count_History.Init (Depth => 100);
      Self.Accepted_Packet_Count_History.Init (Depth => 100);
      -- Packet histories:
      Self.Error_Packet_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Invalid_Packet_Checksum_History.Destroy;
      Self.Invalid_Packet_Type_History.Destroy;
      Self.Packet_Too_Small_History.Destroy;
      Self.Packet_Too_Large_History.Destroy;
      Self.No_Secondary_Header_History.Destroy;
      Self.Counts_Reset_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Rejected_Packet_Count_History.Destroy;
      Self.Accepted_Packet_Count_History.Destroy;
      -- Packet histories:
      Self.Error_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Attach_Ccsds_Space_Packet_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Ccsds_Space_Packet_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The packet send connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_T_Recv_Sync_History.Push (Arg);
   end Command_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

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
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- This connector is used to register the components commands with the command router component.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A packet was received with an invalid checksum
   overriding procedure Invalid_Packet_Checksum (Self : in out Instance; Arg : in Invalid_Packet_Xor8_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Packet_Checksum_History.Push (Arg);
   end Invalid_Packet_Checksum;

   -- A packet was received with an invalid ccsds packet type. The expected packet type is a telecommand, but a telemetry packet was received.
   overriding procedure Invalid_Packet_Type (Self : in out Instance; Arg : in Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Packet_Type_History.Push (Arg);
   end Invalid_Packet_Type;

   -- The packet received was too small to contain necessary command information.
   overriding procedure Packet_Too_Small (Self : in out Instance; Arg : in Invalid_Packet_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Too_Small_History.Push (Arg);
   end Packet_Too_Small;

   -- The packet received was too large and is bigger than the size of a command.
   overriding procedure Packet_Too_Large (Self : in out Instance; Arg : in Invalid_Packet_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Too_Large_History.Push (Arg);
   end Packet_Too_Large;

   -- A packet was received without a secondary header, but the secondary header is required.
   overriding procedure No_Secondary_Header (Self : in out Instance; Arg : in Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.No_Secondary_Header_History.Push (Arg);
   end No_Secondary_Header;

   -- A command was received to reset the counts.
   overriding procedure Counts_Reset (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Counts_Reset_History.Push (Arg);
   end Counts_Reset;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the CCSDS Command Depacketizer component.
   -- The number of packets rejected by the component due to invalid data
   overriding procedure Rejected_Packet_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Rejected_Packet_Count_History.Push (Arg);
   end Rejected_Packet_Count;

   -- The number of packets accepted by the component
   overriding procedure Accepted_Packet_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Accepted_Packet_Count_History.Push (Arg);
   end Accepted_Packet_Count;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the CCSDS Command Depacketizer component.
   -- This packet contains a CCSDS packet that was dropped due to error.
   overriding procedure Error_Packet (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Error_Packet_History.Push (Arg);
   end Error_Packet;

end Component.Ccsds_Command_Depacketizer.Implementation.Tester;
