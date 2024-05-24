--------------------------------------------------------------------------------
-- Ccsds_Command_Depacketizer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Command_Depacketizer_Reciprocal;
with Sys_Time;
with Printable_History;
with Command.Representation;
with Data_Product.Representation;
with Event.Representation;
with Packet.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Data_Product;
with Packed_U16.Representation;
with Event;
with Invalid_Packet_Xor8_Info.Representation;
with Ccsds_Primary_Header.Representation;
with Invalid_Packet_Length.Representation;
with Invalid_Command_Info.Representation;
with Ccsds_Space_Packet.Representation;

-- This component receives CCSDS packets, validates the data within them, and converts them into Adamant commands. Note that the only internal state that this component contains is a packet accept and packet reject count. The component assumes that only a single task is attached to its CCSDS Space Packet invokee connector, and thus these counters are unprotected. If more than one task is attached to the input, a race condition arises around the counters, which may need to become protected.
package Component.Ccsds_Command_Depacketizer.Implementation.Tester is

   use Component.Ccsds_Command_Depacketizer_Reciprocal;
   -- Invoker connector history packages:
   package Command_T_Recv_Sync_History_Package is new Printable_History (Command.T, Command.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);

   -- Event history packages:
   package Invalid_Packet_Checksum_History_Package is new Printable_History (Invalid_Packet_Xor8_Info.T, Invalid_Packet_Xor8_Info.Representation.Image);
   package Invalid_Packet_Type_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Packet_Too_Small_History_Package is new Printable_History (Invalid_Packet_Length.T, Invalid_Packet_Length.Representation.Image);
   package Packet_Too_Large_History_Package is new Printable_History (Invalid_Packet_Length.T, Invalid_Packet_Length.Representation.Image);
   package No_Secondary_Header_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Counts_Reset_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Rejected_Packet_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Accepted_Packet_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Error_Packet_History_Package is new Printable_History (Ccsds_Space_Packet.T, Ccsds_Space_Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Ccsds_Command_Depacketizer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Command_Depacketizer.Implementation.Instance;
      -- Connector histories:
      Command_T_Recv_Sync_History : Command_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Invalid_Packet_Checksum_History : Invalid_Packet_Checksum_History_Package.Instance;
      Invalid_Packet_Type_History : Invalid_Packet_Type_History_Package.Instance;
      Packet_Too_Small_History : Packet_Too_Small_History_Package.Instance;
      Packet_Too_Large_History : Packet_Too_Large_History_Package.Instance;
      No_Secondary_Header_History : No_Secondary_Header_History_Package.Instance;
      Counts_Reset_History : Counts_Reset_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Rejected_Packet_Count_History : Rejected_Packet_Count_History_Package.Instance;
      Accepted_Packet_Count_History : Accepted_Packet_Count_History_Package.Instance;
      -- Packet histories:
      Error_Packet_History : Error_Packet_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The packet send connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Error packets are sent out of this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to register the components commands with the command router component.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A packet was received with an invalid checksum
   overriding procedure Invalid_Packet_Checksum (Self : in out Instance; Arg : in Invalid_Packet_Xor8_Info.T);
   -- A packet was received with an invalid ccsds packet type. The expected packet type is a telecommand, but a telemetry packet was received.
   overriding procedure Invalid_Packet_Type (Self : in out Instance; Arg : in Ccsds_Primary_Header.T);
   -- The packet received was too small to contain necessary command information.
   overriding procedure Packet_Too_Small (Self : in out Instance; Arg : in Invalid_Packet_Length.T);
   -- The packet received was too large and is bigger than the size of a command.
   overriding procedure Packet_Too_Large (Self : in out Instance; Arg : in Invalid_Packet_Length.T);
   -- A packet was received without a secondary header, but the secondary header is required.
   overriding procedure No_Secondary_Header (Self : in out Instance; Arg : in Ccsds_Primary_Header.T);
   -- A command was received to reset the counts.
   overriding procedure Counts_Reset (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the CCSDS Command Depacketizer component.
   -- The number of packets rejected by the component due to invalid data
   overriding procedure Rejected_Packet_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of packets accepted by the component
   overriding procedure Accepted_Packet_Count (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the CCSDS Command Depacketizer component.
   -- This packet contains a CCSDS packet that was dropped due to error.
   overriding procedure Error_Packet (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

end Component.Ccsds_Command_Depacketizer.Implementation.Tester;
