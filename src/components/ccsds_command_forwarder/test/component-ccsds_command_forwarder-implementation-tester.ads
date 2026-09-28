--------------------------------------------------------------------------------
-- Ccsds_Command_Forwarder Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Command_Forwarder_Reciprocal;
with Printable_History;
with Command_Response.Representation;
with Ccsds_Space_Packet.Representation;
with Event.Representation;
with Data_Product.Representation;
with Sys_Time.Representation;
with Event;
with Ccsds_Primary_Header.Representation;
with Invalid_Command_Info.Representation;
with Data_Product;
with Packed_U32.Representation;

-- The CCSDS command forwarder is a component that forwards CCSDS packets received
-- as command arguments out of a CCSDS space packet send connector. The purpose of
-- this component is to allow CCSDS packets to be injected into a CCSDS packet
-- processing chain (e.g. into a CCSDS router) via command, which is useful for
-- testing and debugging the software. The packet is forwarded exactly as provided
-- in the command argument, with no validation or modification of its contents.
-- Note that the maximum size packet that can be forwarded by this component is
-- limited by the size of the command argument buffer, as configured by the
-- command_buffer_size configuration parameter.
package Component.Ccsds_Command_Forwarder.Implementation.Tester is

   use Component.Ccsds_Command_Forwarder_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Ccsds_Space_Packet_T_Recv_Sync_History_Package is new Printable_History (Ccsds_Space_Packet.T, Ccsds_Space_Packet.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Packet_Forwarded_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Packets_Forwarded_Count_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Ccsds_Command_Forwarder_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Command_Forwarder.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Ccsds_Space_Packet_T_Recv_Sync_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Packet_Forwarded_History : Packet_Forwarded_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Packets_Forwarded_Count_History : Packets_Forwarded_Count_History_Package.Instance;
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
   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The connector that forwards on CCSDS packets received by command.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- The event send connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The connector for data products.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    These are the events for the CCSDS command forwarder component.
   -- A CCSDS packet was received by command and forwarded out of the CCSDS space
   -- packet send connector. The event parameter contains the primary header of the
   -- forwarded packet.
   overriding procedure Packet_Forwarded (Self : in out Instance; Arg : in Ccsds_Primary_Header.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the data products for the CCSDS command forwarder component.
   -- The number of CCSDS packets forwarded by this component since startup.
   overriding procedure Packets_Forwarded_Count (Self : in out Instance; Arg : in Packed_U32.T);

end Component.Ccsds_Command_Forwarder.Implementation.Tester;
