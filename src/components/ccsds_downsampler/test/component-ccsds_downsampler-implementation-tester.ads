--------------------------------------------------------------------------------
-- Ccsds_Downsampler Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Downsampler_Reciprocal;
with Sys_Time;
with Printable_History;
with Ccsds_Space_Packet.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Invalid_Command_Info.Representation;
with Filter_Factor_Cmd_Type.Representation;
with Data_Product;
with Packed_U16.Representation;

-- The CCSDS downsampler is a component that is intended to filter down packets that are listed in the downsample list. The input list has two items, one is the APID, and the other is the filter factor. The filter factor is used to know the cadence of filtering and sending packets. This is maintained by a protected binary tree object which takes the APID of the packets from the input list, and adding them to a binary tree with the filter factor. When the packet is received, the APID is checked for filtering and then the filter factor to determine if we send them on or not. Packets that are not in the input list will not be filtered and sent as normal. As a note, the larger that the downsampled list is, the more there is to check in the supporting binary tree. It's recommended that the downsampled list contain less than a couple hundred items.
package Component.Ccsds_Downsampler.Implementation.Tester is

   use Component.Ccsds_Downsampler_Reciprocal;
   -- Invoker connector history packages:
   package Ccsds_Space_Packet_T_Recv_Sync_History_Package is new Printable_History (Ccsds_Space_Packet.T, Ccsds_Space_Packet.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Modified_Factor_Filter_History_Package is new Printable_History (Filter_Factor_Cmd_Type.T, Filter_Factor_Cmd_Type.Representation.Image);
   package Factor_Filter_Change_Failed_Invalid_Apid_History_Package is new Printable_History (Filter_Factor_Cmd_Type.T, Filter_Factor_Cmd_Type.Representation.Image);

   -- Data product history packages:
   package Total_Packets_Filtered_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Total_Packets_Passed_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Ccsds_Downsampler_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Downsampler.Implementation.Instance;
      -- Connector histories:
      Ccsds_Space_Packet_T_Recv_Sync_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Modified_Factor_Filter_History : Modified_Factor_Filter_History_Package.Instance;
      Factor_Filter_Change_Failed_Invalid_Apid_History : Factor_Filter_Change_Failed_Invalid_Apid_History_Package.Instance;
      -- Data product histories:
      Total_Packets_Filtered_History : Total_Packets_Filtered_History_Package.Instance;
      Total_Packets_Passed_History : Total_Packets_Passed_History_Package.Instance;
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
   -- The connector that will forward on unfiltered packets.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- The connector that sends a command response when received.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The Event connector to send the events specific to the component.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- This event indicates that the filter factor for a particular id has been set based on what was commanded.
   overriding procedure Modified_Factor_Filter (Self : in out Instance; Arg : in Filter_Factor_Cmd_Type.T);
   -- This event indicates that the command received a Apid it could not find so it fails since it cannot find the id.
   overriding procedure Factor_Filter_Change_Failed_Invalid_Apid (Self : in out Instance; Arg : in Filter_Factor_Cmd_Type.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the ccsds downsampler component.
   -- The total number of packets that have been filtered and not passed on.
   overriding procedure Total_Packets_Filtered (Self : in out Instance; Arg : in Packed_U16.T);
   -- The total number of packets that were not filtered and passed on.
   overriding procedure Total_Packets_Passed (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Custom functions
   -----------------------------------------------

   -- Override the reciprocal component base Dispatch_Data_Product procedure with our own. Since
   -- the data products are generated by the model, we need to allow data products to be produced that would
   -- otherwise cause the unit test to crash due to an out of range ID.
   overriding procedure Dispatch_Data_Product (Self : in out Instance; Dp : in Data_Product.T);

end Component.Ccsds_Downsampler.Implementation.Tester;
