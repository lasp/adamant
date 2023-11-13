--------------------------------------------------------------------------------
-- Ccsds_Downsampler Component Tester Body
--------------------------------------------------------------------------------

package body Component.Ccsds_Downsampler.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Modified_Factor_Filter_History.Init (Depth => 100);
      Self.Factor_Filter_Change_Failed_Invalid_Apid_History.Init (Depth => 100);
      -- Data product histories:
      Self.Total_Packets_Filtered_History.Init (Depth => 100);
      Self.Total_Packets_Passed_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Invalid_Command_Received_History.Destroy;
      Self.Modified_Factor_Filter_History.Destroy;
      Self.Factor_Filter_Change_Failed_Invalid_Apid_History.Destroy;
      -- Data product histories:
      Self.Total_Packets_Filtered_History.Destroy;
      Self.Total_Packets_Passed_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Ccsds_Space_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Ccsds_Space_Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Ccsds_Space_Packet_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Ccsds_Space_Packet_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The connector that will forward on unfiltered packets.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Push (Arg);
   end Ccsds_Space_Packet_T_Recv_Sync;

   -- The connector that sends a command response when received.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- The Event connector to send the events specifc to the component.
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

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- This event indicates that the filter factor for a particular id has been set based on what was commanded.
   overriding procedure Modified_Factor_Filter (Self : in out Instance; Arg : in Filter_Factor_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Modified_Factor_Filter_History.Push (Arg);
   end Modified_Factor_Filter;

   -- This event indicates that the command received a Apid it could not find so it fails since it cannot find the id.
   overriding procedure Factor_Filter_Change_Failed_Invalid_Apid (Self : in out Instance; Arg : in Filter_Factor_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Factor_Filter_Change_Failed_Invalid_Apid_History.Push (Arg);
   end Factor_Filter_Change_Failed_Invalid_Apid;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the ccsds downsampler component.
   -- The total number of packets that have been filtered and not passed on.
   overriding procedure Total_Packets_Filtered (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Total_Packets_Filtered_History.Push (Arg);
   end Total_Packets_Filtered;

   -- The total number of packets that were not filtered and passed on.
   overriding procedure Total_Packets_Passed (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Total_Packets_Passed_History.Push (Arg);
   end Total_Packets_Passed;

   -----------------------------------------------
   -- Custom functions
   -----------------------------------------------
   overriding procedure Dispatch_Data_Product (Self : in out Instance; Dp : in Data_Product.T) is
      -- Dispatch to dummy no matter what.
      Dispatch_To : constant Dispatch_Data_Product_Procedure := Data_Product_Id_Table (Ccsds_Downsampler_Data_Products.Local_Data_Product_Id_Type'First);
   begin
      Dispatch_To (Component.Ccsds_Downsampler_Reciprocal.Base_Instance (Self), Dp);
   end Dispatch_Data_Product;

end Component.Ccsds_Downsampler.Implementation.Tester;
