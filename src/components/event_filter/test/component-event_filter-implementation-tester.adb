--------------------------------------------------------------------------------
-- Event_Filter Component Tester Body
--------------------------------------------------------------------------------

package body Component.Event_Filter.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Event_Forward_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Filtered_Event_History.Init (Depth => 100);
      Self.Unfiltered_Event_History.Init (Depth => 100);
      Self.Filtered_Event_Range_History.Init (Depth => 100);
      Self.Unfiltered_Event_Range_History.Init (Depth => 100);
      Self.Enable_Event_Filter_History.Init (Depth => 100);
      Self.Disable_Event_Filter_History.Init (Depth => 100);
      Self.Filter_Event_Invalid_Id_History.Init (Depth => 100);
      Self.Unfilter_Event_Invalid_Id_History.Init (Depth => 100);
      Self.Filter_Event_Range_Invalid_Id_History.Init (Depth => 100);
      Self.Unfilter_Event_Range_Invalid_Id_History.Init (Depth => 100);
      Self.Dump_Event_States_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Total_Events_Filtered_History.Init (Depth => 100);
      Self.Total_Events_Unfiltered_History.Init (Depth => 100);
      Self.Component_Filter_State_History.Init (Depth => 100);
      -- Packet histories:
      Self.Event_Filter_State_Packet_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Event_Forward_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Invalid_Command_Received_History.Destroy;
      Self.Filtered_Event_History.Destroy;
      Self.Unfiltered_Event_History.Destroy;
      Self.Filtered_Event_Range_History.Destroy;
      Self.Unfiltered_Event_Range_History.Destroy;
      Self.Enable_Event_Filter_History.Destroy;
      Self.Disable_Event_Filter_History.Destroy;
      Self.Filter_Event_Invalid_Id_History.Destroy;
      Self.Unfilter_Event_Invalid_Id_History.Destroy;
      Self.Filter_Event_Range_Invalid_Id_History.Destroy;
      Self.Unfilter_Event_Range_Invalid_Id_History.Destroy;
      Self.Dump_Event_States_Received_History.Destroy;
      -- Data product histories:
      Self.Total_Events_Filtered_History.Destroy;
      Self.Total_Events_Unfiltered_History.Destroy;
      Self.Component_Filter_State_History.Destroy;
      -- Packet histories:
      Self.Event_Filter_State_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Event_Forward_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_Forward_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_Event_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Event_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The Event connector to forward on events when the filtering is disabled, or if unknown events come in.
   overriding procedure Event_Forward_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Forward_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_Forward_T_Recv_Sync;

   -- The Event connector to send the events specific to the component.
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

   -- This connector is used to register and respond to the component's commands.
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

   -- Packet for sending a packet for all the event states.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- This event indicates that the state of an event was set to enabled for the filter.
   overriding procedure Filtered_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Filtered_Event_History.Push (Arg);
   end Filtered_Event;

   -- This event indicates that the state of an event was set to disabled for the filter.
   overriding procedure Unfiltered_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unfiltered_Event_History.Push (Arg);
   end Unfiltered_Event;

   -- This event indicates that the state of a range of events were set to enabled for the filter.
   overriding procedure Filtered_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Filtered_Event_Range_History.Push (Arg);
   end Filtered_Event_Range;

   -- This event indicates that the state of a range of events were set to disabled for the filter.
   overriding procedure Unfiltered_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unfiltered_Event_Range_History.Push (Arg);
   end Unfiltered_Event_Range;

   -- This event indicates that the state of all events were set to enabled for the filter, but kept the internal state.
   overriding procedure Enable_Event_Filter (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Enable_Event_Filter_History.Push (Arg);
   end Enable_Event_Filter;

   -- This event indicates that the state of all events were set to disabled for the filter, but kept the internal state.
   overriding procedure Disable_Event_Filter (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Disable_Event_Filter_History.Push (Arg);
   end Disable_Event_Filter;

   -- This event indicates that the command to change the event state to enabled failed since the event ID was out of range.
   overriding procedure Filter_Event_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Filter_Event_Invalid_Id_History.Push (Arg);
   end Filter_Event_Invalid_Id;

   -- This event indicates that the command to change the event state to disable failed since the event ID was out of range.
   overriding procedure Unfilter_Event_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unfilter_Event_Invalid_Id_History.Push (Arg);
   end Unfilter_Event_Invalid_Id;

   -- This event indicates that changing the state for the range to enabled, failed due to an invalid id.
   overriding procedure Filter_Event_Range_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Filter_Event_Range_Invalid_Id_History.Push (Arg);
   end Filter_Event_Range_Invalid_Id;

   -- This event indicates that changing the state for the range to disabled, failed due to an invalid id.
   overriding procedure Unfilter_Event_Range_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unfilter_Event_Range_Invalid_Id_History.Push (Arg);
   end Unfilter_Event_Range_Invalid_Id;

   -- Event that indicates the process of building the packet that stores the event states has started and will send the packet once we go through a decrement cycle.
   overriding procedure Dump_Event_States_Received (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dump_Event_States_Received_History.Push (Arg);
   end Dump_Event_States_Received;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the event filter component.
   -- The total number of events that have been filtered for the components lifetime.
   overriding procedure Total_Events_Filtered (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Total_Events_Filtered_History.Push (Arg);
   end Total_Events_Filtered;

   -- The total number of events that have been unfiltered for the components lifetime.
   overriding procedure Total_Events_Unfiltered (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Total_Events_Unfiltered_History.Push (Arg);
   end Total_Events_Unfiltered;

   -- The state of the master switch for filtering events.
   overriding procedure Component_Filter_State (Self : in out Instance; Arg : in Event_Component_State_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Component_Filter_State_History.Push (Arg);
   end Component_Filter_State;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packet to dump the event filter state of all events. Each event state takes one bit.
   -- The packet used to dump all the state information for which events are filtered and which are not. Each event ID takes a bit and any extra bits beyond the event range will show as not filtered.
   overriding procedure Event_Filter_State_Packet (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Filter_State_Packet_History.Push (Arg);
   end Event_Filter_State_Packet;

end Component.Event_Filter.Implementation.Tester;
