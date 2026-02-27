--------------------------------------------------------------------------------
-- Event_Filter Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Event_Filter_Reciprocal;
with Sys_Time;
with Printable_History;
with Event.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Packet.Representation;
with Event;
with Invalid_Command_Info.Representation;
with Event_Filter_Single_Event_Cmd_Type.Representation;
with Event_Filter_Id_Range.Representation;
with Data_Product;
with Packed_U32.Representation;
with Event_Component_State_Type.Representation;

-- The Event Filter component is used to filter out event IDs from leaving the system. The component takes in a range of IDs
package Component.Event_Filter.Implementation.Tester is

   use Component.Event_Filter_Reciprocal;
   -- Invoker connector history packages:
   package Event_Forward_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Event history packages:
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Filtered_Event_History_Package is new Printable_History (Event_Filter_Single_Event_Cmd_Type.T, Event_Filter_Single_Event_Cmd_Type.Representation.Image);
   package Unfiltered_Event_History_Package is new Printable_History (Event_Filter_Single_Event_Cmd_Type.T, Event_Filter_Single_Event_Cmd_Type.Representation.Image);
   package Filtered_Event_Range_History_Package is new Printable_History (Event_Filter_Id_Range.T, Event_Filter_Id_Range.Representation.Image);
   package Unfiltered_Event_Range_History_Package is new Printable_History (Event_Filter_Id_Range.T, Event_Filter_Id_Range.Representation.Image);
   package Enable_Event_Filter_History_Package is new Printable_History (Natural, Natural'Image);
   package Disable_Event_Filter_History_Package is new Printable_History (Natural, Natural'Image);
   package Filter_Event_Invalid_Id_History_Package is new Printable_History (Event_Filter_Single_Event_Cmd_Type.T, Event_Filter_Single_Event_Cmd_Type.Representation.Image);
   package Unfilter_Event_Invalid_Id_History_Package is new Printable_History (Event_Filter_Single_Event_Cmd_Type.T, Event_Filter_Single_Event_Cmd_Type.Representation.Image);
   package Filter_Event_Range_Invalid_Id_History_Package is new Printable_History (Event_Filter_Id_Range.T, Event_Filter_Id_Range.Representation.Image);
   package Unfilter_Event_Range_Invalid_Id_History_Package is new Printable_History (Event_Filter_Id_Range.T, Event_Filter_Id_Range.Representation.Image);
   package Dump_Event_States_Received_History_Package is new Printable_History (Natural, Natural'Image);

   -- Data product history packages:
   package Total_Events_Filtered_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Total_Events_Unfiltered_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Component_Filter_State_History_Package is new Printable_History (Event_Component_State_Type.T, Event_Component_State_Type.Representation.Image);

   -- Packet history packages:
   package Event_Filter_State_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Event_Filter_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Event_Filter.Implementation.Instance;
      -- Connector histories:
      Event_Forward_T_Recv_Sync_History : Event_Forward_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Filtered_Event_History : Filtered_Event_History_Package.Instance;
      Unfiltered_Event_History : Unfiltered_Event_History_Package.Instance;
      Filtered_Event_Range_History : Filtered_Event_Range_History_Package.Instance;
      Unfiltered_Event_Range_History : Unfiltered_Event_Range_History_Package.Instance;
      Enable_Event_Filter_History : Enable_Event_Filter_History_Package.Instance;
      Disable_Event_Filter_History : Disable_Event_Filter_History_Package.Instance;
      Filter_Event_Invalid_Id_History : Filter_Event_Invalid_Id_History_Package.Instance;
      Unfilter_Event_Invalid_Id_History : Unfilter_Event_Invalid_Id_History_Package.Instance;
      Filter_Event_Range_Invalid_Id_History : Filter_Event_Range_Invalid_Id_History_Package.Instance;
      Unfilter_Event_Range_Invalid_Id_History : Unfilter_Event_Range_Invalid_Id_History_Package.Instance;
      Dump_Event_States_Received_History : Dump_Event_States_Received_History_Package.Instance;
      -- Data product histories:
      Total_Events_Filtered_History : Total_Events_Filtered_History_Package.Instance;
      Total_Events_Unfiltered_History : Total_Events_Unfiltered_History_Package.Instance;
      Component_Filter_State_History : Component_Filter_State_History_Package.Instance;
      -- Packet histories:
      Event_Filter_State_Packet_History : Event_Filter_State_Packet_History_Package.Instance;
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
   -- The Event connector to forward on events when the filtering is disabled, or if unknown events come in.
   overriding procedure Event_Forward_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The Event connector to send the events specific to the component.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Packet for sending a packet for all the event states.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- This event indicates that the state of an event was set to enabled for the filter.
   overriding procedure Filtered_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T);
   -- This event indicates that the state of an event was set to disabled for the filter.
   overriding procedure Unfiltered_Event (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T);
   -- This event indicates that the state of a range of events were set to enabled for the filter.
   overriding procedure Filtered_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T);
   -- This event indicates that the state of a range of events were set to disabled for the filter.
   overriding procedure Unfiltered_Event_Range (Self : in out Instance; Arg : in Event_Filter_Id_Range.T);
   -- This event indicates that the state of all events were set to enabled for the filter, but kept the internal state.
   overriding procedure Enable_Event_Filter (Self : in out Instance);
   -- This event indicates that the state of all events were set to disabled for the filter, but kept the internal state.
   overriding procedure Disable_Event_Filter (Self : in out Instance);
   -- This event indicates that the command to change the event state to enabled failed since the event ID was out of range.
   overriding procedure Filter_Event_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T);
   -- This event indicates that the command to change the event state to disable failed since the event ID was out of range.
   overriding procedure Unfilter_Event_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Single_Event_Cmd_Type.T);
   -- This event indicates that changing the state for the range to enabled, failed due to an invalid id.
   overriding procedure Filter_Event_Range_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Id_Range.T);
   -- This event indicates that changing the state for the range to disabled, failed due to an invalid id.
   overriding procedure Unfilter_Event_Range_Invalid_Id (Self : in out Instance; Arg : in Event_Filter_Id_Range.T);
   -- Event that indicates the process of building the packet that stores the event states has started and will send the packet once we go through a decrement cycle.
   overriding procedure Dump_Event_States_Received (Self : in out Instance);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the event filter component.
   -- The total number of events that have been filtered for the components lifetime.
   overriding procedure Total_Events_Filtered (Self : in out Instance; Arg : in Packed_U32.T);
   -- The total number of events that have been unfiltered for the components lifetime.
   overriding procedure Total_Events_Unfiltered (Self : in out Instance; Arg : in Packed_U32.T);
   -- The state of the master switch for filtering events.
   overriding procedure Component_Filter_State (Self : in out Instance; Arg : in Event_Component_State_Type.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packet to dump the event filter state of all events. Each event state takes one bit.
   -- The packet used to dump all the state information for which events are filtered and which are not. Each event ID takes a bit and any extra bits beyond the event range will show as not filtered.
   overriding procedure Event_Filter_State_Packet (Self : in out Instance; Arg : in Packet.T);

end Component.Event_Filter.Implementation.Tester;
