--------------------------------------------------------------------------------
-- Event_Limiter Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Event_Limiter_Reciprocal;
with Sys_Time;
with Printable_History;
with Event.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Packet.Representation;
with Data_Product;
with Packed_U16.Representation;
with Packed_U32.Representation;
with Event_Enable_State_Type.Representation;
with Event;
with Invalid_Command_Info.Representation;
with Event_Limiter_Num_Events_Type.Representation;
with Event_Single_State_Cmd_Type.Representation;
with Event_Limiter_Id_Range.Representation;
with Event_Limiter_Persistence_Type.Representation;

-- The Event Limiter takes in events and checks that there have not been too many events of a single ID within one tick that would cause the system to flood with events. Every tick the event counts are decremented to keep events flowing at the appropriate rate. The component takes in a start and stop ID and should include all IDs in that range. The component also takes a list of event ID's to set as disabled by default. All others will be enabled by default.
package Component.Event_Limiter.Implementation.Tester is

   use Component.Event_Limiter_Reciprocal;
   -- Logger
   --package Logger_Package is new Logger.Unit_Test_Logger;

   -- Invoker connector history packages:
   package Event_Forward_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Event history packages:
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Events_Limited_Since_Last_Tick_History_Package is new Printable_History (Event_Limiter_Num_Events_Type.T, Event_Limiter_Num_Events_Type.Representation.Image);
   package Event_Limit_Enabled_History_Package is new Printable_History (Event_Single_State_Cmd_Type.T, Event_Single_State_Cmd_Type.Representation.Image);
   package Event_Limit_Disabled_History_Package is new Printable_History (Event_Single_State_Cmd_Type.T, Event_Single_State_Cmd_Type.Representation.Image);
   package Event_Limit_Range_Enabled_History_Package is new Printable_History (Event_Limiter_Id_Range.T, Event_Limiter_Id_Range.Representation.Image);
   package Event_Limit_Range_Disabled_History_Package is new Printable_History (Event_Limiter_Id_Range.T, Event_Limiter_Id_Range.Representation.Image);
   package Event_Limiting_Enabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Event_Limiting_Disabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Event_Limit_Enable_Invalid_Id_History_Package is new Printable_History (Event_Single_State_Cmd_Type.T, Event_Single_State_Cmd_Type.Representation.Image);
   package Event_Limit_Disable_Invalid_Id_History_Package is new Printable_History (Event_Single_State_Cmd_Type.T, Event_Single_State_Cmd_Type.Representation.Image);
   package Event_Limit_Range_Enabled_Invalid_Id_History_Package is new Printable_History (Event_Limiter_Id_Range.T, Event_Limiter_Id_Range.Representation.Image);
   package Event_Limit_Range_Disabled_Invalid_Id_History_Package is new Printable_History (Event_Limiter_Id_Range.T, Event_Limiter_Id_Range.Representation.Image);
   package Set_New_Persistence_History_Package is new Printable_History (Event_Limiter_Persistence_Type.T, Event_Limiter_Persistence_Type.Representation.Image);
   package Dump_Event_States_Received_History_Package is new Printable_History (Natural, Natural'Image);

   -- Data product history packages:
   package Limited_Events_Since_Tick_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Total_Events_Limited_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Component_Limiting_Enabled_Status_History_Package is new Printable_History (Event_Enable_State_Type.T, Event_Enable_State_Type.Representation.Image);

   -- Packet history packages:
   package Event_Limiter_State_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Event_Limiter_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Event_Limiter.Implementation.Instance;

      --Logger_Instance : Logger_Package.Instance;
      -- Connector histories:
      Event_Forward_T_Recv_Sync_History : Event_Forward_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Events_Limited_Since_Last_Tick_History : Events_Limited_Since_Last_Tick_History_Package.Instance;
      Event_Limit_Enabled_History : Event_Limit_Enabled_History_Package.Instance;
      Event_Limit_Disabled_History : Event_Limit_Disabled_History_Package.Instance;
      Event_Limit_Range_Enabled_History : Event_Limit_Range_Enabled_History_Package.Instance;
      Event_Limit_Range_Disabled_History : Event_Limit_Range_Disabled_History_Package.Instance;
      Event_Limiting_Enabled_History : Event_Limiting_Enabled_History_Package.Instance;
      Event_Limiting_Disabled_History : Event_Limiting_Disabled_History_Package.Instance;
      Event_Limit_Enable_Invalid_Id_History : Event_Limit_Enable_Invalid_Id_History_Package.Instance;
      Event_Limit_Disable_Invalid_Id_History : Event_Limit_Disable_Invalid_Id_History_Package.Instance;
      Event_Limit_Range_Enabled_Invalid_Id_History : Event_Limit_Range_Enabled_Invalid_Id_History_Package.Instance;
      Event_Limit_Range_Disabled_Invalid_Id_History : Event_Limit_Range_Disabled_Invalid_Id_History_Package.Instance;
      Set_New_Persistence_History : Set_New_Persistence_History_Package.Instance;
      Dump_Event_States_Received_History : Dump_Event_States_Received_History_Package.Instance;
      -- Data product histories:
      Limited_Events_Since_Tick_History : Limited_Events_Since_Tick_History_Package.Instance;
      Total_Events_Limited_History : Total_Events_Limited_History_Package.Instance;
      Component_Limiting_Enabled_Status_History : Component_Limiting_Enabled_Status_History_Package.Instance;
      -- Packet histories:
      Event_Limiter_State_Packet_History : Event_Limiter_State_Packet_History_Package.Instance;

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
   -- The Event connector to send the events specific to the component.
   overriding procedure Event_Forward_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The Event connector to forward on events if enabled and not limited
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
   -- An event that indicates how many events have been limited as well as up to the first 10 ids
   overriding procedure Events_Limited_Since_Last_Tick (Self : in out Instance; Arg : in Event_Limiter_Num_Events_Type.T);
   -- This event indicates that the state of an event was set to enabled for the limiter.
   overriding procedure Event_Limit_Enabled (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T);
   -- This event indicates that the state of an event was set to enabled for the limiter.
   overriding procedure Event_Limit_Disabled (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T);
   -- This event indicates that the state of a range of events were set to enabled for the limiter.
   overriding procedure Event_Limit_Range_Enabled (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T);
   -- This event indicates that the state of a range of events were set to disabled for the limiter.
   overriding procedure Event_Limit_Range_Disabled (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T);
   -- This event indicates that the state of all events were set to enabled for the limiter.
   overriding procedure Event_Limiting_Enabled (Self : in out Instance);
   -- This event indicates that the state of all events were set to disabled for the limiter.
   overriding procedure Event_Limiting_Disabled (Self : in out Instance);
   -- This event indicates that the command to change the event state to enabled failed since the event ID was out of range.
   overriding procedure Event_Limit_Enable_Invalid_Id (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T);
   -- This event indicates that the command to change the event state to disable failed since the event ID was out of range.
   overriding procedure Event_Limit_Disable_Invalid_Id (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T);
   -- This event indicates that changing the state for the range failed due to an invalid id.
   overriding procedure Event_Limit_Range_Enabled_Invalid_Id (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T);
   -- This event indicates that changing the state for the range failed due to an invalid id.
   overriding procedure Event_Limit_Range_Disabled_Invalid_Id (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T);
   -- Indicates that the persistence of the number of events until we limit was changed to a new value between 1 and 7.
   overriding procedure Set_New_Persistence (Self : in out Instance; Arg : in Event_Limiter_Persistence_Type.T);
   -- Event that indicates the process of building the packet that stores the event states has started and will send the packet once we go through a decrement cycle.
   overriding procedure Dump_Event_States_Received (Self : in out Instance);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the pid controller component.
   -- The number of events that were limited since the last tick.
   overriding procedure Limited_Events_Since_Tick (Self : in out Instance; Arg : in Packed_U16.T);
   -- The total number of events that have been limited so far
   overriding procedure Total_Events_Limited (Self : in out Instance; Arg : in Packed_U32.T);
   -- The number of events that were limited since the last tick.
   overriding procedure Component_Limiting_Enabled_Status (Self : in out Instance; Arg : in Event_Enable_State_Type.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packet to dump the event state of all events. Each event state takes one bit.
   -- The packet used to dump all the state information for which events are limited and which are not.
   overriding procedure Event_Limiter_State_Packet (Self : in out Instance; Arg : in Packet.T);

end Component.Event_Limiter.Implementation.Tester;
