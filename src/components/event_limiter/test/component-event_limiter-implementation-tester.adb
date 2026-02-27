--------------------------------------------------------------------------------
-- Event_Limiter Component Tester Body
--------------------------------------------------------------------------------

package body Component.Event_Limiter.Implementation.Tester is

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
      Self.Events_Limited_Since_Last_Tick_History.Init (Depth => 100);
      Self.Event_Limit_Enabled_History.Init (Depth => 100);
      Self.Event_Limit_Disabled_History.Init (Depth => 100);
      Self.Event_Limit_Range_Enabled_History.Init (Depth => 100);
      Self.Event_Limit_Range_Disabled_History.Init (Depth => 100);
      Self.Event_Limiting_Enabled_History.Init (Depth => 100);
      Self.Event_Limiting_Disabled_History.Init (Depth => 100);
      Self.Event_Limit_Enable_Invalid_Id_History.Init (Depth => 100);
      Self.Event_Limit_Disable_Invalid_Id_History.Init (Depth => 100);
      Self.Event_Limit_Range_Enabled_Invalid_Id_History.Init (Depth => 100);
      Self.Event_Limit_Range_Disabled_Invalid_Id_History.Init (Depth => 100);
      Self.Set_New_Persistence_History.Init (Depth => 100);
      Self.Dump_Event_States_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Limited_Events_Since_Tick_History.Init (Depth => 100);
      Self.Total_Events_Limited_History.Init (Depth => 100);
      Self.Component_Limiting_Enabled_Status_History.Init (Depth => 100);
      -- Packet histories:
      Self.Event_Limiter_State_Packet_History.Init (Depth => 100);

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
      Self.Events_Limited_Since_Last_Tick_History.Destroy;
      Self.Event_Limit_Enabled_History.Destroy;
      Self.Event_Limit_Disabled_History.Destroy;
      Self.Event_Limit_Range_Enabled_History.Destroy;
      Self.Event_Limit_Range_Disabled_History.Destroy;
      Self.Event_Limiting_Enabled_History.Destroy;
      Self.Event_Limiting_Disabled_History.Destroy;
      Self.Event_Limit_Enable_Invalid_Id_History.Destroy;
      Self.Event_Limit_Disable_Invalid_Id_History.Destroy;
      Self.Event_Limit_Range_Enabled_Invalid_Id_History.Destroy;
      Self.Event_Limit_Range_Disabled_Invalid_Id_History.Destroy;
      Self.Set_New_Persistence_History.Destroy;
      Self.Dump_Event_States_Received_History.Destroy;
      -- Data product histories:
      Self.Limited_Events_Since_Tick_History.Destroy;
      Self.Total_Events_Limited_History.Destroy;
      Self.Component_Limiting_Enabled_Status_History.Destroy;
      -- Packet histories:
      Self.Event_Limiter_State_Packet_History.Destroy;

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
   -- The Event connector to send the events specific to the component.
   overriding procedure Event_Forward_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Forward_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_Forward_T_Recv_Sync;

   -- The Event connector to forward on events if enabled and not limited
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

   -- An event that indicates how many events have been limited as well as up to the first 10 ids
   overriding procedure Events_Limited_Since_Last_Tick (Self : in out Instance; Arg : in Event_Limiter_Num_Events_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Events_Limited_Since_Last_Tick_History.Push (Arg);
   end Events_Limited_Since_Last_Tick;

   -- This event indicates that the state of an event was set to enabled for the limiter.
   overriding procedure Event_Limit_Enabled (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Enabled_History.Push (Arg);
   end Event_Limit_Enabled;

   -- This event indicates that the state of an event was set to enabled for the limiter.
   overriding procedure Event_Limit_Disabled (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Disabled_History.Push (Arg);
   end Event_Limit_Disabled;

   -- This event indicates that the state of a range of events were set to enabled for the limiter.
   overriding procedure Event_Limit_Range_Enabled (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Range_Enabled_History.Push (Arg);
   end Event_Limit_Range_Enabled;

   -- This event indicates that the state of a range of events were set to disabled for the limiter.
   overriding procedure Event_Limit_Range_Disabled (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Range_Disabled_History.Push (Arg);
   end Event_Limit_Range_Disabled;

   -- This event indicates that the state of all events were set to enabled for the limiter.
   overriding procedure Event_Limiting_Enabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limiting_Enabled_History.Push (Arg);
   end Event_Limiting_Enabled;

   -- This event indicates that the state of all events were set to disabled for the limiter.
   overriding procedure Event_Limiting_Disabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limiting_Disabled_History.Push (Arg);
   end Event_Limiting_Disabled;

   -- This event indicates that the command to change the event state to enabled failed since the event ID was out of range.
   overriding procedure Event_Limit_Enable_Invalid_Id (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Enable_Invalid_Id_History.Push (Arg);
   end Event_Limit_Enable_Invalid_Id;

   -- This event indicates that the command to change the event state to disable failed since the event ID was out of range.
   overriding procedure Event_Limit_Disable_Invalid_Id (Self : in out Instance; Arg : in Event_Single_State_Cmd_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Disable_Invalid_Id_History.Push (Arg);
   end Event_Limit_Disable_Invalid_Id;

   -- This event indicates that changing the state for the range failed due to an invalid id.
   overriding procedure Event_Limit_Range_Enabled_Invalid_Id (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Range_Enabled_Invalid_Id_History.Push (Arg);
   end Event_Limit_Range_Enabled_Invalid_Id;

   -- This event indicates that changing the state for the range failed due to an invalid id.
   overriding procedure Event_Limit_Range_Disabled_Invalid_Id (Self : in out Instance; Arg : in Event_Limiter_Id_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limit_Range_Disabled_Invalid_Id_History.Push (Arg);
   end Event_Limit_Range_Disabled_Invalid_Id;

   -- Indicates that the persistence of the number of events until we limit was changed to a new value between 1 and 7.
   overriding procedure Set_New_Persistence (Self : in out Instance; Arg : in Event_Limiter_Persistence_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Set_New_Persistence_History.Push (Arg);
   end Set_New_Persistence;

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
   --    Data products for the pid controller component.
   -- The number of events that were limited since the last tick.
   overriding procedure Limited_Events_Since_Tick (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Limited_Events_Since_Tick_History.Push (Arg);
   end Limited_Events_Since_Tick;

   -- The total number of events that have been limited so far
   overriding procedure Total_Events_Limited (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Total_Events_Limited_History.Push (Arg);
   end Total_Events_Limited;

   -- The number of events that were limited since the last tick.
   overriding procedure Component_Limiting_Enabled_Status (Self : in out Instance; Arg : in Event_Enable_State_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Component_Limiting_Enabled_Status_History.Push (Arg);
   end Component_Limiting_Enabled_Status;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packet to dump the event state of all events. Each event state takes one bit.
   -- The packet used to dump all the state information for which events are limited and which are not.
   overriding procedure Event_Limiter_State_Packet (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_Limiter_State_Packet_History.Push (Arg);
   end Event_Limiter_State_Packet;

end Component.Event_Limiter.Implementation.Tester;
