--------------------------------------------------------------------------------
-- Time_At_Tone_Master Component Tester Body
--------------------------------------------------------------------------------

package body Component.Time_At_Tone_Master.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Time_Message_Recv_Sync_History.Init (Depth => 100);
      Self.Tone_Message_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Time_At_Tone_Enabled_History.Init (Depth => 100);
      Self.Time_At_Tone_Disabled_History.Init (Depth => 100);
      Self.Sending_Sync_Once_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Tone_Messages_Sent_History.Init (Depth => 100);
      Self.Time_At_Tone_State_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Time_Message_Recv_Sync_History.Destroy;
      Self.Tone_Message_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Time_At_Tone_Enabled_History.Destroy;
      Self.Time_At_Tone_Disabled_History.Destroy;
      Self.Sending_Sync_Once_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Tone_Messages_Sent_History.Destroy;
      Self.Time_At_Tone_State_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Time_Message_Send (To_Component => Self'Unchecked_Access, Hook => Self.Time_Message_Recv_Sync_Access);
      Self.Component_Instance.Attach_Tone_Message_Send (To_Component => Self'Unchecked_Access, Hook => Self.Tone_Message_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Time message send connector, sends a message with the time the tone message will be sent.
   overriding procedure Time_Message_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Time_Message_Recv_Sync_History.Push (Arg);
   end Time_Message_Recv_Sync;

   -- Tone message send connector.
   overriding procedure Tone_Message_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Tone_Message_Recv_Sync_History.Push (Arg);
   end Tone_Message_Recv_Sync;

   -- Used to get system time, used by the master version of the component to get the current time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
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

   -- The data product invoker connector
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- The event send connector
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Time at Tone Master component.
   -- The time at tone has been enabled by command.
   overriding procedure Time_At_Tone_Enabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Time_At_Tone_Enabled_History.Push (Arg);
   end Time_At_Tone_Enabled;

   -- The time at tone has been disabled by command.
   overriding procedure Time_At_Tone_Disabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Time_At_Tone_Disabled_History.Push (Arg);
   end Time_At_Tone_Disabled;

   -- The component will send the time at tone message and tone message at the next received tick.
   overriding procedure Sending_Sync_Once (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sending_Sync_Once_History.Push (Arg);
   end Sending_Sync_Once;

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
   --    Data products for the Time at Tone Master component.
   -- The number of tone messages sent.
   overriding procedure Tone_Messages_Sent (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Tone_Messages_Sent_History.Push (Arg);
   end Tone_Messages_Sent;

   -- The disable/enable state of the time at tone component.
   overriding procedure Time_At_Tone_State (Self : in out Instance; Arg : in Tat_State.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Time_At_Tone_State_History.Push (Arg);
   end Time_At_Tone_State;

end Component.Time_At_Tone_Master.Implementation.Tester;
