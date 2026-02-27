--------------------------------------------------------------------------------
-- Precision_Time_Protocol_Master Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Precision_Time_Protocol_Master.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Ptp_Time_Message_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Unexpected_Message_Type_History.Init (Depth => 100);
      Self.Unexpected_Transaction_Count_History.Init (Depth => 100);
      Self.Ptp_Enabled_History.Init (Depth => 100);
      Self.Ptp_Disabled_History.Init (Depth => 100);
      Self.Syncing_Once_History.Init (Depth => 100);
      Self.Queue_Overflowed_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Transaction_Number_History.Init (Depth => 100);
      Self.Follow_Up_Messages_Sent_History.Init (Depth => 100);
      Self.Delay_Request_Messages_Received_History.Init (Depth => 100);
      Self.Unexpected_Messages_Received_History.Init (Depth => 100);
      Self.Precision_Time_Protocol_State_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Ptp_Time_Message_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Unexpected_Message_Type_History.Destroy;
      Self.Unexpected_Transaction_Count_History.Destroy;
      Self.Ptp_Enabled_History.Destroy;
      Self.Ptp_Disabled_History.Destroy;
      Self.Syncing_Once_History.Destroy;
      Self.Queue_Overflowed_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Transaction_Number_History.Destroy;
      Self.Follow_Up_Messages_Sent_History.Destroy;
      Self.Delay_Request_Messages_Received_History.Destroy;
      Self.Unexpected_Messages_Received_History.Destroy;
      Self.Precision_Time_Protocol_State_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Ptp_Time_Message_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Ptp_Time_Message_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Async_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Ptp_Time_Message_Receive_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Ptp_Time_Message_Receive_T_Recv_Async_Access);
      Self.Attach_Follow_Up_Sys_Time_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Follow_Up_Sys_Time_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Sends PTP time messages. Used to send PTP time messages between the slave and master clocks.
   overriding procedure Ptp_Time_Message_T_Recv_Sync (Self : in out Instance; Arg : in Ptp_Time_Message.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ptp_Time_Message_T_Recv_Sync_History.Push (Arg);
   end Ptp_Time_Message_T_Recv_Sync;

   -- Used to get system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- This connector is used to register and respond to the components commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- The event send connector, sends events.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The data product invoker connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      if not Self.Expect_Tick_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Tick_T_Send was called!");
      else
         Self.Tick_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Tick_T_Send_Dropped := False;
      end if;
   end Tick_T_Send_Dropped;

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      if not Self.Expect_Command_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_T_Send was called!");
      else
         Self.Command_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_T_Send_Dropped := False;
      end if;
   end Command_T_Send_Dropped;

   -- This procedure is called when a Ptp_Time_Message_Receive_T_Send message is dropped due to a full queue.
   overriding procedure Ptp_Time_Message_Receive_T_Send_Dropped (Self : in out Instance; Arg : in Ptp_Time_Message_Receive.T) is
      Ignore : Ptp_Time_Message_Receive.T renames Arg;
   begin
      if not Self.Expect_Ptp_Time_Message_Receive_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Ptp_Time_Message_Receive_T_Send was called!");
      else
         Self.Ptp_Time_Message_Receive_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Ptp_Time_Message_Receive_T_Send_Dropped := False;
      end if;
   end Ptp_Time_Message_Receive_T_Send_Dropped;

   -- This procedure is called when a Follow_Up_Sys_Time_T_Send message is dropped due to a full queue.
   overriding procedure Follow_Up_Sys_Time_T_Send_Dropped (Self : in out Instance; Arg : in Sys_Time.T) is
      Ignore : Sys_Time.T renames Arg;
   begin
      if not Self.Expect_Follow_Up_Sys_Time_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Follow_Up_Sys_Time_T_Send was called!");
      else
         Self.Follow_Up_Sys_Time_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Follow_Up_Sys_Time_T_Send_Dropped := False;
      end if;
   end Follow_Up_Sys_Time_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Precision Time Protocol Master component.
   -- Received a message of unexpected type, message is sent as parameter.
   overriding procedure Unexpected_Message_Type (Self : in out Instance; Arg : in Ptp_Time_Message.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Message_Type_History.Push (Arg);
   end Unexpected_Message_Type;

   -- Received a message of unexpected type, message is sent as parameter along with the expected transaction number.
   overriding procedure Unexpected_Transaction_Count (Self : in out Instance; Arg : in Unexpected_Ptp_Transaction_Count.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Transaction_Count_History.Push (Arg);
   end Unexpected_Transaction_Count;

   -- The PTP has been enabled by command.
   overriding procedure Ptp_Enabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ptp_Enabled_History.Push (Arg);
   end Ptp_Enabled;

   -- The PTP has been disabled by command.
   overriding procedure Ptp_Disabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ptp_Disabled_History.Push (Arg);
   end Ptp_Disabled;

   -- A command was received to complete a single PTP transaction at the next Tick.
   overriding procedure Syncing_Once (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Syncing_Once_History.Push (Arg);
   end Syncing_Once;

   -- An incoming message was dropped due to the queue overflowing. The queue needs to be made larger.
   overriding procedure Queue_Overflowed (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Queue_Overflowed_History.Push (Arg);
   end Queue_Overflowed;

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
   --    Data products for the Precision Time Protocol Master component.
   -- The transaction number of the last sent Sync message.
   overriding procedure Transaction_Number (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Transaction_Number_History.Push (Arg);
   end Transaction_Number;

   -- The number of follow up messages sent.
   overriding procedure Follow_Up_Messages_Sent (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Follow_Up_Messages_Sent_History.Push (Arg);
   end Follow_Up_Messages_Sent;

   -- The number of delay request messages received.
   overriding procedure Delay_Request_Messages_Received (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Delay_Request_Messages_Received_History.Push (Arg);
   end Delay_Request_Messages_Received;

   -- The number of received messages that had unexpected transaction numbers or message types.
   overriding procedure Unexpected_Messages_Received (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Messages_Received_History.Push (Arg);
   end Unexpected_Messages_Received;

   -- The disable/enable state of the PTP component.
   overriding procedure Precision_Time_Protocol_State (Self : in out Instance; Arg : in Ptp_State.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Precision_Time_Protocol_State_History.Push (Arg);
   end Precision_Time_Protocol_State;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

end Component.Precision_Time_Protocol_Master.Implementation.Tester;
