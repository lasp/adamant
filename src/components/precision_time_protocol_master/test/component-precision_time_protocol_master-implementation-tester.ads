--------------------------------------------------------------------------------
-- Precision_Time_Protocol_Master Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Precision_Time_Protocol_Master_Reciprocal;
with Sys_Time;
with Printable_History;
with Ptp_Time_Message.Representation;
with Sys_Time.Representation;
with Command_Response.Representation;
with Event.Representation;
with Data_Product.Representation;
with Event;
with Unexpected_Ptp_Transaction_Count.Representation;
with Invalid_Command_Info.Representation;
with Data_Product;
with Packed_U16.Representation;
with Ptp_State.Representation;

-- This is the Precision Time Protocol (PTP) Master component. This component implements the master portion of the protocol. Any PTP slaves can use the messages from this component to measure their system time relative to the master, or use the master to synchronize their clocks.
package Component.Precision_Time_Protocol_Master.Implementation.Tester is

   use Component.Precision_Time_Protocol_Master_Reciprocal;
   -- Invoker connector history packages:
   package Ptp_Time_Message_T_Recv_Sync_History_Package is new Printable_History (Ptp_Time_Message.T, Ptp_Time_Message.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);

   -- Event history packages:
   package Unexpected_Message_Type_History_Package is new Printable_History (Ptp_Time_Message.T, Ptp_Time_Message.Representation.Image);
   package Unexpected_Transaction_Count_History_Package is new Printable_History (Unexpected_Ptp_Transaction_Count.T, Unexpected_Ptp_Transaction_Count.Representation.Image);
   package Ptp_Enabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Ptp_Disabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Syncing_Once_History_Package is new Printable_History (Natural, Natural'Image);
   package Queue_Overflowed_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Transaction_Number_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Follow_Up_Messages_Sent_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Delay_Request_Messages_Received_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Unexpected_Messages_Received_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Precision_Time_Protocol_State_History_Package is new Printable_History (Ptp_State.T, Ptp_State.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Precision_Time_Protocol_Master_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Precision_Time_Protocol_Master.Implementation.Instance;
      -- Connector histories:
      Ptp_Time_Message_T_Recv_Sync_History : Ptp_Time_Message_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Unexpected_Message_Type_History : Unexpected_Message_Type_History_Package.Instance;
      Unexpected_Transaction_Count_History : Unexpected_Transaction_Count_History_Package.Instance;
      Ptp_Enabled_History : Ptp_Enabled_History_Package.Instance;
      Ptp_Disabled_History : Ptp_Disabled_History_Package.Instance;
      Syncing_Once_History : Syncing_Once_History_Package.Instance;
      Queue_Overflowed_History : Queue_Overflowed_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Transaction_Number_History : Transaction_Number_History_Package.Instance;
      Follow_Up_Messages_Sent_History : Follow_Up_Messages_Sent_History_Package.Instance;
      Delay_Request_Messages_Received_History : Delay_Request_Messages_Received_History_Package.Instance;
      Unexpected_Messages_Received_History : Unexpected_Messages_Received_History_Package.Instance;
      Precision_Time_Protocol_State_History : Precision_Time_Protocol_State_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Tick_T_Send_Dropped : Boolean := False;
      Tick_T_Send_Dropped_Count : Natural := 0;
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Ptp_Time_Message_Receive_T_Send_Dropped : Boolean := False;
      Ptp_Time_Message_Receive_T_Send_Dropped_Count : Natural := 0;
      Expect_Follow_Up_Sys_Time_T_Send_Dropped : Boolean := False;
      Follow_Up_Sys_Time_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Sends PTP time messages. Used to send PTP time messages between the slave and master clocks.
   overriding procedure Ptp_Time_Message_T_Recv_Sync (Self : in out Instance; Arg : in Ptp_Time_Message.T);
   -- Used to get system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to register and respond to the components commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The event send connector, sends events.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The data product invoker connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T);

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Ptp_Time_Message_Receive_T_Send message is dropped due to a full queue.
   overriding procedure Ptp_Time_Message_Receive_T_Send_Dropped (Self : in out Instance; Arg : in Ptp_Time_Message_Receive.T);

   -- This procedure is called when a Follow_Up_Sys_Time_T_Send message is dropped due to a full queue.
   overriding procedure Follow_Up_Sys_Time_T_Send_Dropped (Self : in out Instance; Arg : in Sys_Time.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Precision Time Protocol Master component.
   -- Received a message of unexpected type, message is sent as parameter.
   overriding procedure Unexpected_Message_Type (Self : in out Instance; Arg : in Ptp_Time_Message.T);
   -- Received a message of unexpected type, message is sent as parameter along with the expected transaction number.
   overriding procedure Unexpected_Transaction_Count (Self : in out Instance; Arg : in Unexpected_Ptp_Transaction_Count.T);
   -- PTP been enabled by command.
   overriding procedure Ptp_Enabled (Self : in out Instance);
   -- PTP has been disabled by command.
   overriding procedure Ptp_Disabled (Self : in out Instance);
   -- A command was received to complete a single PTP transaction at the next Tick.
   overriding procedure Syncing_Once (Self : in out Instance);
   -- An incoming message was dropped due to the queue overflowing. The queue needs to be made larger.
   overriding procedure Queue_Overflowed (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Precision Time Protocol Master component.
   -- The transaction number of the last sent Sync message.
   overriding procedure Transaction_Number (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of follow up messages sent.
   overriding procedure Follow_Up_Messages_Sent (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of delay request messages received.
   overriding procedure Delay_Request_Messages_Received (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of received messages that had unexpected transaction numbers or message types.
   overriding procedure Unexpected_Messages_Received (Self : in out Instance; Arg : in Packed_U16.T);
   -- The disable/enable state of the PTP component.
   overriding procedure Precision_Time_Protocol_State (Self : in out Instance; Arg : in Ptp_State.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Precision_Time_Protocol_Master.Implementation.Tester;
