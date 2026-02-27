--------------------------------------------------------------------------------
-- Precision_Time_Protocol_Master Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Command;
with Ptp_Time_Message;
with Sys_Time;

-- This is the Precision Time Protocol (PTP) Master component. This component implements the master portion of the protocol. Any PTP slaves can use the messages from this component to measure their system time relative to the master, or use the master to synchronize their clocks.
package Component.Precision_Time_Protocol_Master.Implementation is

   -- The component class instance record:
   type Instance is new Precision_Time_Protocol_Master.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Sync_Period : Positive - The number of ticks between sending precision time protocol messages. A value of zero disables syncing.
   -- Enabled_State : Ptp_State.Ptp_State_Type - Is precision time protocol enabled or disabled by default at startup.
   --
   overriding procedure Init (Self : in out Instance; Sync_Period : in Positive := 1; Enabled_State : in Ptp_State.Ptp_State_Type := Ptp_State.Enabled);

private

   -- The component class instance record:
   type Instance is new Precision_Time_Protocol_Master.Base_Instance with record
      -- Variables for determining when to send sync messages.
      State : Ptp_State.Ptp_State_Type := Ptp_State.Enabled;
      Cycle_Count : Natural := Natural'First;
      Sync_Period : Positive := Positive'First;
      -- Boolean to allow us to sync once:
      Sync_Once : Boolean := False;
      -- Counters for keeping track of messages sent and received
      -- by PTP master.
      Transaction_Count : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
      Follow_Up_Message_Count : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
      Delay_Request_Message_Count : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
      Unexpected_Message_Count : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick input that is used to calculate time message frequency. The sync_period can be used to set how many ticks should be received before the master clock starts the update process.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T);
   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T);
   -- The command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- Receives PTP time messages. Used to send PTP time messages between the slave and master clocks.
   overriding procedure Ptp_Time_Message_Receive_T_Recv_Async (Self : in out Instance; Arg : in Ptp_Time_Message_Receive.T);
   -- This procedure is called when a Ptp_Time_Message_Receive_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ptp_Time_Message_Receive_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ptp_Time_Message_Receive.T);
   -- Receives and forwards the PTP time for a follow-up message (if connected). The time received is an accurate time stamp of when the Sync message was sent. This time can be provided to this component by a lower level component which actually records the time the Sync message leaves the system.
   overriding procedure Follow_Up_Sys_Time_T_Recv_Async (Self : in out Instance; Arg : in Sys_Time.T);
   -- This procedure is called when a Follow_Up_Sys_Time_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Follow_Up_Sys_Time_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Sys_Time.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ptp_Time_Message_T_Send message is dropped due to a full queue.
   overriding procedure Ptp_Time_Message_T_Send_Dropped (Self : in out Instance; Arg : in Ptp_Time_Message.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Precision Time Protocol Master component.
   -- This enables the sending of PTP messages.
   overriding function Enable_Precision_Time_Protocol (Self : in out Instance) return Command_Execution_Status.E;
   -- This enables the sending of PTP messages.
   overriding function Disable_Precision_Time_Protocol (Self : in out Instance) return Command_Execution_Status.E;
   -- This sends a PTP sync message at the next tick, regardless of the current sync period. This is useful during testing to send a sync one time.
   overriding function Sync_Once (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Precision_Time_Protocol_Master.Implementation;
