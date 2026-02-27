--------------------------------------------------------------------------------
-- Precision_Time_Protocol_Master Component Implementation Body
--------------------------------------------------------------------------------

with Ptp_Enums;

package body Component.Precision_Time_Protocol_Master.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Sync_Period : Positive - The number of ticks between sending precision time protocol messages.
   -- Enabled_State : Ptp_State.Ptp_State_Type - Is precision time protocol enabled or disabled by default at startup.
   --
   overriding procedure Init (Self : in out Instance; Sync_Period : in Positive := 1; Enabled_State : in Ptp_State.Ptp_State_Type := Ptp_State.Enabled) is
   begin
      -- Save variables:
      Self.Sync_Period := Sync_Period;
      Self.State := Enabled_State;
   end Init;

   -- Send out data products:
   overriding procedure Set_Up (Self : in out Instance) is
      Time_Stamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Update data products after init:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Transaction_Number (Time_Stamp, (Value => Self.Transaction_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Follow_Up_Messages_Sent (Time_Stamp, (Value => Self.Follow_Up_Message_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Delay_Request_Messages_Received (Time_Stamp, (Value => Self.Delay_Request_Message_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Unexpected_Messages_Received (Time_Stamp, (Value => Self.Unexpected_Message_Count)));
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Precision_Time_Protocol_State (Time_Stamp, (State => Self.State)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Tick input that is used to calculate time message frequency. The sync_period can be used to set how many ticks should be received before the master clock starts the update process.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
      use Ptp_State;
   begin
      -- Check the cycle count to see if it is time to send out a new sync message
      -- to any PTP slaves.
      if Self.Sync_Once or else
         (Self.State = Enabled and then (Self.Cycle_Count mod Self.Sync_Period) = 0)
      then

         declare
            use Ptp_Enums.Ptp_Message_Type;
            Time_Stamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
         begin
            -- We increment the transaction count number prior to sending out the message.
            -- Transaction_Number data product is defined as the last sent sequence number.
            Self.Transaction_Count := @ + 1;

            -- Send out sync message:
            Self.Ptp_Time_Message_T_Send ((Message_Type => Sync, Transaction_Count => Self.Transaction_Count, Time_Stamp => Time_Stamp));

            -- Update data products:
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Transaction_Number (Time_Stamp, (Value => Self.Transaction_Count)));
         end;

         -- Reset sync_once:
         Self.Sync_Once := False;
      end if;

      -- Increment cycle count.
      Self.Cycle_Count := (@ + 1) mod Self.Sync_Period;
   end Tick_T_Recv_Async;

   -- The command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -- Receives PTP time messages. Used to send PTP time messages between the slave and master clocks.
   overriding procedure Ptp_Time_Message_Receive_T_Recv_Async (Self : in out Instance; Arg : in Ptp_Time_Message_Receive.T) is
      use Ptp_Enums.Ptp_Message_Type;
      -- Break out parts of the input arguments for ease of access:
      Receive_Time : Sys_Time.T := Arg.Receive_Time;
      Message : Ptp_Time_Message.T renames Arg.Message;
   begin
      -- Get a timestamp if one has not been provided in the input argument. If a timestamp is provided
      -- as part of the connector call, this is a likely a more accurate representation of the message
      -- receive time, so we use that instead. Some systems do not have this capability, and in that
      -- case the best timestamp we can get is to timestamp the message right here as we receive the
      -- message.
      if Receive_Time.Subseconds = 0 and then Receive_Time.Seconds = 0 then
         Receive_Time := Self.Sys_Time_T_Get;
      end if;

      -- Let's make sure this is a valid message. We are only expected to receive messages from slaves
      -- requesting a Delay_Response message. Anything else is unexpected and will be reported as an
      -- event and counted.
      case Message.Message_Type is
         -- This is the message type we are expected:
         when Delay_Request =>
            -- Check the sequence count to make sure it makes sense:
            if Message.Transaction_Count = Self.Transaction_Count then
               -- We are good, send out a Delay_Response message:
               Self.Ptp_Time_Message_T_Send ((Message_Type => Delay_Response, Transaction_Count => Self.Transaction_Count, Time_Stamp => Receive_Time));

               -- Update data products:
               Self.Delay_Request_Message_Count := @ + 1;
               Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Delay_Request_Messages_Received (Receive_Time, (Value => Self.Delay_Request_Message_Count)));
            else
               -- This is an old or out of order transaction. Report it and do not send a delay response.
               Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Transaction_Count (Receive_Time, (Message => Message, Expected_Transaction_Count => Self.Transaction_Count)));

               -- Update data product:
               Self.Unexpected_Message_Count := @ + 1;
               Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Unexpected_Messages_Received (Receive_Time, (Value => Self.Unexpected_Message_Count)));
            end if;
            -- These should only be sent by the master, thus we should not be getting any of these.
         when Sync | Follow_Up | Delay_Response =>
            -- Send error event:
            Self.Event_T_Send_If_Connected (Self.Events.Unexpected_Message_Type (Receive_Time, Message));

            -- Update data product:
            Self.Unexpected_Message_Count := @ + 1;
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Unexpected_Messages_Received (Receive_Time, (Value => Self.Unexpected_Message_Count)));
      end case;
   end Ptp_Time_Message_Receive_T_Recv_Async;

   -- Receives and forwards the PTP time for a follow-up message (if connected). The time received is an accurate time stamp of when the Sync message was sent. This time can be provided to this component by a lower level component which actually records the time the Sync message leaves the system.
   overriding procedure Follow_Up_Sys_Time_T_Recv_Async (Self : in out Instance; Arg : in Sys_Time.T) is
      use Ptp_Enums.Ptp_Message_Type;
   begin
      -- Send out follow up message with updated time:
      Self.Ptp_Time_Message_T_Send ((Message_Type => Follow_Up, Transaction_Count => Self.Transaction_Count, Time_Stamp => Arg));

      -- Update data products:
      Self.Follow_Up_Message_Count := @ + 1;
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Follow_Up_Messages_Sent (Arg, (Value => Self.Follow_Up_Message_Count)));
   end Follow_Up_Sys_Time_T_Recv_Async;

   procedure Queue_Overflow_Event (Self : in out Instance) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Queue_Overflowed (Self.Sys_Time_T_Get));
   end Queue_Overflow_Event;

   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      Self.Queue_Overflow_Event;
   end Tick_T_Recv_Async_Dropped;

   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      Self.Queue_Overflow_Event;
   end Command_T_Recv_Async_Dropped;

   -- This procedure is called when a Ptp_Time_Message_Receive_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ptp_Time_Message_Receive_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ptp_Time_Message_Receive.T) is
   begin
      Self.Queue_Overflow_Event;
   end Ptp_Time_Message_Receive_T_Recv_Async_Dropped;

   -- This procedure is called when a Follow_Up_Sys_Time_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Follow_Up_Sys_Time_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Sys_Time.T) is
      Ignore : Sys_Time.T renames Arg;
   begin
      Self.Queue_Overflow_Event;
   end Follow_Up_Sys_Time_T_Recv_Async_Dropped;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Precision Time Protocol Master component.
   -- This enables the sending of PTP messages.
   overriding function Enable_Precision_Time_Protocol (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Ptp_State;
      Time_Stamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new state:
      Self.State := Enabled;

      -- Reset cycle count:
      Self.Cycle_Count := 0;

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Ptp_Enabled (Time_Stamp));

      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Precision_Time_Protocol_State (Time_Stamp, (State => Self.State)));

      return Success;
   end Enable_Precision_Time_Protocol;

   -- This enables the sending of PTP messages.
   overriding function Disable_Precision_Time_Protocol (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Ptp_State;
      Time_Stamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new state:
      Self.State := Disabled;

      -- Reset cycle count:
      Self.Cycle_Count := 0;

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Ptp_Disabled (Time_Stamp));

      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Precision_Time_Protocol_State (Time_Stamp, (State => Self.State)));

      return Success;
   end Disable_Precision_Time_Protocol;

   -- This sends a PTP sync message at the next tick, regardless of the current sync period. This is useful during testing to send a sync one time.
   overriding function Sync_Once (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Set new sync period:
      Self.Sync_Once := True;

      -- Send info event:
      Self.Event_T_Send_If_Connected (Self.Events.Syncing_Once (Self.Sys_Time_T_Get));
      return Success;
   end Sync_Once;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Precision_Time_Protocol_Master.Implementation;
