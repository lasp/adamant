--------------------------------------------------------------------------------
-- Command_Router Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Command_Router.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural; Command_T_Send_Count : in Connector_Count_Type; Command_Response_T_To_Forward_Send_Count : in Connector_Count_Type) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size, Command_T_Send_Count => Command_T_Send_Count, Command_Response_T_To_Forward_Send_Count => Command_Response_T_To_Forward_Send_Count);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_To_Forward_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Command_Received_History.Init (Depth => 100);
      Self.Command_Execution_Successful_History.Init (Depth => 100);
      Self.Command_Execution_Failure_History.Init (Depth => 100);
      Self.Command_Id_Not_Registered_History.Init (Depth => 100);
      Self.Registration_Id_Conflict_History.Init (Depth => 100);
      Self.Router_Table_Full_History.Init (Depth => 100);
      Self.Outgoing_Command_Dropped_History.Init (Depth => 100);
      Self.Incoming_Command_Dropped_History.Init (Depth => 100);
      Self.Noop_Command_Dropped_History.Init (Depth => 100);
      Self.Command_Response_Dropped_History.Init (Depth => 100);
      Self.Noop_Received_History.Init (Depth => 100);
      Self.Noop_Arg_Received_History.Init (Depth => 100);
      Self.Noop_Response_Received_History.Init (Depth => 100);
      Self.Noop_Response_Forwarding_Success_History.Init (Depth => 100);
      Self.Forwarded_Command_Response_Dropped_History.Init (Depth => 100);
      Self.Invalid_Command_Source_Id_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Data_Products_Reset_History.Init (Depth => 100);
      -- Data product histories:
      Self.Command_Receive_Count_History.Init (Depth => 100);
      Self.Command_Success_Count_History.Init (Depth => 100);
      Self.Command_Failure_Count_History.Init (Depth => 100);
      Self.Last_Received_Command_History.Init (Depth => 100);
      Self.Last_Successful_Command_History.Init (Depth => 100);
      Self.Last_Failed_Command_History.Init (Depth => 100);
      Self.Noop_Arg_Last_Value_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_To_Forward_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Command_Received_History.Destroy;
      Self.Command_Execution_Successful_History.Destroy;
      Self.Command_Execution_Failure_History.Destroy;
      Self.Command_Id_Not_Registered_History.Destroy;
      Self.Registration_Id_Conflict_History.Destroy;
      Self.Router_Table_Full_History.Destroy;
      Self.Outgoing_Command_Dropped_History.Destroy;
      Self.Incoming_Command_Dropped_History.Destroy;
      Self.Noop_Command_Dropped_History.Destroy;
      Self.Command_Response_Dropped_History.Destroy;
      Self.Noop_Received_History.Destroy;
      Self.Noop_Arg_Received_History.Destroy;
      Self.Noop_Response_Received_History.Destroy;
      Self.Noop_Response_Forwarding_Success_History.Destroy;
      Self.Forwarded_Command_Response_Dropped_History.Destroy;
      Self.Invalid_Command_Source_Id_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Data_Products_Reset_History.Destroy;
      -- Data product histories:
      Self.Command_Receive_Count_History.Destroy;
      Self.Command_Success_Count_History.Destroy;
      Self.Command_Failure_Count_History.Destroy;
      Self.Last_Received_Command_History.Destroy;
      Self.Last_Successful_Command_History.Destroy;
      Self.Last_Failed_Command_History.Destroy;
      Self.Noop_Arg_Last_Value_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      ---------------------------------------------------------------
      -- Modified the default connections to better test the router.
      ---------------------------------------------------------------
      -- Self.Component_Instance.Attach_Command_T_Send (1, Self'Unchecked_Access, Self.Command_T_Recv_Sync_Access);
      -- Attach the first index to the Command_Router instead
      Self.Component_Instance.Attach_Command_T_Send (1, Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Component_Instance.Attach_Command_T_Send (2, Self'Unchecked_Access, Self.Command_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_T_Send (3, Self'Unchecked_Access, Self.Command_T_Recv_Sync_Access);
      -- Self.Component_Instance.Attach_Command_Response_T_To_Forward_Send (1, Self'Unchecked_Access, Self.Command_Response_T_To_Forward_Recv_Sync_Access);
      -- Attach the first index to the Command_Response_Forwarding to the command router instead
      Self.Component_Instance.Attach_Command_Response_T_To_Forward_Send (1, Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_Response_T_Recv_Async_Access);
      Self.Component_Instance.Attach_Command_Response_T_To_Forward_Send (2, Self'Unchecked_Access, Self.Command_Response_T_To_Forward_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_To_Forward_Send (3, Self'Unchecked_Access, Self.Command_Response_T_To_Forward_Recv_Sync_Access);
      -- Self.Component_Instance.Attach_Command_Response_T_Send (Self'Unchecked_Access, Self.Command_Response_T_Recv_Sync_Access);
      -- Attach the response connector to the Command Router instead:
      Self.Component_Instance.Attach_Command_Response_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_Response_T_Recv_Async_Access);
      Self.Component_Instance.Attach_Event_T_Send (Self'Unchecked_Access, Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (Self'Unchecked_Access, Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Attach_Command_T_To_Route_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_To_Route_Recv_Async_Access);
      Self.Attach_Command_T_To_Route_Send_2 (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_To_Route_Recv_Sync_Access);
      -- Self.Attach_Command_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_Recv_Async_Access);
      -- We will send these commands through the To_Route connector instead.
      Self.Attach_Command_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Command_Response_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_Response_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector has an unconstrained size that is determined by the assembly in which the Command Router is instantiated. Each index of the connector should connect to different destination component that receives commands. The Command Router will route commands destined for each component on the appropriate index of this connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_T_Recv_Sync_History.Push (Arg);
   end Command_T_Recv_Sync;

   -- Command responses received from command executing components are forwarded back to their command sources using this arrayed connector. One index of this connector can be connected in loopback to the Command_Response_T_Recv_Async connector in order to command forwarding self test capabilities (see the Noop_Response command).
   overriding procedure Command_Response_T_To_Forward_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_To_Forward_Recv_Sync_History.Push (Arg);
   end Command_Response_T_To_Forward_Recv_Sync;

   -- This connector is used to register the Command Router's NOOP commands at initialization, and respond to NOOP commands during execution. It is usually connected in loopback to the Command_Response_T_Recv_Async connector.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_To_Route_Send message is dropped due to a full queue.
   overriding procedure Command_T_To_Route_Send_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      if not Self.Expect_Command_T_To_Route_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_T_To_Route_Send was called!");
      else
         Self.Command_T_To_Route_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_T_To_Route_Send_Dropped := False;
      end if;
   end Command_T_To_Route_Send_Dropped;

   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
      Ignore : Command_Response.T renames Arg;
   begin
      if not Self.Expect_Command_Response_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_Response_T_Send was called!");
      else
         Self.Command_Response_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_Response_T_Send_Dropped := False;
      end if;
   end Command_Response_T_Send_Dropped;

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

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received by the command router to be routed.
   overriding procedure Command_Received (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Received_History.Push (Arg);
   end Command_Received;

   -- A command was routed, executed, and returned a response saying it was executed successfully
   overriding procedure Command_Execution_Successful (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Execution_Successful_History.Push (Arg);
   end Command_Execution_Successful;

   -- A command execution failed.
   overriding procedure Command_Execution_Failure (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Execution_Failure_History.Push (Arg);
   end Command_Execution_Failure;

   -- A command was sent to the router, but it was not found in the router table.
   overriding procedure Command_Id_Not_Registered (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Id_Not_Registered_History.Push (Arg);
   end Command_Id_Not_Registered;

   -- The command Id has already been registered.
   overriding procedure Registration_Id_Conflict (Self : in out Instance; Arg : in Command_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Registration_Id_Conflict_History.Push (Arg);
   end Registration_Id_Conflict;

   -- Cannot add command Id to router table because it is full.
   overriding procedure Router_Table_Full (Self : in out Instance; Arg : in Command_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Router_Table_Full_History.Push (Arg);
   end Router_Table_Full;

   -- A command was dropped because the recipient's queue was full.
   overriding procedure Outgoing_Command_Dropped (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Outgoing_Command_Dropped_History.Push (Arg);
   end Outgoing_Command_Dropped;

   -- A command was dropped because the command router's queue was full.
   overriding procedure Incoming_Command_Dropped (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Incoming_Command_Dropped_History.Push (Arg);
   end Incoming_Command_Dropped;

   -- A noop command was dropped because the command router's queue was full.
   overriding procedure Noop_Command_Dropped (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Noop_Command_Dropped_History.Push (Arg);
   end Noop_Command_Dropped;

   -- A command response was dropped because the command router's queue was full.
   overriding procedure Command_Response_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_Dropped_History.Push (Arg);
   end Command_Response_Dropped;

   -- A Noop command was received.
   overriding procedure Noop_Received (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Noop_Received_History.Push (Arg);
   end Noop_Received;

   -- A Noop command was received with an argument.
   overriding procedure Noop_Arg_Received (Self : in out Instance; Arg : in Command_Router_Arg.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Noop_Arg_Received_History.Push (Arg);
   end Noop_Arg_Received;

   -- A noop response self test command was received.
   overriding procedure Noop_Response_Received (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Noop_Response_Received_History.Push (Arg);
   end Noop_Response_Received;

   -- If this event is sent then the noop response self test command succeeded.
   overriding procedure Noop_Response_Forwarding_Success (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Noop_Response_Forwarding_Success_History.Push (Arg);
   end Noop_Response_Forwarding_Success;

   -- A forwarded command response was dropped because the receiving component's queue overflowed.
   overriding procedure Forwarded_Command_Response_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Forwarded_Command_Response_Dropped_History.Push (Arg);
   end Forwarded_Command_Response_Dropped;

   -- A command response contained an invalid source id. This is a software bug and should be corrected.
   overriding procedure Invalid_Command_Source_Id (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Source_Id_History.Push (Arg);
   end Invalid_Command_Source_Id;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- The component's data products have been reset to initialization values.
   overriding procedure Data_Products_Reset (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Products_Reset_History.Push (Arg);
   end Data_Products_Reset;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Command Router component.
   -- The number of commands received by the component.
   overriding procedure Command_Receive_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Receive_Count_History.Push (Arg);
   end Command_Receive_Count;

   -- The number of commands that successfully executed.
   overriding procedure Command_Success_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Success_Count_History.Push (Arg);
   end Command_Success_Count;

   -- The number of commands that failed to execute.
   overriding procedure Command_Failure_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Failure_Count_History.Push (Arg);
   end Command_Failure_Count;

   -- The ID of the last received command by the command router.
   overriding procedure Last_Received_Command (Self : in out Instance; Arg : in Command_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Received_Command_History.Push (Arg);
   end Last_Received_Command;

   -- The ID of the last successful command routed by the command router.
   overriding procedure Last_Successful_Command (Self : in out Instance; Arg : in Command_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Successful_Command_History.Push (Arg);
   end Last_Successful_Command;

   -- The ID and status of the last failed command routed by the command router.
   overriding procedure Last_Failed_Command (Self : in out Instance; Arg : in Command_Id_Status.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Failed_Command_History.Push (Arg);
   end Last_Failed_Command;

   -- The last value sent with the Noop_Arg command. This data product can be useful for testing purposes.
   overriding procedure Noop_Arg_Last_Value (Self : in out Instance; Arg : in Command_Router_Arg.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Noop_Arg_Last_Value_History.Push (Arg);
   end Noop_Arg_Last_Value;

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

end Component.Command_Router.Implementation.Tester;
