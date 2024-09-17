--------------------------------------------------------------------------------
-- Fault_Correction Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Fault_Correction.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Fault_Received_History.Init (Depth => 100);
      Self.Fault_Response_Sent_History.Init (Depth => 100);
      Self.Fault_Response_Cleared_History.Init (Depth => 100);
      Self.Fault_Response_Disabled_History.Init (Depth => 100);
      Self.Fault_Response_Enabled_History.Init (Depth => 100);
      Self.All_Fault_Responses_Cleared_History.Init (Depth => 100);
      Self.Unrecognized_Fault_Id_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Command_Dropped_History.Init (Depth => 100);
      Self.Fault_Dropped_History.Init (Depth => 100);
      Self.Data_Products_Reset_History.Init (Depth => 100);
      -- Data product histories:
      Self.Fault_Counter_History.Init (Depth => 100);
      Self.Last_Fault_Id_Received_History.Init (Depth => 100);
      Self.Time_Of_Last_Fault_Received_History.Init (Depth => 100);
      Self.Fault_Response_Statuses_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Command_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Fault_Received_History.Destroy;
      Self.Fault_Response_Sent_History.Destroy;
      Self.Fault_Response_Cleared_History.Destroy;
      Self.Fault_Response_Disabled_History.Destroy;
      Self.Fault_Response_Enabled_History.Destroy;
      Self.All_Fault_Responses_Cleared_History.Destroy;
      Self.Unrecognized_Fault_Id_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Command_Dropped_History.Destroy;
      Self.Fault_Dropped_History.Destroy;
      Self.Data_Products_Reset_History.Destroy;
      -- Data product histories:
      Self.Fault_Counter_History.Destroy;
      Self.Last_Fault_Id_Received_History.Destroy;
      Self.Time_Of_Last_Fault_Received_History.Destroy;
      Self.Fault_Response_Statuses_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Fault_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Fault_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- The command send connector, for sending correction commands for faults.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_T_Recv_Sync_History.Push (Arg);
   end Command_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

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

   -- This procedure is called when a Fault_T_Send message is dropped due to a full queue.
   overriding procedure Fault_T_Send_Dropped (Self : in out Instance; Arg : in Fault.T) is
      Ignore : Fault.T renames Arg;
   begin
      if not Self.Expect_Fault_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Fault_T_Send was called!");
      else
         Self.Fault_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Fault_T_Send_Dropped := False;
      end if;
   end Fault_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Fault Correction component.
   -- A fault was received.
   overriding procedure Fault_Received (Self : in out Instance; Arg : in Fault_Static.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Received_History.Push (Arg);
   end Fault_Received;

   -- A fault response was sent with the included command header.
   overriding procedure Fault_Response_Sent (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Response_Sent_History.Push (Arg);
   end Fault_Response_Sent;

   -- A fault response was cleared.
   overriding procedure Fault_Response_Cleared (Self : in out Instance; Arg : in Packed_Fault_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Response_Cleared_History.Push (Arg);
   end Fault_Response_Cleared;

   -- A fault response has been disabled
   overriding procedure Fault_Response_Disabled (Self : in out Instance; Arg : in Packed_Fault_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Response_Disabled_History.Push (Arg);
   end Fault_Response_Disabled;

   -- A fault response has been enabled.
   overriding procedure Fault_Response_Enabled (Self : in out Instance; Arg : in Packed_Fault_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Response_Enabled_History.Push (Arg);
   end Fault_Response_Enabled;

   -- Any latched faults have been unlatched by command.
   overriding procedure All_Fault_Responses_Cleared (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.All_Fault_Responses_Cleared_History.Push (Arg);
   end All_Fault_Responses_Cleared;

   -- A fault response entry with the included fault ID was not found in the table.
   overriding procedure Unrecognized_Fault_Id (Self : in out Instance; Arg : in Packed_Fault_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unrecognized_Fault_Id_History.Push (Arg);
   end Unrecognized_Fault_Id;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Dropped_History.Push (Arg);
   end Command_Dropped;

   -- A fault was dropped due to a full queue.
   overriding procedure Fault_Dropped (Self : in out Instance; Arg : in Fault_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Dropped_History.Push (Arg);
   end Fault_Dropped;

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
   --    Data products for the Fault Correction component.
   -- The number of faults received by the component.
   overriding procedure Fault_Counter (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Counter_History.Push (Arg);
   end Fault_Counter;

   -- The ID of the last fault received.
   overriding procedure Last_Fault_Id_Received (Self : in out Instance; Arg : in Packed_Fault_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Fault_Id_Received_History.Push (Arg);
   end Last_Fault_Id_Received;

   -- The system time of the last fault received.
   overriding procedure Time_Of_Last_Fault_Received (Self : in out Instance; Arg : in Sys_Time.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Time_Of_Last_Fault_Received_History.Push (Arg);
   end Time_Of_Last_Fault_Received;

   -- 2-bits of status for each fault response that this component is managing. Note that Packed_U32.T is just a placeholder type for this data product. The actual type of this data product will be autocoded and at assembly model ingest time.
   overriding procedure Fault_Response_Statuses (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_Response_Statuses_History.Push (Arg);
   end Fault_Response_Statuses;

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

end Component.Fault_Correction.Implementation.Tester;
