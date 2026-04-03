--------------------------------------------------------------------------------
-- Ccsds_Parameter_Table_Router Component Tester Body
--------------------------------------------------------------------------------

-- Includes:
with String_Util;

package body Component.Ccsds_Parameter_Table_Router.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Parameters_Memory_Region_T_Send_Count : in Connector_Count_Type; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Parameters_Memory_Region_T_Send_Count => Parameters_Memory_Region_T_Send_Count, Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Parameters_Memory_Region_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Receiving_New_Table_History.Init (Depth => 100);
      Self.Table_Received_History.Init (Depth => 100);
      Self.Table_Updated_History.Init (Depth => 100);
      Self.Loading_Table_History.Init (Depth => 100);
      Self.Table_Loaded_History.Init (Depth => 100);
      Self.Table_Update_Failure_History.Init (Depth => 100);
      Self.Table_Update_Timeout_History.Init (Depth => 100);
      Self.Table_Load_Failure_History.Init (Depth => 100);
      Self.No_Load_Source_History.Init (Depth => 100);
      Self.Unrecognized_Table_Id_History.Init (Depth => 100);
      Self.Packet_Ignored_History.Init (Depth => 100);
      Self.Too_Small_Table_History.Init (Depth => 100);
      Self.Staging_Buffer_Overflow_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Command_Dropped_History.Init (Depth => 100);
      Self.Packet_Dropped_History.Init (Depth => 100);
      Self.Loading_All_Parameter_Tables_History.Init (Depth => 100);
      Self.All_Parameter_Tables_Loaded_History.Init (Depth => 100);
      -- Data product histories:
      Self.Num_Packets_Received_History.Init (Depth => 100);
      Self.Num_Packets_Rejected_History.Init (Depth => 100);
      Self.Num_Tables_Updated_History.Init (Depth => 100);
      Self.Num_Tables_Invalid_History.Init (Depth => 100);
      Self.Last_Table_Received_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Parameters_Memory_Region_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Receiving_New_Table_History.Destroy;
      Self.Table_Received_History.Destroy;
      Self.Table_Updated_History.Destroy;
      Self.Loading_Table_History.Destroy;
      Self.Table_Loaded_History.Destroy;
      Self.Table_Update_Failure_History.Destroy;
      Self.Table_Update_Timeout_History.Destroy;
      Self.Table_Load_Failure_History.Destroy;
      Self.No_Load_Source_History.Destroy;
      Self.Unrecognized_Table_Id_History.Destroy;
      Self.Packet_Ignored_History.Destroy;
      Self.Too_Small_Table_History.Destroy;
      Self.Staging_Buffer_Overflow_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Command_Dropped_History.Destroy;
      Self.Packet_Dropped_History.Destroy;
      Self.Loading_All_Parameter_Tables_History.Destroy;
      Self.All_Parameter_Tables_Loaded_History.Destroy;
      -- Data product histories:
      Self.Num_Packets_Received_History.Destroy;
      Self.Num_Packets_Rejected_History.Destroy;
      Self.Num_Tables_Updated_History.Destroy;
      Self.Num_Tables_Invalid_History.Destroy;
      Self.Last_Table_Received_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Parameters_Memory_Region_T_Send (From_Index => 1, To_Component => Self'Unchecked_Access, Hook => Self.Parameters_Memory_Region_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Parameters_Memory_Region_T_Send (From_Index => 2, To_Component => Self'Unchecked_Access, Hook => Self.Parameters_Memory_Region_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Parameters_Memory_Region_T_Send (From_Index => 3, To_Component => Self'Unchecked_Access, Hook => Self.Parameters_Memory_Region_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Ccsds_Space_Packet_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Ccsds_Space_Packet_T_Recv_Async_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Timeout_Tick_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Timeout_Tick_Recv_Sync_Access);
      Self.Attach_Parameters_Memory_Region_Release_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Parameters_Memory_Region_Release_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Arrayed output connector for sending parameter table memory regions to
   -- downstream components.
   overriding procedure Parameters_Memory_Region_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameters_Memory_Region_T_Recv_Sync_History.Push (Arg);
   end Parameters_Memory_Region_T_Recv_Sync;

   -- Send command responses.
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
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      Ignore : Ccsds_Space_Packet.T renames Arg;
   begin
      if not Self.Expect_Ccsds_Space_Packet_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Ccsds_Space_Packet_T_Send was called!");
      else
         Self.Ccsds_Space_Packet_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Ccsds_Space_Packet_T_Send_Dropped := False;
      end if;
   end Ccsds_Space_Packet_T_Send_Dropped;

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
   -- Description:
   --    Events for the Parameter Table Router component.
   -- A new parameter table FirstSegment has been received and buffering has started.
   overriding procedure Receiving_New_Table (Self : in out Instance; Arg : in Parameter_Table_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Receiving_New_Table_History.Push (Arg);
   end Receiving_New_Table;

   -- A complete parameter table has been reassembled from CCSDS segments.
   overriding procedure Table_Received (Self : in out Instance; Arg : in Parameter_Table_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Received_History.Push (Arg);
   end Table_Received;

   -- A parameter table has been successfully sent to all downstream destinations.
   overriding procedure Table_Updated (Self : in out Instance; Arg : in Parameter_Table_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Updated_History.Push (Arg);
   end Table_Updated;

   -- Starting to load a parameter table from its load_from source.
   overriding procedure Loading_Table (Self : in out Instance; Arg : in Parameter_Table_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Loading_Table_History.Push (Arg);
   end Loading_Table;

   -- A parameter table has been successfully loaded from persistent storage and sent
   -- to all destinations.
   overriding procedure Table_Loaded (Self : in out Instance; Arg : in Parameter_Table_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Loaded_History.Push (Arg);
   end Table_Loaded;

   -- A downstream component rejected a parameter table update. Includes the table
   -- ID, connector index, and release status.
   overriding procedure Table_Update_Failure (Self : in out Instance; Arg : in Parameter_Table_Operation_Failure_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Update_Failure_History.Push (Arg);
   end Table_Update_Failure;

   -- Timed out waiting for a response from a downstream component during table
   -- update.
   overriding procedure Table_Update_Timeout (Self : in out Instance; Arg : in Parameter_Table_Timeout_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Update_Timeout_History.Push (Arg);
   end Table_Update_Timeout;

   -- Failed to load a parameter table from persistent storage. Includes the table
   -- ID, connector index, and release status.
   overriding procedure Table_Load_Failure (Self : in out Instance; Arg : in Parameter_Table_Operation_Failure_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Load_Failure_History.Push (Arg);
   end Table_Load_Failure;

   -- Load_Parameter_Table command received for a table ID that has no load_from
   -- destination configured.
   overriding procedure No_Load_Source (Self : in out Instance; Arg : in Parameter_Table_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.No_Load_Source_History.Push (Arg);
   end No_Load_Source;

   -- Received a parameter table with a Table ID not found in the routing table.
   overriding procedure Unrecognized_Table_Id (Self : in out Instance; Arg : in Parameter_Table_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unrecognized_Table_Id_History.Push (Arg);
   end Unrecognized_Table_Id;

   -- A CCSDS packet was ignored (continuation/last segment without prior first
   -- segment, or unsegmented).
   overriding procedure Packet_Ignored (Self : in out Instance; Arg : in Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Ignored_History.Push (Arg);
   end Packet_Ignored;

   -- A FirstSegment packet was too small to contain a Table ID (less than 2 bytes of
   -- data).
   overriding procedure Too_Small_Table (Self : in out Instance; Arg : in Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Too_Small_Table_History.Push (Arg);
   end Too_Small_Table;

   -- The staging buffer is full. Dropping packets until a new FirstSegment resets
   -- the buffer.
   overriding procedure Staging_Buffer_Overflow (Self : in out Instance; Arg : in Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Staging_Buffer_Overflow_History.Push (Arg);
   end Staging_Buffer_Overflow;

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

   -- A CCSDS packet was dropped due to a full queue.
   overriding procedure Packet_Dropped (Self : in out Instance; Arg : in Ccsds_Primary_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_Dropped_History.Push (Arg);
   end Packet_Dropped;

   -- Starting to load all parameter tables that have a load_from source.
   overriding procedure Loading_All_Parameter_Tables (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Loading_All_Parameter_Tables_History.Push (Arg);
   end Loading_All_Parameter_Tables;

   -- Finished loading all parameter tables.
   overriding procedure All_Parameter_Tables_Loaded (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.All_Parameter_Tables_Loaded_History.Push (Arg);
   end All_Parameter_Tables_Loaded;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Parameter Table Router component.
   -- Total number of CCSDS packets received on the async connector.
   overriding procedure Num_Packets_Received (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Num_Packets_Received_History.Push (Arg);
   end Num_Packets_Received;

   -- Number of packets rejected (ignored, too small, or buffer overflow).
   overriding procedure Num_Packets_Rejected (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Num_Packets_Rejected_History.Push (Arg);
   end Num_Packets_Rejected;

   -- Number of parameter tables successfully distributed to all destinations.
   overriding procedure Num_Tables_Updated (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Num_Tables_Updated_History.Push (Arg);
   end Num_Tables_Updated;

   -- Number of parameter tables that failed distribution, had unrecognized IDs, or
   -- timed out.
   overriding procedure Num_Tables_Invalid (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Num_Tables_Invalid_History.Push (Arg);
   end Num_Tables_Invalid;

   -- Information about the last complete parameter table received.
   overriding procedure Last_Table_Received (Self : in out Instance; Arg : in Parameter_Table_Received_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Table_Received_History.Push (Arg);
   end Last_Table_Received;

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

end Component.Ccsds_Parameter_Table_Router.Implementation.Tester;
