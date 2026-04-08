--------------------------------------------------------------------------------
-- Ccsds_Parameter_Table_Router Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Parameter_Table_Router_Reciprocal;
with Printable_History;
with Parameters_Memory_Region.Representation;
with Command_Response.Representation;
with Event.Representation;
with Data_Product.Representation;
with Sys_Time.Representation;
with Event;
with Parameter_Table_Id.Representation;
with Parameter_Table_Operation_Failure_Info.Representation;
with Parameter_Table_Timeout_Info.Representation;
with Ccsds_Primary_Header.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;
with Data_Product;
with Packed_U32.Representation;
with Parameter_Table_Received_Info.Representation;

-- This component receives segmented CCSDS packets containing parameter tables,
-- reassembles them in a staging buffer, and routes completed tables to downstream
-- components that accept a Parameters_Memory_Region type. It supports loading
-- parameter tables from a designated component on command, which will often be a
-- Parameter_Store sitting in front of persistent storage.
-- A generator exists to produce the routing table that maps parameter table IDs
-- to output connector indexes. See the gen/ subdirectory for documentation.
package Component.Ccsds_Parameter_Table_Router.Implementation.Tester is

   use Component.Ccsds_Parameter_Table_Router_Reciprocal;
   -- Invoker connector history packages:
   package Parameters_Memory_Region_T_Recv_Sync_History_Package is new Printable_History (Parameters_Memory_Region.T, Parameters_Memory_Region.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Receiving_New_Table_History_Package is new Printable_History (Parameter_Table_Id.T, Parameter_Table_Id.Representation.Image);
   package Table_Received_History_Package is new Printable_History (Parameter_Table_Id.T, Parameter_Table_Id.Representation.Image);
   package Table_Updated_History_Package is new Printable_History (Parameter_Table_Id.T, Parameter_Table_Id.Representation.Image);
   package Loading_Table_History_Package is new Printable_History (Parameter_Table_Id.T, Parameter_Table_Id.Representation.Image);
   package Table_Loaded_History_Package is new Printable_History (Parameter_Table_Id.T, Parameter_Table_Id.Representation.Image);
   package Table_Update_Failure_History_Package is new Printable_History (Parameter_Table_Operation_Failure_Info.T, Parameter_Table_Operation_Failure_Info.Representation.Image);
   package Table_Update_Timeout_History_Package is new Printable_History (Parameter_Table_Timeout_Info.T, Parameter_Table_Timeout_Info.Representation.Image);
   package Table_Load_Failure_History_Package is new Printable_History (Parameter_Table_Operation_Failure_Info.T, Parameter_Table_Operation_Failure_Info.Representation.Image);
   package No_Load_Source_History_Package is new Printable_History (Parameter_Table_Id.T, Parameter_Table_Id.Representation.Image);
   package Unrecognized_Table_Id_History_Package is new Printable_History (Parameter_Table_Id.T, Parameter_Table_Id.Representation.Image);
   package Packet_Ignored_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Too_Small_Table_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Staging_Buffer_Overflow_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Packet_Dropped_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Loading_All_Parameter_Tables_History_Package is new Printable_History (Natural, Natural'Image);
   package All_Parameter_Tables_Loaded_History_Package is new Printable_History (Natural, Natural'Image);

   -- Data product history packages:
   package Num_Packets_Received_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Num_Packets_Rejected_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Num_Tables_Updated_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Num_Tables_Invalid_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Last_Table_Received_History_Package is new Printable_History (Parameter_Table_Received_Info.T, Parameter_Table_Received_Info.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Ccsds_Parameter_Table_Router_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Parameter_Table_Router.Implementation.Instance;
      -- Connector histories:
      Parameters_Memory_Region_T_Recv_Sync_History : Parameters_Memory_Region_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Receiving_New_Table_History : Receiving_New_Table_History_Package.Instance;
      Table_Received_History : Table_Received_History_Package.Instance;
      Table_Updated_History : Table_Updated_History_Package.Instance;
      Loading_Table_History : Loading_Table_History_Package.Instance;
      Table_Loaded_History : Table_Loaded_History_Package.Instance;
      Table_Update_Failure_History : Table_Update_Failure_History_Package.Instance;
      Table_Update_Timeout_History : Table_Update_Timeout_History_Package.Instance;
      Table_Load_Failure_History : Table_Load_Failure_History_Package.Instance;
      No_Load_Source_History : No_Load_Source_History_Package.Instance;
      Unrecognized_Table_Id_History : Unrecognized_Table_Id_History_Package.Instance;
      Packet_Ignored_History : Packet_Ignored_History_Package.Instance;
      Too_Small_Table_History : Too_Small_Table_History_Package.Instance;
      Staging_Buffer_Overflow_History : Staging_Buffer_Overflow_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Command_Dropped_History : Command_Dropped_History_Package.Instance;
      Packet_Dropped_History : Packet_Dropped_History_Package.Instance;
      Loading_All_Parameter_Tables_History : Loading_All_Parameter_Tables_History_Package.Instance;
      All_Parameter_Tables_Loaded_History : All_Parameter_Tables_Loaded_History_Package.Instance;
      -- Data product histories:
      Num_Packets_Received_History : Num_Packets_Received_History_Package.Instance;
      Num_Packets_Rejected_History : Num_Packets_Rejected_History_Package.Instance;
      Num_Tables_Updated_History : Num_Tables_Updated_History_Package.Instance;
      Num_Tables_Invalid_History : Num_Tables_Invalid_History_Package.Instance;
      Last_Table_Received_History : Last_Table_Received_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Ccsds_Space_Packet_T_Send_Dropped : Boolean := False;
      Ccsds_Space_Packet_T_Send_Dropped_Count : Natural := 0;
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Parameters_Memory_Region_T_Send_Count : in Connector_Count_Type; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Arrayed output connector for sending parameter table memory regions to
   -- downstream components.
   overriding procedure Parameters_Memory_Region_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region.T);
   -- Send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Parameter Table Router component.
   -- A new parameter table FirstSegment has been received and buffering has started.
   overriding procedure Receiving_New_Table (Self : in out Instance; Arg : in Parameter_Table_Id.T);
   -- A complete parameter table has been reassembled from CCSDS segments.
   overriding procedure Table_Received (Self : in out Instance; Arg : in Parameter_Table_Id.T);
   -- A parameter table has been successfully sent to all downstream destinations.
   overriding procedure Table_Updated (Self : in out Instance; Arg : in Parameter_Table_Id.T);
   -- Starting to load a parameter table from its load_from source.
   overriding procedure Loading_Table (Self : in out Instance; Arg : in Parameter_Table_Id.T);
   -- A parameter table has been successfully loaded from persistent storage and sent
   -- to all destinations.
   overriding procedure Table_Loaded (Self : in out Instance; Arg : in Parameter_Table_Id.T);
   -- A downstream component rejected a parameter table update. Includes the table
   -- ID, connector index, and release status.
   overriding procedure Table_Update_Failure (Self : in out Instance; Arg : in Parameter_Table_Operation_Failure_Info.T);
   -- Timed out waiting for a response from a downstream component during table
   -- update.
   overriding procedure Table_Update_Timeout (Self : in out Instance; Arg : in Parameter_Table_Timeout_Info.T);
   -- Failed to load a parameter table from persistent storage. Includes the table
   -- ID, connector index, and release status.
   overriding procedure Table_Load_Failure (Self : in out Instance; Arg : in Parameter_Table_Operation_Failure_Info.T);
   -- Load_Parameter_Table command received for a table ID that has no load_from
   -- destination configured.
   overriding procedure No_Load_Source (Self : in out Instance; Arg : in Parameter_Table_Id.T);
   -- Received a parameter table with a Table ID not found in the routing table.
   overriding procedure Unrecognized_Table_Id (Self : in out Instance; Arg : in Parameter_Table_Id.T);
   -- A CCSDS packet was ignored (continuation/last segment without prior first
   -- segment, or unsegmented).
   overriding procedure Packet_Ignored (Self : in out Instance; Arg : in Ccsds_Primary_Header.T);
   -- A FirstSegment packet was too small to contain a Table ID (less than 2 bytes of
   -- data).
   overriding procedure Too_Small_Table (Self : in out Instance; Arg : in Ccsds_Primary_Header.T);
   -- The staging buffer is full. Dropping packets until a new FirstSegment resets
   -- the buffer.
   overriding procedure Staging_Buffer_Overflow (Self : in out Instance; Arg : in Ccsds_Primary_Header.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A CCSDS packet was dropped due to a full queue.
   overriding procedure Packet_Dropped (Self : in out Instance; Arg : in Ccsds_Primary_Header.T);
   -- Starting to load all parameter tables that have a load_from source.
   overriding procedure Loading_All_Parameter_Tables (Self : in out Instance);
   -- Finished loading all parameter tables.
   overriding procedure All_Parameter_Tables_Loaded (Self : in out Instance);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Parameter Table Router component.
   -- Total number of CCSDS packets received on the async connector.
   overriding procedure Num_Packets_Received (Self : in out Instance; Arg : in Packed_U32.T);
   -- Number of packets rejected (ignored, too small, or buffer overflow).
   overriding procedure Num_Packets_Rejected (Self : in out Instance; Arg : in Packed_U32.T);
   -- Number of parameter tables successfully distributed to all destinations.
   overriding procedure Num_Tables_Updated (Self : in out Instance; Arg : in Packed_U32.T);
   -- Number of parameter tables that failed distribution, had unrecognized IDs, or
   -- timed out.
   overriding procedure Num_Tables_Invalid (Self : in out Instance; Arg : in Packed_U32.T);
   -- Information about the last complete parameter table received.
   overriding procedure Last_Table_Received (Self : in out Instance; Arg : in Parameter_Table_Received_Info.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Ccsds_Parameter_Table_Router.Implementation.Tester;
