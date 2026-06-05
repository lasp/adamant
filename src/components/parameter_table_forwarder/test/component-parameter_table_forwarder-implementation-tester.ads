--------------------------------------------------------------------------------
-- Parameter_Table_Forwarder Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Parameter_Table_Forwarder_Reciprocal;
with Printable_History;
with Parameter_Enums;
with Basic_Types;
with Command_Response.Representation;
with Parameters_Memory_Region_Release.Representation;
with Parameters_Memory_Region.Representation;
with Memory_Packetizer_Types;
with Packet;
with Event.Representation;
with Data_Product.Representation;
with Sys_Time.Representation;
with Data_Product;
with Packed_Table_Operation_Status.Representation;
with Event;
with Invalid_Parameters_Memory_Region_Length.Representation;
with Invalid_Parameters_Memory_Region_Crc.Representation;
with Memory_Region.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;

-- Sits between an upstream parameter table source (Ccsds_Parameter_Table_Router
-- or an upstream Parameter_Store) and a single downstream component that owns
-- its own parameter table layout. Validates incoming memory regions (length and
-- CRC), forwards all parameter table operations (Set, Validate, Get) to the
-- downstream component, and provides standard boilerplate (dump command, active
-- parameters packet, table status data product, common events) so the
-- downstream component only has to deserialize and apply its own packed record.
-- The forwarder owns no parameter table bytes -- it is pure middleware. The
-- downstream component is the authoritative source of the table contents and is
-- queried via the Get operation when a dump is requested.
package Component.Parameter_Table_Forwarder.Implementation.Tester is

   use Component.Parameter_Table_Forwarder_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Parameters_Memory_Region_Release_T_Recv_Sync_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Parameters_Memory_Region_T_Forwarded_Reciprocal_History_Package is new Printable_History (Parameters_Memory_Region.T, Parameters_Memory_Region.Representation.Image);
   package Memory_Dump_Recv_Sync_History_Package is new Printable_History (Memory_Packetizer_Types.Memory_Dump, Memory_Packetizer_Types.Memory_Dump'Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Memory_Region_Length_Mismatch_History_Package is new Printable_History (Invalid_Parameters_Memory_Region_Length.T, Invalid_Parameters_Memory_Region_Length.Representation.Image);
   package Memory_Region_Crc_Invalid_History_Package is new Printable_History (Invalid_Parameters_Memory_Region_Crc.T, Invalid_Parameters_Memory_Region_Crc.Representation.Image);
   package Parameter_Table_Updated_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Parameter_Table_Validated_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Parameter_Table_Fetched_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Downstream_Component_Rejected_Update_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Downstream_Component_Rejected_Validation_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Downstream_Component_Rejected_Fetch_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Get_Pointer_Not_Supported_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Dumped_Parameters_History_Package is new Printable_History (Natural, Natural'Image);
   package Dump_Failed_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Memory_Region_Dropped_History_Package is new Printable_History (Parameters_Memory_Region.T, Parameters_Memory_Region.Representation.Image);

   -- Data product history packages:
   package Table_Status_History_Package is new Printable_History (Packed_Table_Operation_Status.T, Packed_Table_Operation_Status.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Parameter_Table_Forwarder_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Parameter_Table_Forwarder.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Parameters_Memory_Region_Release_T_Recv_Sync_History : Parameters_Memory_Region_Release_T_Recv_Sync_History_Package.Instance;
      Parameters_Memory_Region_T_Forwarded_Reciprocal_History : Parameters_Memory_Region_T_Forwarded_Reciprocal_History_Package.Instance;
      Memory_Dump_Recv_Sync_History : Memory_Dump_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Memory_Region_Length_Mismatch_History : Memory_Region_Length_Mismatch_History_Package.Instance;
      Memory_Region_Crc_Invalid_History : Memory_Region_Crc_Invalid_History_Package.Instance;
      Parameter_Table_Updated_History : Parameter_Table_Updated_History_Package.Instance;
      Parameter_Table_Validated_History : Parameter_Table_Validated_History_Package.Instance;
      Parameter_Table_Fetched_History : Parameter_Table_Fetched_History_Package.Instance;
      Downstream_Component_Rejected_Update_History : Downstream_Component_Rejected_Update_History_Package.Instance;
      Downstream_Component_Rejected_Validation_History : Downstream_Component_Rejected_Validation_History_Package.Instance;
      Downstream_Component_Rejected_Fetch_History : Downstream_Component_Rejected_Fetch_History_Package.Instance;
      Get_Pointer_Not_Supported_History : Get_Pointer_Not_Supported_History_Package.Instance;
      Dumped_Parameters_History : Dumped_Parameters_History_Package.Instance;
      Dump_Failed_History : Dump_Failed_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Command_Dropped_History : Command_Dropped_History_Package.Instance;
      Memory_Region_Dropped_History : Memory_Region_Dropped_History_Package.Instance;
      -- Data product histories:
      Table_Status_History : Table_Status_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Parameters_Memory_Region_T_Send_Dropped : Boolean := False;
      Parameters_Memory_Region_T_Send_Dropped_Count : Natural := 0;
      -- Mock downstream behavior: the recv_sync handler of the forwarded
      -- region uses these per-op fields to decide what status to release back
      -- to the forwarder. Test code sets these before driving stimuli.
      Mock_Set_Release_Status : Parameter_Enums.Parameter_Table_Update_Status.E :=
         Parameter_Enums.Parameter_Table_Update_Status.Success;
      Mock_Validate_Release_Status : Parameter_Enums.Parameter_Table_Update_Status.E :=
         Parameter_Enums.Parameter_Table_Update_Status.Success;
      Mock_Get_Release_Status : Parameter_Enums.Parameter_Table_Update_Status.E :=
         Parameter_Enums.Parameter_Table_Update_Status.Success;
      -- Pattern byte the mock downstream writes into the Get region on success.
      Mock_Get_Fill_Pattern : Basic_Types.Byte := 16#AA#;
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
   -- Connects every connector except Memory_Dump_Send (the dump pathway is
   -- left unconnected by default).
   procedure Connect (Self : in out Instance);
   -- Attaches the Memory_Dump_Send connector; call after Connect for tests that
   -- exercise the dump pathway.
   procedure Connect_Memory_Dump (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Command response.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Release back to the upstream sender with the operation status.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- Outbound validated memory region forwarded to the downstream component;
   -- returns the operation release status synchronously.
   -- For Set/Validate, the region points at the upstream caller's buffer with
   -- the validated header and data. For Get, the region points at the upstream
   -- caller's buffer to be filled by the downstream component. For internal
   -- Dump operations, the region points at the outgoing packet's buffer.
   overriding function Parameters_Memory_Region_T_Forwarded_Reciprocal (Self : in out Instance; Arg : in Parameters_Memory_Region.T) return Parameters_Memory_Region_Release.T;
   -- Memory dump send connector. Each Memory_Dump record is queued on the
   -- packetizer's async input; the tester captures them into a history
   -- for inspection by tests.
   overriding procedure Memory_Dump_Recv_Sync (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump);
   -- Events out.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Data products out.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- System time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Parameters_Memory_Region_T_Send message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Send_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Parameter Table Forwarder component.
   -- An incoming memory region length did not match the configured Table_Size.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Length.T);
   -- An incoming memory region's stored CRC did not match the computed CRC over its
   -- contents.
   overriding procedure Memory_Region_Crc_Invalid (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Crc.T);
   -- A new parameter table was successfully applied by the downstream component.
   overriding procedure Parameter_Table_Updated (Self : in out Instance; Arg : in Memory_Region.T);
   -- A parameter table was successfully validated by the downstream component.
   overriding procedure Parameter_Table_Validated (Self : in out Instance; Arg : in Memory_Region.T);
   -- The current parameter table was written into the requester's memory region by
   -- the downstream component.
   overriding procedure Parameter_Table_Fetched (Self : in out Instance; Arg : in Memory_Region.T);
   -- The downstream component rejected a Set operation.
   overriding procedure Downstream_Component_Rejected_Update (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- The downstream component rejected a Validate operation.
   overriding procedure Downstream_Component_Rejected_Validation (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- The downstream component rejected a Get operation.
   overriding procedure Downstream_Component_Rejected_Fetch (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- A Get_Pointer request was received from an external caller and rejected.
   overriding procedure Get_Pointer_Not_Supported (Self : in out Instance; Arg : in Memory_Region.T);
   -- An Active_Parameters packet was emitted.
   overriding procedure Dumped_Parameters (Self : in out Instance);
   -- A Dump command failed because the downstream component rejected the internal
   -- Get.
   overriding procedure Dump_Failed (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A parameter memory region was dropped due to a full queue.
   overriding procedure Memory_Region_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Parameter Table Forwarder component.
   -- Version/CRC/timestamp/last-operation status of the active table as known
   -- to the forwarder. Emitted on every Set and Validate operation; Get_Copy,
   -- Get_Pointer, and the Dump command do not advance it.
   overriding procedure Table_Status (Self : in out Instance; Arg : in Packed_Table_Operation_Status.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Parameter Table Forwarder component.
   -- Snapshot of the current parameter table fetched from the downstream
   -- component at the time of dump, prefixed with a freshly computed CRC for
   -- bit-rot detection.
   overriding procedure Active_Parameters (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Parameter_Table_Forwarder.Implementation.Tester;
