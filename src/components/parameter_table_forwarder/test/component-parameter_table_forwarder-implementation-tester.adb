--------------------------------------------------------------------------------
-- Parameter_Table_Forwarder Component Tester Body
--------------------------------------------------------------------------------

-- Includes:
with String_Util;
with Byte_Array_Pointer.Packed;

package body Component.Parameter_Table_Forwarder.Implementation.Tester is

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
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Init (Depth => 100);
      Self.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Init (Depth => 100);
      Self.Memory_Dump_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Memory_Region_Length_Mismatch_History.Init (Depth => 100);
      Self.Memory_Region_Crc_Invalid_History.Init (Depth => 100);
      Self.Parameter_Table_Updated_History.Init (Depth => 100);
      Self.Parameter_Table_Validated_History.Init (Depth => 100);
      Self.Parameter_Table_Fetched_History.Init (Depth => 100);
      Self.Downstream_Component_Rejected_Update_History.Init (Depth => 100);
      Self.Downstream_Component_Rejected_Validation_History.Init (Depth => 100);
      Self.Downstream_Component_Rejected_Fetch_History.Init (Depth => 100);
      Self.Get_Pointer_Not_Supported_History.Init (Depth => 100);
      Self.Dumped_Parameters_History.Init (Depth => 100);
      Self.Dump_Failed_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Command_Dropped_History.Init (Depth => 100);
      Self.Memory_Region_Dropped_History.Init (Depth => 100);
      -- Data product histories:
      Self.Table_Status_History.Init (Depth => 100);
      -- Packet histories:
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Destroy;
      Self.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Destroy;
      Self.Memory_Dump_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Memory_Region_Length_Mismatch_History.Destroy;
      Self.Memory_Region_Crc_Invalid_History.Destroy;
      Self.Parameter_Table_Updated_History.Destroy;
      Self.Parameter_Table_Validated_History.Destroy;
      Self.Parameter_Table_Fetched_History.Destroy;
      Self.Downstream_Component_Rejected_Update_History.Destroy;
      Self.Downstream_Component_Rejected_Validation_History.Destroy;
      Self.Downstream_Component_Rejected_Fetch_History.Destroy;
      Self.Get_Pointer_Not_Supported_History.Destroy;
      Self.Dumped_Parameters_History.Destroy;
      Self.Dump_Failed_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Command_Dropped_History.Destroy;
      Self.Memory_Region_Dropped_History.Destroy;
      -- Data product histories:
      Self.Table_Status_History.Destroy;
      -- Packet histories:

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   -- Connects every connector EXCEPT Memory_Dump_Send. The dump pathway is
   -- left unconnected by default so tests exercise the "no dump connector"
   -- behavior; tests that need to dump call Connect_Memory_Dump in addition.
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Parameters_Memory_Region_Release_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Parameters_Memory_Region_Release_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Parameters_Memory_Region_T_Forwarded (To_Component => Self'Unchecked_Access, Hook => Self.Parameters_Memory_Region_T_Forwarded_Reciprocal_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Parameters_Memory_Region_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Parameters_Memory_Region_T_Recv_Async_Access);
   end Connect;

   -- Attaches the Memory_Dump_Send connector. Call after Connect for tests that
   -- exercise the dump pathway (dump command, dump-on-change).
   procedure Connect_Memory_Dump (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Memory_Dump_Send (To_Component => Self'Unchecked_Access, Hook => Self.Memory_Dump_Recv_Sync_Access);
   end Connect_Memory_Dump;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Command response.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Release back to the upstream sender with the operation status.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Push (Arg);
   end Parameters_Memory_Region_Release_T_Recv_Sync;

   -- Outbound validated memory region forwarded to the downstream component;
   -- returns the operation release status synchronously.
   -- Mock downstream: writes a pattern into the region on Get, then returns a
   -- release with the configured per-op status. This mirrors the real
   -- downstream contract (service handler runs synchronously on the
   -- forwarder's task and returns the release directly).
   overriding function Parameters_Memory_Region_T_Forwarded_Reciprocal (Self : in out Instance; Arg : in Parameters_Memory_Region.T) return Parameters_Memory_Region_Release.T is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      Status : Parameter_Enums.Parameter_Table_Update_Status.E;
   begin
      Self.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Push (Arg);

      case Arg.Operation is
         when Set =>
            Status := Self.Mock_Set_Release_Status;
         when Validate =>
            Status := Self.Mock_Validate_Release_Status;
         when Get_Copy =>
            Status := Self.Mock_Get_Release_Status;
            if Status = Success then
               -- Fill the requested region with the pattern byte. Mimics a
               -- real downstream serializing its current bytes into the region.
               declare
                  Ptr : constant Byte_Array_Pointer.Instance :=
                     Byte_Array_Pointer.Packed.Unpack (Arg.Region);
                  Fill : constant Basic_Types.Byte_Array (0 .. Arg.Region.Length - 1) :=
                     [others => Self.Mock_Get_Fill_Pattern];
               begin
                  Byte_Array_Pointer.Copy_To (Ptr, Fill);
               end;
            end if;
         when Get_Pointer =>
            -- Mock downstream returns a pointer to its own snapshot buffer.
            -- For now we just echo Arg.Region back -- the forwarder relays
            -- whatever the downstream returns. (Real downstreams will
            -- return a pointer to their internal buffer.)
            Status := Self.Mock_Get_Release_Status;
      end case;

      return (Region => Arg.Region, Status => Status);
   end Parameters_Memory_Region_T_Forwarded_Reciprocal;

   -- Memory dump send connector. Captures each Memory_Dump record into the
   -- history. In production this connector lands on a Memory_Packetizer
   -- which chunks the pointed-at region into Packet.T's sharing the
   -- Memory_Dump's packet ID; the tester just records the records.
   overriding procedure Memory_Dump_Recv_Sync (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is
   begin
      Self.Memory_Dump_Recv_Sync_History.Push (Arg);
   end Memory_Dump_Recv_Sync;

   -- Events out.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- Data products out.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- System time.
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

   -- This procedure is called when a Parameters_Memory_Region_T_Send message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Send_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      Ignore : Parameters_Memory_Region.T renames Arg;
   begin
      if not Self.Expect_Parameters_Memory_Region_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Parameters_Memory_Region_T_Send was called!");
      else
         Self.Parameters_Memory_Region_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Parameters_Memory_Region_T_Send_Dropped := False;
      end if;
   end Parameters_Memory_Region_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Parameter Table Forwarder component.
   -- An incoming memory region length did not match the configured Table_Size.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Length_Mismatch_History.Push (Arg);
   end Memory_Region_Length_Mismatch;

   -- An incoming memory region's stored CRC did not match the computed CRC over its
   -- contents.
   overriding procedure Memory_Region_Crc_Invalid (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Crc.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Crc_Invalid_History.Push (Arg);
   end Memory_Region_Crc_Invalid;

   -- A new parameter table was successfully applied by the downstream component.
   overriding procedure Parameter_Table_Updated (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Table_Updated_History.Push (Arg);
   end Parameter_Table_Updated;

   -- A parameter table was successfully validated by the downstream component.
   overriding procedure Parameter_Table_Validated (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Table_Validated_History.Push (Arg);
   end Parameter_Table_Validated;

   -- The current parameter table was written into the requester's memory region by
   -- the downstream component.
   overriding procedure Parameter_Table_Fetched (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Table_Fetched_History.Push (Arg);
   end Parameter_Table_Fetched;

   -- The downstream component rejected a Set operation.
   overriding procedure Downstream_Component_Rejected_Update (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Downstream_Component_Rejected_Update_History.Push (Arg);
   end Downstream_Component_Rejected_Update;

   -- The downstream component rejected a Validate operation.
   overriding procedure Downstream_Component_Rejected_Validation (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Downstream_Component_Rejected_Validation_History.Push (Arg);
   end Downstream_Component_Rejected_Validation;

   -- The downstream component rejected a Get operation.
   overriding procedure Downstream_Component_Rejected_Fetch (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Downstream_Component_Rejected_Fetch_History.Push (Arg);
   end Downstream_Component_Rejected_Fetch;

   -- A Get_Pointer request was received from an external caller and rejected.
   overriding procedure Get_Pointer_Not_Supported (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      Self.Get_Pointer_Not_Supported_History.Push (Arg);
   end Get_Pointer_Not_Supported;

   -- An Active_Parameters packet was emitted.
   overriding procedure Dumped_Parameters (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumped_Parameters_History.Push (Arg);
   end Dumped_Parameters;

   -- A Dump command failed because the downstream component rejected the internal
   -- Get.
   overriding procedure Dump_Failed (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dump_Failed_History.Push (Arg);
   end Dump_Failed;

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

   -- A parameter memory region was dropped due to a full queue.
   overriding procedure Memory_Region_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Dropped_History.Push (Arg);
   end Memory_Region_Dropped;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Parameter Table Forwarder component.
   -- Version/CRC/timestamp/last-operation status of the active table as known
   -- to the forwarder. Updated on every Set/Validate/Get/Dump operation.
   overriding procedure Table_Status (Self : in out Instance; Arg : in Packed_Table_Operation_Status.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Status_History.Push (Arg);
   end Table_Status;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Parameter Table Forwarder component.
   -- Snapshot of the current parameter table fetched from the downstream
   -- component at the time of dump, prefixed with a freshly computed CRC for
   -- bit-rot detection.
   -- Active_Parameters packet primitive is required by the reciprocal
   -- because parameters_packets.yaml still defines the Active_Parameters
   -- packet ID (used by the Memory_Dump records). The forwarder no longer
   -- sends Packet.T directly -- the Memory_Packetizer emits packets with
   -- this APID via Memory_Dump_Send -- so this dispatch path is dead.
   overriding procedure Active_Parameters (Self : in out Instance; Arg : in Packet.T) is
      pragma Unreferenced (Self, Arg);
   begin
      null;
   end Active_Parameters;

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

end Component.Parameter_Table_Forwarder.Implementation.Tester;
