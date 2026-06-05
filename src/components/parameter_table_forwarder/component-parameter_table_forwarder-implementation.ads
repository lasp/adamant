--------------------------------------------------------------------------------
-- Parameter_Table_Forwarder Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Memory_Packetizer_Types;
with Parameter_Table_Header;
with Parameters_Memory_Region;
with Parameters_Memory_Region_Release;

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
package Component.Parameter_Table_Forwarder.Implementation is

   -- The component class instance record:
   type Instance is new Parameter_Table_Forwarder.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The forwarder is initialized with the expected size of the downstream
   -- component's parameter table and a dump-on-change flag.
   --
   -- Init Parameters:
   -- Table_Size : Natural - The expected size in bytes of the parameter table
   -- managed by the downstream component. Set/Validate operations require the
   -- incoming memory region length to equal Table_Size exactly. Get operations
   -- require the incoming memory region length to be at least Table_Size.
   -- Dump_Parameters_On_Change : Boolean - If True, automatically dump the
   -- current table (by forwarding a Get to the downstream component) after
   -- every Set that returns Success.
   --
   overriding procedure Init (Self : in out Instance; Table_Size : in Natural; Dump_Parameters_On_Change : in Boolean := False);

private

   -- The component class instance record:
   type Instance is new Parameter_Table_Forwarder.Base_Instance with record
      -- Configured at Init:
      Table_Size : Natural := 0;
      Dump_On_Change : Boolean := False;

      -- The header of the last successfully applied Set. Serves two roles:
      -- (1) bookkeeping for the Table_Status data product (Crc_Table and
      -- Version), and (2) the source bytes for the Active_Parameters dump.
      -- Parameter_Table_Header.T is a packed record whose in-memory bytes
      -- match the wire format, so Stored_Header'Address can be handed
      -- directly to the Memory_Packetizer without an intermediate buffer.
      -- Living as a record field ensures the address remains valid across
      -- the asynchronous send to the packetizer.
      Stored_Header : Parameter_Table_Header.T :=
         (Crc_Table => [0, 0], Version => 0.0);
      Table_Update_Time : Interfaces.Unsigned_32 := 0;
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
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Command receive.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- Inbound parameter table memory region from the router (or an upstream
   -- Parameter_Store). The operation (Set, Validate, Get) is in the region.
   overriding procedure Parameters_Memory_Region_T_Recv_Async (Self : in out Instance; Arg : in Parameters_Memory_Region.T);
   -- This procedure is called when a Parameters_Memory_Region_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Parameters_Memory_Region_Release_T_Send message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_Release_T_Send_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is null;
   -- This procedure is called when a Memory_Dump_Send message is dropped due to a full queue.
   overriding procedure Memory_Dump_Send_Dropped (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Parameter Table Forwarder component.
   -- Fetch the current parameter table from the downstream component by
   -- issuing an internal Get and emit it as an Active_Parameters packet.
   overriding function Dump_Parameter_Table (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Parameter_Table_Forwarder.Implementation;
