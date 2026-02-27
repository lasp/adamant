--------------------------------------------------------------------------------
-- Memory_Dumper Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;

-- The memory dumper component is an active component that can dump memory regions or report the CRC of memory regions by command. It reports an error if an action is requested on a memory region outside of the address space that it is configured with during initialization.
package Component.Memory_Dumper.Implementation is

   -- The component class instance record:
   type Instance is new Memory_Dumper.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of memory regions which it can dump and CRC.
   --
   -- Init Parameters:
   -- Memory_Regions : Memory_Manager_Types.Memory_Region_Array_Access - An access to a list of memory regions.
   --
   overriding procedure Init (Self : in out Instance; Memory_Regions : in not null Memory_Manager_Types.Memory_Region_Array_Access);

private

   -- The component class instance record:
   type Instance is new Memory_Dumper.Base_Instance with record
      Regions : Memory_Manager_Types.Memory_Region_Array_Access := null;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is null;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Memory_Dump_Send message is dropped due to a full queue.
   overriding procedure Memory_Dump_Send_Dropped (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Memory Dumper component.
   -- Dump a region of memory starting at a given address and of a given length.
   overriding function Dump_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T) return Command_Execution_Status.E;
   -- Perform a CRC on a region of memory starting at a given address and of a given length. The CRC will be reported via event and data product, if those connectors are connected.
   overriding function Crc_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Memory_Dumper.Implementation;
