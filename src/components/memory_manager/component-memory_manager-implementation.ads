--------------------------------------------------------------------------------
-- Memory_Manager Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Memory_Region;
with Ided_Memory_Region;
with Command;
with Memory_Manager_Enums;

-- The component manages access to a single memory location through a single pointer. When requested, the component loans out access to the pointer if it is available. The length of the pointer will always be the entire length of the memory region. The component will reject any requests to access the pointer again until the pointer is returned from the requester. Request/release memory transactions are each provided a unique ID. To release the memory, the same ID must be provided that was issued upon request. This mechanism reduces the risk of an inadvertent call to release from causing an unintended release of the memory. The component includes a data product relating whether the memory is currently allocated or not. The component responds to commands to CRC, dump, and force-release the memory region. Note that this component is active only to provide a separate thread of execution on which to execute the CRC command, which could take a long time to execute.
package Component.Memory_Manager.Implementation is

   -- The component class instance record:
   type Instance is new Memory_Manager.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This init function provides memory allocation for the managers internal memory region. Preallocated memory can be provided via the "bytes" access type, in which case "size" must be negative and will be ignored. If you would like to allocate the internal memory on the heap then "bytes" must be set to null, and "size" must be a positive number representing the number of bytes you would like to allocate.
   --
   -- Init Parameters:
   -- bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to be used for the memory region. If this is set to null, then memory will be allocated on the heap using the "size" parameter instead. Note: This must be set to null if the "size" parameter is positive below.
   -- size : Integer - The number of bytes to allocate on the heap for the memory region. Note: This must be set to a negative value if the "bytes" parameters is not null.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in Basic_Types.Byte_Array_Access := null; Size : in Integer := -1);

private

   -- Protected type to manage access to the memory region. This ensures thread safety should
   -- more than one task request or release the memory region at the same time.
   type Request_Status is (Success, Memory_Unavailable);
   type Release_Status is (Success, Memory_Available, Unexpected_Id);
   protected type Protected_Memory_Arbiter is
      -- Request access. If status is set to true then a unique ID is returned.
      procedure Request (Self : in out Instance; Id : out Unsigned_16; State : out Memory_Manager_Enums.Memory_State.E; Status : out Request_Status);
      -- Release access with a given ID. If status is set to true then the release
      -- succeeded, otherwise an unexpected ID was returned.
      procedure Release (Self : in out Instance; Id : in Unsigned_16; State : out Memory_Manager_Enums.Memory_State.E; Status : out Release_Status);
      -- Release access, no ID needed.
      procedure Force_Release (Self : in out Instance);
      -- Get the current state of the memory access:
      function Get_State return Memory_Manager_Enums.Memory_State.E;
      function Get_Current_Id return Unsigned_16;
   private
      Current_State : Memory_Manager_Enums.Memory_State.E := Memory_Manager_Enums.Memory_State.Available;
      Current_Id : Unsigned_16 := 0;
   end Protected_Memory_Arbiter;

   -- The component class instance record:
   type Instance is new Memory_Manager.Base_Instance with record
      -- The allocation of the virtual memory region:
      Bytes : Basic_Types.Byte_Array_Access;
      -- The (protected) arbiter object for the virtual memory region:
      Arbiter : Protected_Memory_Arbiter;
      -- A copy of the memory region managed in "bytes" for efficient
      -- returning to requesters.
      Region : Memory_Region.T;
      Virtual_Region : Virtual_Memory_Region_Positive.T;
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
   -- The memory region is requested on this connector.
   overriding function Memory_Region_Request_T_Return (Self : in out Instance) return Memory_Region_Request.T;
   -- The memory region is released (returned) on this connector.
   overriding procedure Ided_Memory_Region_T_Release (Self : in out Instance; Arg : in Ided_Memory_Region.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);

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
   --    These are the commands for the Memory Manager component.
   -- Dump the entire memory region.
   overriding function Dump_Memory_Region (Self : in out Instance) return Command_Execution_Status.E;
   -- Dump the memory region at the provided virtual address and length.
   overriding function Dump_Memory_Region_Bytes (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T) return Command_Execution_Status.E;
   -- Perform a CRC on the region with the provided virtual address and length. The CRC will be reported via event and data product, if those connectors are connected.
   overriding function Crc_Memory_Region_Bytes (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T) return Command_Execution_Status.E;
   -- Perform a write to the memory region at the provided address. If the memory is not available an error event will be produced.
   overriding function Write_Memory_Region (Self : in out Instance; Arg : in Virtual_Memory_Region_Write.T) return Command_Execution_Status.E;
   -- Forces the release of the memory region if it is currently allocated. This command can be used to recover from an anomalous condition.
   overriding function Force_Release (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Memory_Manager.Implementation;
