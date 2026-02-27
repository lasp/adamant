--------------------------------------------------------------------------------
-- Memory_Stuffer Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Memory_Region_Copy;
with Arm_State;

-- The memory stuffer component is an active component that can stuff (write to) memory regions. It reports an error if an action is requested on a memory region outside of the address space that it is configured with during initialization.
package Component.Memory_Stuffer.Implementation is

   -- The component class instance record:
   type Instance is new Memory_Stuffer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of memory regions which it can write to. These regions can either be protected (requiring an arm command prior to execution) or unprotected, as specified by the second parameter.
   --
   -- Init Parameters:
   -- Memory_Regions : Memory_Manager_Types.Memory_Region_Array_Access - An access to a list of memory regions.
   -- Memory_Region_Protection_List : Memory_Manager_Types.Memory_Protection_Array_Access - An access to a list of the protected/unprotected state of each memory region. The index in this array corresponds to the index of the memory region affected in the previous parameter. If the array is null, then it is assumed that all memory regions are unprotected.
   --
   overriding procedure Init (Self : in out Instance; Memory_Regions : in not null Memory_Manager_Types.Memory_Region_Array_Access; Memory_Region_Protection_List : in Memory_Manager_Types.Memory_Protection_Array_Access := null);

private

   -- The component class instance record:
   type Instance is new Memory_Stuffer.Base_Instance with record
      -- The current arm state:
      Command_Arm_State : Arm_State.Protected_Arm_State;
      -- The memory regions:
      Regions : Memory_Manager_Types.Memory_Region_Array_Access := null;
      -- A list of whether the memory region is protected or not:
      Region_Protection_List : Memory_Manager_Types.Memory_Protection_Array_Access := null;
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
   -- This tick is used to keep track of the armed state timeout and send the data product relating the current timeout value.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T);
   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is null;
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is null;
   -- A memory region is received on this connector and stuffed to a different memory region, a memory copy.
   overriding procedure Memory_Region_Copy_T_Recv_Async (Self : in out Instance; Arg : in Memory_Region_Copy.T);
   -- This procedure is called when a Memory_Region_Copy_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Memory_Region_Copy_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Memory_Region_Copy.T) is null;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Memory_Region_T_Send message is dropped due to a full queue.
   overriding procedure Memory_Region_Release_T_Send_Dropped (Self : in out Instance; Arg : in Memory_Region_Release.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Memory Stuffer component.
   -- Write bytes to a region in memory.
   overriding function Write_Memory (Self : in out Instance; Arg : in Memory_Region_Write.T) return Command_Execution_Status.E;
   -- An arm command which enables the next write command to a protected memory to be accepted. The armed state of the component will expire on the next command to this component no matter what it is or after the configurable timeout.
   overriding function Arm_Protected_Write (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Memory_Stuffer.Implementation;
