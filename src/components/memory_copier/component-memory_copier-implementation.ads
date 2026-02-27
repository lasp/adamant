--------------------------------------------------------------------------------
-- Memory_Copier Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Command;
with Protected_Variables;
with Task_Synchronization;

-- This component services a command to copy from one memory region to another. The to/from destination of the copy command is determined by how it is connected in the assembly. The component will wait a configurable timeout for the copy command to complete before failing the command and reporting a timeout error.
package Component.Memory_Copier.Implementation is

   -- The component class instance record:
   type Instance is new Memory_Copier.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Initialization parameters for the Memory Copier.
   --
   -- Init Parameters:
   -- Ticks_Until_Timeout : Natural - The component will wait until it has received at least this many ticks before reporting a timeout error while waiting for a memory copy to complete. For example, if the component is attached to a 10Hz rate group and this value is set to 7, then the component will wait between 700 and 800 ms before declaring a timeout error from an unresponsive downstream component.
   --
   overriding procedure Init (Self : in out Instance; Ticks_Until_Timeout : in Natural);

private

   -- Create a protected object that holds a memory region release. This will be set synchronously
   -- by the responses the downstream component, and will be read by this
   -- component, so must be protected to prevent corruption.
   package Protected_Memory_Region_Release is new Protected_Variables.Generic_Variable (Memory_Region_Release.T);

   -- The component class instance record:
   type Instance is new Memory_Copier.Base_Instance with record
      -- Memory region release protected variable, set by downstream components.
      Response : Protected_Memory_Region_Release.Variable;
      -- Variables used for task synchronization and timeouts:
      Sync_Object : Task_Synchronization.Wait_Release_Timeout_Counter_Object;
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
   -- The component should be attached to a periodic tick that is used to timeout waiting for a memory region copy response. See the Ticks_Until_Timeout initialization parameter.
   overriding procedure Timeout_Tick_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- The command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- The memory region is returned synchronously on this connector. The component waits internally for this response, or times out if the response is not received in time.
   overriding procedure Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Memory_Region_Release.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Memory_Region_Copy_T_Send message is dropped due to a full queue.
   overriding procedure Memory_Region_Copy_T_Send_Dropped (Self : in out Instance; Arg : in Memory_Region_Copy.T) is null;
   -- This procedure is called when a Memory_Region_Release message is dropped due to a full queue.
   overriding procedure Ided_Memory_Region_Release_Dropped (Self : in out Instance; Arg : in Ided_Memory_Region.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Memory Copier component.
   -- Copy Length bytes of memory from the source to the destination Address.
   overriding function Copy_Memory_Region (Self : in out Instance; Arg : in Virtual_Memory_Region_Copy.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Memory_Copier.Implementation;
