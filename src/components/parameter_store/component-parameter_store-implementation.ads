--------------------------------------------------------------------------------
-- Parameter_Store Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Parameters_Memory_Region;

-- The Parameters Component is responsible for storing and managing access to a memory region holding a parameter table. The managed memory region is usually located in nonvolatile storage and can serve as the backup or the default parameter values to use at startup for the system.
package Component.Parameter_Store.Implementation is

   -- The component class instance record:
   type Instance is new Parameter_Store.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the memory region it is to manage which holds the parameter table.
   --
   -- Init Parameters:
   -- bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to be used for storing the parameter table. The size of this byte array MUST be the exact size of the parameter table to be stored, or updating or fetching the table will be rejected with a length error.
   -- Dump_Parameters_On_Change : Boolean - If set to True, the component will dump the current parameter values any time a memory region is received to change the parameter table. If set to False, parameters will only be dumped when requested by command.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in not null Basic_Types.Byte_Array_Access; Dump_Parameters_On_Change : in Boolean := False);

private

   -- The component class instance record:
   type Instance is new Parameter_Store.Base_Instance with record
      -- The allocation of bytes to the parameter table:
      Bytes : Basic_Types.Byte_Array_Access := null;
      -- Should the parameter table be dumped automatically when updated?
      Dump_Parameters_On_Change : Boolean := False;
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
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- When a memory region is received on this connector it is assumed that it contains a memory region that is the same size as the managed region?
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
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Parameter Store component.
   -- Produce a packet with the currently stored parameter values.
   overriding function Dump_Parameter_Store (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Parameter_Store.Implementation;
