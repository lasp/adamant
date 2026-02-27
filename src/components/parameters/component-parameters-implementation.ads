--------------------------------------------------------------------------------
-- Parameters Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Crc_16;

-- The Parameters Component is responsible for updating and reporting the values of the "active" parameters being used in the system. The component does not contain a parameter table itself. Instead it acts as an interface for the rest of the system to component's internal staged parameters. The component allows updating of parameters through a table upload (via Memory_Region.T) or updating of individual parameter values by command. The component also provides a command to fetch all of the parameters held within components and produce a packet with the fetched values. The component can be configured to produce this packet automatically any time a parameter change is requested.
package Component.Parameters.Implementation is

   -- The component class instance record:
   type Instance is new Parameters.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This init function provides a list of parameter entries that describe the
   -- layout of the parameter table in memory. Calling this function also provides
   -- memory allocation for the parameter manager's internal parameter table.
   -- Preallocated memory can be provided via the "bytes" access type. Note the size
   -- of the preallocated memory MUST match the size of the parameter table exactly,
   -- as defined in the Parameter_Entries parameter. If you would like to allocate
   -- the internal memory on the heap then "bytes" can be set to null.
   --
   -- Init Parameters:
   -- Parameter_Table_Entries :
   -- Parameters_Component_Types.Parameter_Table_Entry_List_Access - A pointer to an
   -- autocoded list of parameter table entries. This table tells the parameter
   -- manager how the parameters are laid out in memory, so that it knows how to
   -- construct parameter types to update downstream components.
   -- Table_Id : Parameter_Types.Parameter_Table_Id - Provide a unique parameter
   -- table ID for this parameter table. This item is autocoded in the same package
   -- as the parameter table entries list.
   -- Dump_Parameters_On_Change : Boolean - If set to True, the component will dump
   -- the current parameter values any time a command or memory region is received to
   -- alter one or more parameter values. If set to False, parameters will only be
   -- dumped when requested by command.
   --
   overriding procedure Init (Self : in out Instance; Parameter_Table_Entries : in not null Parameters_Component_Types.Parameter_Table_Entry_List_Access; Table_Id : in Parameter_Types.Parameter_Table_Id; Dump_Parameters_On_Change : in Boolean := False);

private

   -- The component class instance record:
   type Instance is new Parameters.Base_Instance with record
      Table_Id : Parameter_Types.Parameter_Table_Id := 1;
      Entries : Parameters_Component_Types.Parameter_Table_Entry_List_Access := null;
      Dump_Parameters_On_Change : Boolean := False;
      Stored_Crc : Crc_16.Crc_16_Type := [0, 0];
      Table_Version : Short_Float := 0.0;
      Table_Update_Time : Interfaces.Unsigned_32 := 0;
      -- Some useful constants calculated in Init:
      Parameter_Table_Data_Length : Natural := 0;
      Parameter_Table_Length : Natural := 0;
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
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- When a memory region is received on this connector it can either be a parameter table that is used to stage and update the parameters of all connected components, or it can be a memory region that is used to store the current value of the parameters stored within the component. The operation field determines which logic is run. For a "set" operation, the memory region length MUST match the length of the managed parameter table, otherwise the update will not be processed.
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
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Parameters component.
   -- Update the active parameter value in a component for a parameter table entry with the given ID, Length, and Value. If multiple parameters share the same entry ID (grouped parameters), all will be updated.
   overriding function Update_Parameter (Self : in out Instance; Arg : in Parameter_Table_Entry.T) return Command_Execution_Status.E;
   -- Produce a packet with the currently staged parameter values contained within connected components.
   overriding function Dump_Parameters (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Parameters.Implementation;
