--------------------------------------------------------------------------------
-- Last_Chance_Manager Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;

-- The purpose of this component is to manage a region of non-volatile memory where the last chance handler saves exception information, should one be thrown. This component provides commands to dump this region of memory and reset the contents of the memory to all zeros. The component provides a data product that reports the first address of the stack trace, which can be used as confirmation that the LCH got called (if the value is nonzero). This component also supplies the packet definition for the assembly for a LCH packet that is created by the last chance handler itself (which is not usually implemented as an Adamant component). This provides the ground system the LCH packet definition so it can be parsed and stored.
package Component.Last_Chance_Manager.Implementation is

   -- The component class instance record:
   type Instance is new Last_Chance_Manager.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires the memory region which the last chance handler data will be stored.
   --
   -- Init Parameters:
   -- Exception_Data : Packed_Exception_Occurrence.T_Access - The copy of the exception data that is updated by the last chance handler, presumably in a nonvolatile memory region.
   -- Dump_Exception_Data_At_Startup : Boolean - If True, then the exception data will be dumped in packet at startup.
   --
   overriding procedure Init (Self : in out Instance; Exception_Data : in not null Packed_Exception_Occurrence.T_Access; Dump_Exception_Data_At_Startup : in Boolean);

private

   -- The component class instance record:
   type Instance is new Last_Chance_Manager.Base_Instance with record
      Exception_Data : Packed_Exception_Occurrence.T_Access := null;
      Dump_Exception_Data_At_Startup : Boolean := False;
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
   -- The command receive connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Last Chance Manager component.
   -- Dump the last chance handler memory region into a packet for downlink.
   overriding function Dump_Last_Chance_Handler_Region (Self : in out Instance) return Command_Execution_Status.E;
   -- Clear the last chance handler memory region by writing all zeros to it.
   overriding function Clear_Last_Chance_Handler_Region (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Last_Chance_Manager.Implementation;
