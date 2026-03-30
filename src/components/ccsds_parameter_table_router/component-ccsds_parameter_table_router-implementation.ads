--------------------------------------------------------------------------------
-- Ccsds_Parameter_Table_Router Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Ccsds_Space_Packet;
with Command;
with Tick;
with Parameters_Memory_Region_Release;
with Protected_Variables;
with Task_Synchronization;
with Binary_Tree;
with Parameter_Table_Buffer;
with Ccsds_Parameter_Table_Router_Types;

-- This component receives segmented CCSDS packets containing parameter tables,
-- reassembles them in a staging buffer, and routes completed tables to downstream
-- components that accept a Parameters_Memory_Region type. It supports loading
-- parameter tables from a designated component on command, which will often be a
-- Parameter_Store sitting in front of persistent storage.
-- A generator exists to produce the routing table that maps parameter table IDs
-- to output connector indexes. See the gen/ subdirectory for documentation.
package Component.Ccsds_Parameter_Table_Router.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Parameter_Table_Router.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Initialization parameters for the Parameter Table Router.
   --
   -- Init Parameters:
   -- Table : Ccsds_Parameter_Table_Router_Types.Router_Table - The routing table mapping
   -- parameter table IDs to destination connector indexes. Typically produced by the
   -- generator.
   -- Buffer_Size : Positive - The size in bytes of the internal staging buffer for
   -- reassembling segmented CCSDS packets.
   -- Ticks_Until_Timeout : Natural - The number of timeout ticks to wait for a
   -- response from a downstream component before declaring a timeout.
   -- Load_All_Parameter_Tables_On_Set_Up : Boolean - If True, all parameter tables
   -- that have a load_from source will be loaded from persistent storage during
   -- Set_Up.
   --
   overriding procedure Init (Self : in out Instance; Table : in Ccsds_Parameter_Table_Router_Types.Router_Table; Buffer_Size : in Positive; Ticks_Until_Timeout : in Natural; Load_All_Parameter_Tables_On_Set_Up : in Boolean := False);
   not overriding procedure Final (Self : in out Instance);

private
   use Ccsds_Parameter_Table_Router_Types;

   -- Protected variable for downstream response (same pattern as Parameter Manager):
   package Protected_Parameters_Memory_Region_Release is
      new Protected_Variables.Generic_Variable (Parameters_Memory_Region_Release.T);

   -- Protected counter for reject count (incremented from both the component's
   -- task and the sender's task context via the Dropped handler):
   package Protected_Unsigned_32_Counter is
      new Protected_Variables.Generic_Protected_Counter (Interfaces.Unsigned_32);

   -- Binary tree comparison operators for Router_Table_Entry keyed by Table_Id:
   function Less_Than (Left, Right : Router_Table_Entry) return Boolean with
      Inline => True;
   function Greater_Than (Left, Right : Router_Table_Entry) return Boolean with
      Inline => True;
   package Router_Table_B_Tree is new Binary_Tree (Router_Table_Entry, Less_Than, Greater_Than);

   -- The component class instance record:
   type Instance is new Ccsds_Parameter_Table_Router.Base_Instance with record
      -- Routing table:
      Table : Router_Table_B_Tree.Instance;
      -- Staging buffer:
      Staging_Buffer : Parameter_Table_Buffer.Instance;
      -- Synchronization for downstream responses:
      Response : Protected_Parameters_Memory_Region_Release.Variable;
      Sync_Object : Task_Synchronization.Wait_Release_Timeout_Counter_Object;
      -- Configuration:
      Load_All_On_Set_Up : Boolean := False;
      -- Data product counters (Reject_Count is protected since
      -- Ccsds_Space_Packet_T_Recv_Async_Dropped runs in the sender's context):
      Packet_Count : Interfaces.Unsigned_32 := 0;
      Reject_Count : Protected_Unsigned_32_Counter.Counter;
      Valid_Table_Count : Interfaces.Unsigned_32 := 0;
      Invalid_Table_Count : Interfaces.Unsigned_32 := 0;
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
   -- Receives segmented CCSDS packets containing parameter table data.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- This procedure is called when a Ccsds_Space_Packet_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- The command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- Periodic tick used for timeout counting when waiting for downstream responses.
   overriding procedure Timeout_Tick_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- Synchronous response from downstream components after a Set or Get operation.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Parameters_Memory_Region_T_Send message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Send_Dropped (Self : in out Instance; Index : in Parameters_Memory_Region_T_Send_Index; Arg : in Parameters_Memory_Region.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Parameter Table Router component.
   -- Load a single parameter table from its load_from source and distribute to other
   -- destinations.
   overriding function Load_Parameter_Table (Self : in out Instance; Arg : in Parameter_Table_Id.T) return Command_Execution_Status.E;
   -- Load all parameter tables that have a load_from source configured and
   -- distribute to their destinations.
   overriding function Load_All_Parameter_Tables (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Ccsds_Parameter_Table_Router.Implementation;
