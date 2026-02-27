--------------------------------------------------------------------------------
-- Fault_Correction Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Fault;
with Fault_Types; use Fault_Types;
with Fault_Correction_Enums;
with Binary_Tree;

-- The Fault Correction component receives faults asynchronously. When it processes a fault, it determines the correct command response to send and sends it.
package Component.Fault_Correction.Implementation is

   -- The component class instance record:
   type Instance is new Fault_Correction.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing an access to a list of fault response configuration records.
   --
   -- Init Parameters:
   -- Fault_Response_Configurations : Fault_Correction_Types.Fault_Response_Config_List - An access to a list of fault response configurations.
   --
   overriding procedure Init (Self : in out Instance; Fault_Response_Configurations : in Fault_Correction_Types.Fault_Response_Config_List);
   not overriding procedure Final (Self : in out Instance);

private

   -- Define the lookup entry type. This type is stored as a node
   -- in a binary tree and defines where in the table the response
   -- entry is stored.
   type Fault_Response_Table_Index is new Natural;
   type Fault_Response_Lookup_Entry is record
      Id : Fault_Types.Fault_Id;
      Table_Index : Fault_Response_Table_Index := Fault_Response_Table_Index'First;
   end record;

   -- Define the protected command binary tree package:
   function Less_Than (Left, Right : in Fault_Response_Lookup_Entry) return Boolean is (Left.Id < Right.Id)
      with Inline => True;
   function Greater_Than (Left, Right : in Fault_Response_Lookup_Entry) return Boolean is (Left.Id > Right.Id)
      with Inline => True;
   package Fault_Response_Lookup_B_Tree is new Binary_Tree (Fault_Response_Lookup_Entry, Less_Than, Greater_Than);

   -- Define the table entry type:
   type Fault_Response_Table_Entry is record
      Latching : Fault_Correction_Enums.Latching_Type.E;
      Status : Fault_Correction_Enums.Status_Type.E;
      Command_Response : Command.T;
   end record;

   -- Define the table type:
   type Fault_Response_Table_Type is array (Fault_Response_Table_Index range <>) of Fault_Response_Table_Entry;
   type Fault_Response_Table_Type_Access is access all Fault_Response_Table_Type;

   -- The component class instance record:
   type Instance is new Fault_Correction.Base_Instance with record
      -- Internal fault response table. We have a binary tree lookup that maps
      -- fault ids to an index in the fault response table. The fault response
      -- table includes the command response to send.
      Fault_Response_Lookup : Fault_Response_Lookup_B_Tree.Instance;
      Fault_Response_Table : Fault_Response_Table_Type_Access := null;
      Fault_Counter : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
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
   -- Faults are received asynchronously on this connector.
   overriding procedure Fault_T_Recv_Async (Self : in out Instance; Arg : in Fault.T);
   -- This procedure is called when a Fault_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Fault_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Fault.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Fault Correction component.
   -- Enable a fault response for the provided ID. This will only succeed if another response with the same Fault ID is not already enabled.
   overriding function Enable_Fault_Response (Self : in out Instance; Arg : in Packed_Fault_Id.T) return Command_Execution_Status.E;
   -- Disable a fault response for the provided ID.
   overriding function Disable_Fault_Response (Self : in out Instance; Arg : in Packed_Fault_Id.T) return Command_Execution_Status.E;
   -- Resets a fault response to the Enabled state of the provided ID. If the fault is latched, it unlatches the fault.
   overriding function Clear_Fault_Response (Self : in out Instance; Arg : in Packed_Fault_Id.T) return Command_Execution_Status.E;
   -- Resets all fault responses to the Enabled state. Unlatches all latched fault responses.
   overriding function Clear_All_Fault_Responses (Self : in out Instance) return Command_Execution_Status.E;
   -- This command resets the values of all the component's data product to the values at initialization, except for the Fault_Response_Statuses data product which can be reset by the Clear_All_Fault_Responses command.
   overriding function Reset_Data_Products (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Fault_Correction.Implementation;
