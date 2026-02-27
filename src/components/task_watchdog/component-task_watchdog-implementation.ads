--------------------------------------------------------------------------------
-- Task_Watchdog Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Pet;
with Command;
with Watchdog_List;
with Task_Watchdog_Types;
with Task_Watchdog_Enums;
with Fault_Types;

-- The Task Watchdog component receives pets from components that execute in a periodic manner throughout the assembly. The receipt of a pet indicates that the component is running well. If it detects that the component has stopped executing for some configurable time, it will throw a warning event, a fault, or stop servicing a downstream watchdog (usually a hardware watchdog) based on the component's configuration.
package Component.Task_Watchdog.Implementation is
   use Task_Watchdog_Types;
   use Task_Watchdog_Enums;
   use Watchdog_List;
   -- The component class instance record:
   type Instance is new Task_Watchdog.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Task_Watchdog_Entry_Init_List : Task_Watchdog_Types.Task_Watchdog_Init_List - The list of components that have a watchdog to pet that need to be tracked by the task watchdog.
   --
   overriding procedure Init (Self : in out Instance; Task_Watchdog_Entry_Init_List : in Task_Watchdog_Types.Task_Watchdog_Init_List);

   --------------------------------------------------
   -- Protected object to handle internal counters for the watchdog petters:
   --------------------------------------------------
   protected type Protected_Watchdog_Entries is
      -- init
      procedure Init (Task_Watchdog_Entry_Init_List : in Task_Watchdog_Init_List);
      -- Procedure to fetch the event range. This helps keep the component in sync with the package
      procedure Reset_Pet_Count (Index : in Connector_Types.Connector_Index_Type);
      procedure Set_Pet_Limit (Index : in Connector_Types.Connector_Index_Type; New_Limit : in Missed_Pet_Limit_Type);
      procedure Set_Pet_Action (Index : in Connector_Types.Connector_Index_Type; New_Action : in Watchdog_Action_State.E);
      -- Function to check the watchdog petting status and counters. Returns if the pets are still being received or not.
      function Check_Watchdog_Pets (Index : in Connector_Types.Connector_Index_Type; Is_Critical : out Boolean) return Check_Status;
      -- Functions for the component to fetch the static critical and action items.
      function Get_Pet_Criticality (Index : in Connector_Types.Connector_Index_Type) return Boolean;
      function Get_Pet_Action (Index : in Connector_Types.Connector_Index_Type) return Watchdog_Action_State.E;
      function Get_Pet_Action_Id (Index : in Connector_Types.Connector_Index_Type) return Fault_Types.Fault_Id;
      function Get_Pet_Has_Fault (Index : in Connector_Types.Connector_Index_Type) return Boolean;
      -- Getters for the commands to make sure that the item was set.
      function Get_Pet_Count_Limit (Index : in Connector_Types.Connector_Index_Type) return Missed_Pet_Limit_Type;

   private
      Protected_Watchdog_Entries_Package : Watchdog_List.Instance;
   end Protected_Watchdog_Entries;

private
   -- Helper functions to create faults and data products
   procedure Send_Fault (Self : in out Instance; Index : in Connector_Types.Connector_Index_Type; Timestamp : in Sys_Time.T);
   procedure Send_Limit_Dp (Self : in out Instance; Index : in Connector_Types.Connector_Index_Type; Timestamp : in Sys_Time.T; Tick_Limit : in Missed_Pet_Limit_Type);
   procedure Send_Action_Dp (Self : in out Instance; The_Time : in Sys_Time.T);

   -- The component class instance record:
   type Instance is new Task_Watchdog.Base_Instance with record
      Task_Watchdog_Entries : Protected_Watchdog_Entries;
      -- Flag to indicate if we want to do checks on the petters
      Task_Watchdog_Component_State : Watchdog_Enabled_State.E := Watchdog_Enabled_State.Enabled;
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
   -- The schedule invokee connector.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- The arrayed pet receive connector. Upstream components call this connector to let the Task Watchdog know they are running OK.
   overriding procedure Pet_T_Recv_Sync (Self : in out Instance; Index : in Pet_T_Recv_Sync_Index; Arg : in Pet.T);
   -- The command receive connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Pet_T_Send message is dropped due to a full queue.
   overriding procedure Pet_T_Send_Dropped (Self : in out Instance; Arg : in Pet.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Fault_T_Send message is dropped due to a full queue.
   overriding procedure Fault_T_Send_Dropped (Self : in out Instance; Arg : in Fault.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Task Watchdog component.
   -- Command to enable the watchdog component to check all connected components for incoming pets.
   overriding function Enable_Watchdog_Pet_Checks (Self : in out Instance) return Command_Execution_Status.E;
   -- Command to disable the watchdog component to check all connected components for incoming pets.
   overriding function Disable_Watchdog_Pet_Checks (Self : in out Instance) return Command_Execution_Status.E;
   -- Set the limit value for the watchdog given an index and the new index value.
   overriding function Set_Watchdog_Limit (Self : in out Instance; Arg : in Watchdog_Limit_Cmd.T) return Command_Execution_Status.E;
   -- Sets the action of a petter given the index of that petter and the updated action. Note that actions cannot be promoted to fault if they were not provided a fault id.
   overriding function Set_Watchdog_Action (Self : in out Instance; Arg : in Watchdog_Action_Cmd.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Task_Watchdog.Implementation;
