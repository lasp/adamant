--------------------------------------------------------------------------------
-- Task_Watchdog Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Task_Watchdog_Reciprocal;
with Sys_Time;
with Printable_History;
with Pet.Representation;
with Command_Response.Representation;
with Fault.Representation;
with Event.Representation;
with Data_Product.Representation;
with Sys_Time.Representation;
with Fault;
with Packed_U16.Representation;
with Data_Product;
with Packed_Watchdog_Component_State.Representation;
with Packed_U32.Representation;
with Event;
with Watchdog_Limit_Cmd.Representation;
with Watchdog_Action_Cmd.Representation;
with Packed_Connector_Index.Representation;
with Invalid_Command_Info.Representation;

-- The Task Watchdog component receives pets from components that execute in a periodic manner throughout the assembly. The receipt of a pet indicates that the component is running well and is what is referred to as a watchdog. If it detects that the component has stopped executing for some configurable time, called a limit, it will either ignore the fault, throw a warning event, or throw a fault and possibly stop servicing an downstream watchdog (usually a hardware watchdog) based on the component's configuration. The configuration is dependent on the input list generated from a yaml model for this component. The input requires a connector name, a limit for the number of ticks without a pet, the criticality of the component, and the action to take if the limit is exceeded which is one of the three described before.
-- In addition, the criticality of the task is also defined in the yaml model which determines if the watchdog component stops petting the downstream watchdog.
package Component.Task_Watchdog.Implementation.Tester is

   use Component.Task_Watchdog_Reciprocal;
   -- Invoker connector history packages:
   package Pet_T_Recv_Sync_History_Package is new Printable_History (Pet.T, Pet.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Fault_T_Recv_Sync_History_Package is new Printable_History (Fault.T, Fault.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Watchdog_Pet_Checks_Enabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Watchdog_Pet_Checks_Disabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Watchdog_Limit_Set_History_Package is new Printable_History (Watchdog_Limit_Cmd.T, Watchdog_Limit_Cmd.Representation.Image);
   package Watchdog_Action_Set_History_Package is new Printable_History (Watchdog_Action_Cmd.T, Watchdog_Action_Cmd.Representation.Image);
   package Watchdog_Limit_Change_Index_Out_Of_Range_History_Package is new Printable_History (Packed_Connector_Index.T, Packed_Connector_Index.Representation.Image);
   package Watchdog_Action_Change_Index_Out_Of_Range_History_Package is new Printable_History (Packed_Connector_Index.T, Packed_Connector_Index.Representation.Image);
   package Watchdog_Action_Change_Invalid_Transition_To_Fault_History_Package is new Printable_History (Packed_Connector_Index.T, Packed_Connector_Index.Representation.Image);
   package Component_Exceeded_Pet_Limit_History_Package is new Printable_History (Packed_Connector_Index.T, Packed_Connector_Index.Representation.Image);
   package Critical_Task_Not_Petting_History_Package is new Printable_History (Packed_Connector_Index.T, Packed_Connector_Index.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Watchdog_Component_Petter_State_History_Package is new Printable_History (Packed_Watchdog_Component_State.T, Packed_Watchdog_Component_State.Representation.Image);
   package Pet_Connector_Action_States_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);

   -- Fault history packages:
   package Dummy_Fault_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Task_Watchdog_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Task_Watchdog.Implementation.Instance;
      -- Connector histories:
      Pet_T_Recv_Sync_History : Pet_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Fault_T_Recv_Sync_History : Fault_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Watchdog_Pet_Checks_Enabled_History : Watchdog_Pet_Checks_Enabled_History_Package.Instance;
      Watchdog_Pet_Checks_Disabled_History : Watchdog_Pet_Checks_Disabled_History_Package.Instance;
      Watchdog_Limit_Set_History : Watchdog_Limit_Set_History_Package.Instance;
      Watchdog_Action_Set_History : Watchdog_Action_Set_History_Package.Instance;
      Watchdog_Limit_Change_Index_Out_Of_Range_History : Watchdog_Limit_Change_Index_Out_Of_Range_History_Package.Instance;
      Watchdog_Action_Change_Index_Out_Of_Range_History : Watchdog_Action_Change_Index_Out_Of_Range_History_Package.Instance;
      Watchdog_Action_Change_Invalid_Transition_To_Fault_History : Watchdog_Action_Change_Invalid_Transition_To_Fault_History_Package.Instance;
      Component_Exceeded_Pet_Limit_History : Component_Exceeded_Pet_Limit_History_Package.Instance;
      Critical_Task_Not_Petting_History : Critical_Task_Not_Petting_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Watchdog_Component_Petter_State_History : Watchdog_Component_Petter_State_History_Package.Instance;
      Pet_Connector_Action_States_History : Pet_Connector_Action_States_History_Package.Instance;
      -- Fault histories:
      Dummy_Fault_History : Dummy_Fault_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Pet_T_Recv_Sync_Count : in Connector_Count_Type);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The pet send connector. This is used to service a downstream watchdog component, usually a component which services a hardware-based watchdog.
   overriding procedure Pet_T_Recv_Sync (Self : in out Instance; Arg : in Pet.T);
   -- This connector is used to register the components commands with the command router component.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Faults are sent on this connector.
   overriding procedure Fault_T_Recv_Sync (Self : in out Instance; Arg : in Fault.T);
   -- The post mortem log can be dumped using packets.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Data products for limit values and states.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Indicates a command was received to enable the checks on upstream pets.
   overriding procedure Watchdog_Pet_Checks_Enabled (Self : in out Instance);
   -- Indicates a command was received to disable the checks on upstream pets.
   overriding procedure Watchdog_Pet_Checks_Disabled (Self : in out Instance);
   -- An event to indicate that the limit was changed by command for a particular index.
   overriding procedure Watchdog_Limit_Set (Self : in out Instance; Arg : in Watchdog_Limit_Cmd.T);
   -- An event to indicate that the action was changed by command for a particular index.
   overriding procedure Watchdog_Action_Set (Self : in out Instance; Arg : in Watchdog_Action_Cmd.T);
   -- Event indicating there was an error for the index range in the set limit command.
   overriding procedure Watchdog_Limit_Change_Index_Out_Of_Range (Self : in out Instance; Arg : in Packed_Connector_Index.T);
   -- Event indicating there was an error for the index range in the set limit command.
   overriding procedure Watchdog_Action_Change_Index_Out_Of_Range (Self : in out Instance; Arg : in Packed_Connector_Index.T);
   -- Event indicating there was an error trying to set the action to fault. The petter did not have a fault declared in the model so the action cannot be set to fault.
   overriding procedure Watchdog_Action_Change_Invalid_Transition_To_Fault (Self : in out Instance; Arg : in Packed_Connector_Index.T);
   -- Event to indicate a pet connector has not received a pet within the set limits for that component.
   overriding procedure Component_Exceeded_Pet_Limit (Self : in out Instance; Arg : in Packed_Connector_Index.T);
   -- Event to indicate that one or more of our critical tasks have not indicated a pet in the maximum limit of ticks. The hardware watchdog will not be pet in this case.
   overriding procedure Critical_Task_Not_Petting (Self : in out Instance; Arg : in Packed_Connector_Index.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Task Watchdog component.
   -- Data product that tracks the global state to enable or disable all checks on the upstream watchdog pets.
   overriding procedure Watchdog_Component_Petter_State (Self : in out Instance; Arg : in Packed_Watchdog_Component_State.T);
   -- 2-bit of state for each pet connector indicating the current action that will be taken if there is an error. Note that Packed_U32.T is just a placeholder type for this data product. The actual type of this data product will be autocoded and at assembly model ingest time.
   overriding procedure Pet_Connector_Action_States (Self : in out Instance; Arg : in Packed_U32.T);

   -----------------------------------------------
   -- Fault handler primitive:
   -----------------------------------------------
   -- Description:
   --    Faults for the Task Watchdog component
   -- Dummy fault which will be deleted and filled back in from the watchdog list
   overriding procedure Dummy_Fault (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Custom functions
   -----------------------------------------------

   -- Override the reciprocal component base Dispatch_Data_Product procedure with our own. Since
   -- the data products are generated by the model, we need to allow data products to be produced that would
   -- otherwise cause the unit test to crash due to an out of range ID.
   overriding procedure Dispatch_Data_Product (Self : in out Instance; Dp : in Data_Product.T);
   overriding procedure Dispatch_Fault (Self : in out Instance; F : in Fault.T);

end Component.Task_Watchdog.Implementation.Tester;
