--------------------------------------------------------------------------------
-- Task_Watchdog Component Tester Body
--------------------------------------------------------------------------------

with Safe_Deallocator;
package body Component.Task_Watchdog.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Pet_T_Recv_Sync_Count : in Connector_Count_Type) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Pet_T_Recv_Sync_Count => Pet_T_Recv_Sync_Count);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Pet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Fault_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Allocated the arrayed connector for the tester invoker connector Pet_T_Send. We
      -- make this the same size as allocated for the component under test.
      if Pet_T_Recv_Sync_Count > 0 then
         Self.Connector_Pet_T_Send := new Pet_T_Send_Array (Pet_T_Send_Index'First .. Pet_T_Send_Index'First + Pet_T_Recv_Sync_Count - 1);
      end if;
      -- Event histories:
      Self.Watchdog_Pet_Checks_Enabled_History.Init (Depth => 100);
      Self.Watchdog_Pet_Checks_Disabled_History.Init (Depth => 100);
      Self.Watchdog_Limit_Set_History.Init (Depth => 100);
      Self.Watchdog_Action_Set_History.Init (Depth => 100);
      Self.Watchdog_Limit_Change_Index_Out_Of_Range_History.Init (Depth => 100);
      Self.Watchdog_Action_Change_Index_Out_Of_Range_History.Init (Depth => 100);
      Self.Watchdog_Action_Change_Invalid_Transition_To_Fault_History.Init (Depth => 100);
      Self.Component_Exceeded_Pet_Limit_History.Init (Depth => 100);
      Self.Critical_Task_Not_Petting_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Watchdog_Component_Petter_State_History.Init (Depth => 100);
      Self.Pet_Connector_Action_States_History.Init (Depth => 100);
      -- Fault histories:
      Self.Dummy_Fault_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
      procedure Free_Pet_T_Send_Array is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Pet_T_Send_Array,
         Name => Pet_T_Send_Array_Access
      );
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Pet_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Fault_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Watchdog_Pet_Checks_Enabled_History.Destroy;
      Self.Watchdog_Pet_Checks_Disabled_History.Destroy;
      Self.Watchdog_Limit_Set_History.Destroy;
      Self.Watchdog_Action_Set_History.Destroy;
      Self.Watchdog_Limit_Change_Index_Out_Of_Range_History.Destroy;
      Self.Watchdog_Action_Change_Index_Out_Of_Range_History.Destroy;
      Self.Watchdog_Action_Change_Invalid_Transition_To_Fault_History.Destroy;
      Self.Component_Exceeded_Pet_Limit_History.Destroy;
      Self.Critical_Task_Not_Petting_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Watchdog_Component_Petter_State_History.Destroy;
      Self.Pet_Connector_Action_States_History.Destroy;
      -- Fault histories:
      Self.Dummy_Fault_History.Destroy;
      if Self.Connector_Pet_T_Send /= null then
         Free_Pet_T_Send_Array (Self.Connector_Pet_T_Send);
      end if;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Pet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Pet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Fault_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Fault_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
      for Idx in Self.Connector_Pet_T_Send.all'Range loop
         Self.Attach_Pet_T_Send (From_Index => Idx, To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Pet_T_Recv_Sync_Access (Index => Idx), To_Index => Idx);
      end loop;
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The pet send connector. This is used to service a downstream watchdog component, usually a component which services a hardware-based watchdog.
   overriding procedure Pet_T_Recv_Sync (Self : in out Instance; Arg : in Pet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pet_T_Recv_Sync_History.Push (Arg);
   end Pet_T_Recv_Sync;

   -- This connector is used to register the components commands with the command router component.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Faults are sent on this connector.
   overriding procedure Fault_T_Recv_Sync (Self : in out Instance; Arg : in Fault.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Fault_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the fault to the correct handler:
      Self.Dispatch_Fault (Arg);
   end Fault_T_Recv_Sync;

   -- The post mortem log can be dumped using packets.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- Data products for limit values and states.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Indicates a command was received to enable the checks on upstream pets.
   overriding procedure Watchdog_Pet_Checks_Enabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Pet_Checks_Enabled_History.Push (Arg);
   end Watchdog_Pet_Checks_Enabled;

   -- Indicates a command was received to disable the checks on upstream pets.
   overriding procedure Watchdog_Pet_Checks_Disabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Pet_Checks_Disabled_History.Push (Arg);
   end Watchdog_Pet_Checks_Disabled;

   -- An event to indicate that the limit was changed by command for a particular index.
   overriding procedure Watchdog_Limit_Set (Self : in out Instance; Arg : in Watchdog_Limit_Cmd.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Limit_Set_History.Push (Arg);
   end Watchdog_Limit_Set;

   -- An event to indicate that the action was changed by command for a particular index.
   overriding procedure Watchdog_Action_Set (Self : in out Instance; Arg : in Watchdog_Action_Cmd.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Action_Set_History.Push (Arg);
   end Watchdog_Action_Set;

   -- Event indicating there was an error for the index range in the set limit command.
   overriding procedure Watchdog_Limit_Change_Index_Out_Of_Range (Self : in out Instance; Arg : in Packed_Connector_Index.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Limit_Change_Index_Out_Of_Range_History.Push (Arg);
   end Watchdog_Limit_Change_Index_Out_Of_Range;

   -- Event indicating there was an error for the index range in the set limit command.
   overriding procedure Watchdog_Action_Change_Index_Out_Of_Range (Self : in out Instance; Arg : in Packed_Connector_Index.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Action_Change_Index_Out_Of_Range_History.Push (Arg);
   end Watchdog_Action_Change_Index_Out_Of_Range;

   -- Event indicating there was an error trying to set the action to fault. The petter did not have a fault declared in the model so the action cannot be set to fault.
   overriding procedure Watchdog_Action_Change_Invalid_Transition_To_Fault (Self : in out Instance; Arg : in Packed_Connector_Index.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Action_Change_Invalid_Transition_To_Fault_History.Push (Arg);
   end Watchdog_Action_Change_Invalid_Transition_To_Fault;

   -- Event to indicate a pet connector has not received a pet within the set limits for that component.
   overriding procedure Component_Exceeded_Pet_Limit (Self : in out Instance; Arg : in Packed_Connector_Index.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Component_Exceeded_Pet_Limit_History.Push (Arg);
   end Component_Exceeded_Pet_Limit;

   -- Event to indicate that one or more of our critical tasks have not indicated a pet in the maximum limit of ticks. The hardware watchdog will not be pet in this case.
   overriding procedure Critical_Task_Not_Petting (Self : in out Instance; Arg : in Packed_Connector_Index.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Critical_Task_Not_Petting_History.Push (Arg);
   end Critical_Task_Not_Petting;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Task Watchdog component.
   -- Data product that tracks the global state to enable or disable all checks on the upstream watchdog pets.
   overriding procedure Watchdog_Component_Petter_State (Self : in out Instance; Arg : in Packed_Watchdog_Component_State.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Watchdog_Component_Petter_State_History.Push (Arg);
   end Watchdog_Component_Petter_State;

   -- 2-bit of state for each pet connector indicating the current action that will be taken if there is an error. Note that Packed_U32.T is just a placeholder type for this data product. The actual type of this data product will be autocoded and at assembly model ingest time.
   overriding procedure Pet_Connector_Action_States (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pet_Connector_Action_States_History.Push (Arg);
   end Pet_Connector_Action_States;

   -----------------------------------------------
   -- Fault handler primitive:
   -----------------------------------------------
   -- Description:
   --    Faults for the Task Watchdog component
   -- Dummy fault which will be deleted and filled back in from the watchdog list
   overriding procedure Dummy_Fault (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dummy_Fault_History.Push (Arg);
   end Dummy_Fault;

   -----------------------------------------------
   -- Custom functions
   -----------------------------------------------

   overriding procedure Dispatch_Data_Product (Self : in out Instance; Dp : in Data_Product.T) is
      -- Dispatch to dummy no matter what.
      Dispatch_To : constant Dispatch_Data_Product_Procedure := Data_Product_Id_Table (Task_Watchdog_Data_Products.Local_Data_Product_Id_Type'First);
   begin
      Dispatch_To (Component.Task_Watchdog_Reciprocal.Base_Instance (Self), Dp);
   end Dispatch_Data_Product;

   overriding procedure Dispatch_Fault (Self : in out Instance; F : in Fault.T) is
      -- Dispatch to dummy no matter what.
      Dispatch_To : constant Dispatch_Fault_Procedure := Fault_Id_Table (Task_Watchdog_Faults.Local_Fault_Id_Type'First);
   begin
      Dispatch_To (Component.Task_Watchdog_Reciprocal.Base_Instance (Self), F);
   end Dispatch_Fault;

end Component.Task_Watchdog.Implementation.Tester;
