--------------------------------------------------------------------------------
-- Memory_Stuffer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Memory_Stuffer_Reciprocal;
with Sys_Time;
with Printable_History;
with Memory_Region_Release.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Memory_Region.Representation;
with Packed_Arm_Timeout.Representation;
with Memory_Region_Copy.Representation;
with Invalid_Command_Info.Representation;
with Data_Product;
with Packed_Arm_State.Representation;

-- The memory stuffer component is an active component that can stuff (write to) memory regions. It reports an error if an action is requested on a memory region outside of the address space that it is configured with during initialization. The component can manage both protected memory regions (which require an arm command prior to stuffing) and unprotected regions (which require no arm prior to stuffing). In addition, the component has a connector to accept a memory region copy request, which will stuff memory with data from another system address. The memory region copy and release connectors may be disconnected if this feature is not needed.
package Component.Memory_Stuffer.Implementation.Tester is

   use Component.Memory_Stuffer_Reciprocal;
   -- Invoker connector history packages:
   package Memory_Region_Release_T_Recv_Sync_History_Package is new Printable_History (Memory_Region_Release.T, Memory_Region_Release.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Invalid_Memory_Region_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Invalid_Copy_Destination_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Protected_Write_Enabled_History_Package is new Printable_History (Packed_Arm_Timeout.T, Packed_Arm_Timeout.Representation.Image);
   package Protected_Write_Disabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Writing_Memory_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Memory_Written_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Copying_Memory_History_Package is new Printable_History (Memory_Region_Copy.T, Memory_Region_Copy.Representation.Image);
   package Memory_Copied_History_Package is new Printable_History (Memory_Region_Copy.T, Memory_Region_Copy.Representation.Image);
   package Protected_Write_Denied_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Protected_Write_Disabled_Timeout_History_Package is new Printable_History (Natural, Natural'Image);

   -- Data product history packages:
   package Armed_State_History_Package is new Printable_History (Packed_Arm_State.T, Packed_Arm_State.Representation.Image);
   package Armed_State_Timeout_History_Package is new Printable_History (Packed_Arm_Timeout.T, Packed_Arm_Timeout.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Memory_Stuffer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Memory_Stuffer.Implementation.Instance;
      -- Connector histories:
      Memory_Region_Release_T_Recv_Sync_History : Memory_Region_Release_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Invalid_Memory_Region_History : Invalid_Memory_Region_History_Package.Instance;
      Invalid_Copy_Destination_History : Invalid_Copy_Destination_History_Package.Instance;
      Protected_Write_Enabled_History : Protected_Write_Enabled_History_Package.Instance;
      Protected_Write_Disabled_History : Protected_Write_Disabled_History_Package.Instance;
      Writing_Memory_History : Writing_Memory_History_Package.Instance;
      Memory_Written_History : Memory_Written_History_Package.Instance;
      Copying_Memory_History : Copying_Memory_History_Package.Instance;
      Memory_Copied_History : Memory_Copied_History_Package.Instance;
      Protected_Write_Denied_History : Protected_Write_Denied_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Protected_Write_Disabled_Timeout_History : Protected_Write_Disabled_Timeout_History_Package.Instance;
      -- Data product histories:
      Armed_State_History : Armed_State_History_Package.Instance;
      Armed_State_Timeout_History : Armed_State_Timeout_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Tick_T_Send_Dropped : Boolean := False;
      Tick_T_Send_Dropped_Count : Natural := 0;
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Memory_Region_Copy_T_Send_Dropped : Boolean := False;
      Memory_Region_Copy_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to release the received memory region after a copy has occurred.
   overriding procedure Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Memory_Region_Release.T);
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T);

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Memory_Region_Copy_T_Send message is dropped due to a full queue.
   overriding procedure Memory_Region_Copy_T_Send_Dropped (Self : in out Instance; Arg : in Memory_Region_Copy.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was sent to access a memory region with an invalid address and/or length.
   overriding procedure Invalid_Memory_Region (Self : in out Instance; Arg : in Memory_Region.T);
   -- A copy request was received with an invalid destination address and length.
   overriding procedure Invalid_Copy_Destination (Self : in out Instance; Arg : in Memory_Region.T);
   -- An arm command was received and the protected write state is enabled.
   overriding procedure Protected_Write_Enabled (Self : in out Instance; Arg : in Packed_Arm_Timeout.T);
   -- The protected write state was disabled either by timeout or receiving a subsequent command.
   overriding procedure Protected_Write_Disabled (Self : in out Instance);
   -- The component is currently writing the memory location for the following region.
   overriding procedure Writing_Memory (Self : in out Instance; Arg : in Memory_Region.T);
   -- The component has finished writing the memory location for the following region.
   overriding procedure Memory_Written (Self : in out Instance; Arg : in Memory_Region.T);
   -- The component is currently copying memory from one address to another.
   overriding procedure Copying_Memory (Self : in out Instance; Arg : in Memory_Region_Copy.T);
   -- The component has finished copying memory from one address to another.
   overriding procedure Memory_Copied (Self : in out Instance; Arg : in Memory_Region_Copy.T);
   -- A command was received to write to a protected region, but the component was not armed so the command is being rejected.
   overriding procedure Protected_Write_Denied (Self : in out Instance; Arg : in Memory_Region.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- The component armed state timed out and is now unarmed.
   overriding procedure Protected_Write_Disabled_Timeout (Self : in out Instance);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Memory Stuffer component.
   -- The current armed/unarmed state of the component.
   overriding procedure Armed_State (Self : in out Instance; Arg : in Packed_Arm_State.T);
   -- The time remaining (in ticks) until the armed state expires.
   overriding procedure Armed_State_Timeout (Self : in out Instance; Arg : in Packed_Arm_Timeout.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Memory_Stuffer.Implementation.Tester;
