--------------------------------------------------------------------------------
-- Memory_Stuffer Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Memory_Stuffer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Memory_Region_Release_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Invalid_Memory_Region_History.Init (Depth => 100);
      Self.Invalid_Copy_Destination_History.Init (Depth => 100);
      Self.Protected_Write_Enabled_History.Init (Depth => 100);
      Self.Protected_Write_Disabled_History.Init (Depth => 100);
      Self.Writing_Memory_History.Init (Depth => 100);
      Self.Memory_Written_History.Init (Depth => 100);
      Self.Copying_Memory_History.Init (Depth => 100);
      Self.Memory_Copied_History.Init (Depth => 100);
      Self.Protected_Write_Denied_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Protected_Write_Disabled_Timeout_History.Init (Depth => 100);
      -- Data product histories:
      Self.Armed_State_History.Init (Depth => 100);
      Self.Armed_State_Timeout_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Memory_Region_Release_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Invalid_Memory_Region_History.Destroy;
      Self.Invalid_Copy_Destination_History.Destroy;
      Self.Protected_Write_Enabled_History.Destroy;
      Self.Protected_Write_Disabled_History.Destroy;
      Self.Writing_Memory_History.Destroy;
      Self.Memory_Written_History.Destroy;
      Self.Copying_Memory_History.Destroy;
      Self.Memory_Copied_History.Destroy;
      Self.Protected_Write_Denied_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Protected_Write_Disabled_Timeout_History.Destroy;
      -- Data product histories:
      Self.Armed_State_History.Destroy;
      Self.Armed_State_Timeout_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Memory_Region_Release_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Memory_Region_Release_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Async_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Memory_Region_Copy_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Memory_Region_Copy_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to release the received memory region after a copy has occurred.
   overriding procedure Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Release_T_Recv_Sync_History.Push (Arg);
   end Memory_Region_Release_T_Recv_Sync;

   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      if not Self.Expect_Tick_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Tick_T_Send was called!");
      else
         Self.Tick_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Tick_T_Send_Dropped := False;
      end if;
   end Tick_T_Send_Dropped;

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      if not Self.Expect_Command_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_T_Send was called!");
      else
         Self.Command_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_T_Send_Dropped := False;
      end if;
   end Command_T_Send_Dropped;

   -- This procedure is called when a Memory_Region_Copy_T_Send message is dropped due to a full queue.
   overriding procedure Memory_Region_Copy_T_Send_Dropped (Self : in out Instance; Arg : in Memory_Region_Copy.T) is
      Ignore : Memory_Region_Copy.T renames Arg;
   begin
      if not Self.Expect_Memory_Region_Copy_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Memory_Region_Copy_T_Send was called!");
      else
         Self.Memory_Region_Copy_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Memory_Region_Copy_T_Send_Dropped := False;
      end if;
   end Memory_Region_Copy_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was sent to access a memory region with an invalid address and/or length.
   overriding procedure Invalid_Memory_Region (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Memory_Region_History.Push (Arg);
   end Invalid_Memory_Region;

   -- A copy request was received with an invalid destination address and length.
   overriding procedure Invalid_Copy_Destination (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Copy_Destination_History.Push (Arg);
   end Invalid_Copy_Destination;

   -- An arm command was received and the protected write state is enabled.
   overriding procedure Protected_Write_Enabled (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Protected_Write_Enabled_History.Push (Arg);
   end Protected_Write_Enabled;

   -- The protected write state was disabled either by timeout or receiving a subsequent command.
   overriding procedure Protected_Write_Disabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Protected_Write_Disabled_History.Push (Arg);
   end Protected_Write_Disabled;

   -- The component is currently writing the memory location for the following region.
   overriding procedure Writing_Memory (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Writing_Memory_History.Push (Arg);
   end Writing_Memory;

   -- The component has finished writing the memory location for the following region.
   overriding procedure Memory_Written (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Written_History.Push (Arg);
   end Memory_Written;

   -- The component is currently copying memory from one address to another.
   overriding procedure Copying_Memory (Self : in out Instance; Arg : in Memory_Region_Copy.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Copying_Memory_History.Push (Arg);
   end Copying_Memory;

   -- The component has finished copying memory from one address to another.
   overriding procedure Memory_Copied (Self : in out Instance; Arg : in Memory_Region_Copy.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Copied_History.Push (Arg);
   end Memory_Copied;

   -- A command was received to write to a protected region, but the component was not armed so the command is being rejected.
   overriding procedure Protected_Write_Denied (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Protected_Write_Denied_History.Push (Arg);
   end Protected_Write_Denied;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- The component armed state timed out and is now unarmed.
   overriding procedure Protected_Write_Disabled_Timeout (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Protected_Write_Disabled_Timeout_History.Push (Arg);
   end Protected_Write_Disabled_Timeout;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Memory Stuffer component.
   -- The current armed/unarmed state of the component.
   overriding procedure Armed_State (Self : in out Instance; Arg : in Packed_Arm_State.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Armed_State_History.Push (Arg);
   end Armed_State;

   -- The time remaining (in ticks) until the armed state expires.
   overriding procedure Armed_State_Timeout (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Armed_State_Timeout_History.Push (Arg);
   end Armed_State_Timeout;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

end Component.Memory_Stuffer.Implementation.Tester;
