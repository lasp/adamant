--------------------------------------------------------------------------------
-- Memory_Copier Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Memory_Copier.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Memory_Region_Copy_T_Recv_Sync_History.Init (Depth => 100);
      Self.Memory_Region_Request_T_Return_History.Init (Depth => 100);
      Self.Ided_Memory_Region_Release_Reciprocal_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Memory_Region_Length_Mismatch_History.Init (Depth => 100);
      Self.Memory_Region_Unavailable_History.Init (Depth => 100);
      Self.Starting_Copy_History.Init (Depth => 100);
      Self.Finished_Copy_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Copy_Timeout_History.Init (Depth => 100);
      Self.Copy_Failure_History.Init (Depth => 100);
      Self.Command_Dropped_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Memory_Region_Copy_T_Recv_Sync_History.Destroy;
      Self.Memory_Region_Request_T_Return_History.Destroy;
      Self.Ided_Memory_Region_Release_Reciprocal_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Memory_Region_Length_Mismatch_History.Destroy;
      Self.Memory_Region_Unavailable_History.Destroy;
      Self.Starting_Copy_History.Destroy;
      Self.Finished_Copy_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Copy_Timeout_History.Destroy;
      Self.Copy_Failure_History.Destroy;
      Self.Command_Dropped_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Memory_Region_Copy_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Memory_Region_Copy_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Memory_Region_Request_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Memory_Region_Request_T_Return_Access);
      Self.Component_Instance.Attach_Ided_Memory_Region_Release (To_Component => Self'Unchecked_Access, Hook => Self.Ided_Memory_Region_Release_Reciprocal_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Timeout_Tick_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Timeout_Tick_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Memory_Region_Release_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Memory_Region_Release_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send the command response back to the command router.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- A memory region is sent on this connector for copy.
   overriding procedure Memory_Region_Copy_T_Recv_Sync (Self : in out Instance; Arg : in Memory_Region_Copy.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Copy_T_Recv_Sync_History.Push (Arg);
   end Memory_Region_Copy_T_Recv_Sync;

   -- The scratch memory region is requested on this connector.
   overriding function Memory_Region_Request_T_Return (Self : in out Instance) return Memory_Region_Request.T is
      To_Return : constant Memory_Region_Request.T := (Ided_Region => (Id => 0, Region => (Address => Self.Scratch'Address, Length => Self.Scratch'Length)), Status => Self.Scratch_Return_Status);
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Request_T_Return_History.Push (To_Return);
      return To_Return;
   end Memory_Region_Request_T_Return;

   -- The memory region is released (returned) to scratch on this connector.
   overriding procedure Ided_Memory_Region_Release_Reciprocal (Self : in out Instance; Arg : in Ided_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ided_Memory_Region_Release_Reciprocal_History.Push (Arg);
   end Ided_Memory_Region_Release_Reciprocal;

   -- The event send connector
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
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

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Memory Copier component.
   -- A memory region was received with a length less than that specified in the copy command. The length of the region requested must be the same size or greater than the length specified in the copy command.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Memory_Region_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Length_Mismatch_History.Push (Arg);
   end Memory_Region_Length_Mismatch;

   -- Requesting the memory region was denied because it is currently in use by another component.
   overriding procedure Memory_Region_Unavailable (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Unavailable_History.Push (Arg);
   end Memory_Region_Unavailable;

   -- Starting copy from source to destination.
   overriding procedure Starting_Copy (Self : in out Instance; Arg : in Virtual_Memory_Region_Copy.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Starting_Copy_History.Push (Arg);
   end Starting_Copy;

   -- Finished copy from source to destination, without errors.
   overriding procedure Finished_Copy (Self : in out Instance; Arg : in Virtual_Memory_Region_Copy.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Finished_Copy_History.Push (Arg);
   end Finished_Copy;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A timeout occurred while waiting for a copy operation to complete.
   overriding procedure Copy_Timeout (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Copy_Timeout_History.Push (Arg);
   end Copy_Timeout;

   -- A copy failed.
   overriding procedure Copy_Failure (Self : in out Instance; Arg : in Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Copy_Failure_History.Push (Arg);
   end Copy_Failure;

   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Dropped_History.Push (Arg);
   end Command_Dropped;

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

end Component.Memory_Copier.Implementation.Tester;
