--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Tester Body
--------------------------------------------------------------------------------

-- Includes:
with String_Util;

package body Component.Simple_Command_Sequencer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Sequence_Started_History.Init (Depth => 100);
      Self.Sequence_Completed_History.Init (Depth => 100);
      Self.Sequence_Aborted_History.Init (Depth => 100);
      Self.Sequence_Timeout_History.Init (Depth => 100);
      Self.Sequence_Out_Of_Range_Sleep_History.Init (Depth => 100);
      Self.Sequence_Out_Of_Range_Timeout_History.Init (Depth => 100);
      Self.Command_Failure_History.Init (Depth => 100);
      Self.Invalid_Sequence_Id_History.Init (Depth => 100);
      Self.Extra_Sequence_Id_History.Init (Depth => 100);
      Self.No_Frame_Available_History.Init (Depth => 100);
      Self.Dropped_Command_History.Init (Depth => 100);
      Self.Dropped_Command_Response_History.Init (Depth => 100);
      Self.Dropped_Tick_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Unexpected_Command_Response_History.Init (Depth => 100);
      Self.Killed_All_Sequences_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Sequence_Started_History.Destroy;
      Self.Sequence_Completed_History.Destroy;
      Self.Sequence_Aborted_History.Destroy;
      Self.Sequence_Timeout_History.Destroy;
      Self.Sequence_Out_Of_Range_Sleep_History.Destroy;
      Self.Sequence_Out_Of_Range_Timeout_History.Destroy;
      Self.Command_Failure_History.Destroy;
      Self.Invalid_Sequence_Id_History.Destroy;
      Self.Extra_Sequence_Id_History.Destroy;
      Self.No_Frame_Available_History.Destroy;
      Self.Dropped_Command_History.Destroy;
      Self.Dropped_Command_Response_History.Destroy;
      Self.Dropped_Tick_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Unexpected_Command_Response_History.Destroy;
      Self.Killed_All_Sequences_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Command_Response_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_Response_T_Recv_Async_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Sub-commands are sent out this connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_T_Recv_Sync_History.Push (Arg);
   end Command_T_Recv_Sync;

   -- Events are sent out of this connector
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

   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
      Ignore : Command_Response.T renames Arg;
   begin
      if not Self.Expect_Command_Response_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_Response_T_Send was called!");
      else
         Self.Command_Response_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_Response_T_Send_Dropped := False;
      end if;
   end Command_Response_T_Send_Dropped;

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

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Simple Command Sequencer component
   -- A sequence has begun execution on a frame
   overriding procedure Sequence_Started (Self : in out Instance; Arg : in Sequence_Event_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Started_History.Push (Arg);
   end Sequence_Started;

   -- A sequence has completed all steps successfully
   overriding procedure Sequence_Completed (Self : in out Instance; Arg : in Sequence_Event_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Completed_History.Push (Arg);
   end Sequence_Completed;

   -- A sequence was aborted due to a command failure
   overriding procedure Sequence_Aborted (Self : in out Instance; Arg : in Sequence_Step_Event_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Aborted_History.Push (Arg);
   end Sequence_Aborted;

   -- A sequence timed out while waiting for a command response
   overriding procedure Sequence_Timeout (Self : in out Instance; Arg : in Sequence_Step_Event_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Timeout_History.Push (Arg);
   end Sequence_Timeout;

   -- A sequence has entered sleep state with either overflow or underflow.
   overriding procedure Sequence_Out_Of_Range_Sleep (Self : in out Instance; Arg : in Sequence_Sleep_Event_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Out_Of_Range_Sleep_History.Push (Arg);
   end Sequence_Out_Of_Range_Sleep;

   -- A sequence timeout duration is out of range either underflow or overflow.
   overriding procedure Sequence_Out_Of_Range_Timeout (Self : in out Instance; Arg : in Sequence_Timeout_Event_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Out_Of_Range_Timeout_History.Push (Arg);
   end Sequence_Out_Of_Range_Timeout;

   -- A command sent by a sequence received a failure response
   overriding procedure Command_Failure (Self : in out Instance; Arg : in Sequence_Step_Command_Event_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Failure_History.Push (Arg);
   end Command_Failure;

   -- A Run_Sequence command was received with an out of range sequence ID
   overriding procedure Invalid_Sequence_Id (Self : in out Instance; Arg : in Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Sequence_Id_History.Push (Arg);
   end Invalid_Sequence_Id;

   -- Too many sequence IDs were passed to the Simple Command Sequencer
   overriding procedure Extra_Sequence_Id (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Extra_Sequence_Id_History.Push (Arg);
   end Extra_Sequence_Id;

   -- A Run_Sequence command was received but all frames are in use
   overriding procedure No_Frame_Available (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.No_Frame_Available_History.Push (Arg);
   end No_Frame_Available;

   -- A command was dropped due to a full queue
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Command_History.Push (Arg);
   end Dropped_Command;

   -- A command response was dropped due to a full queue
   overriding procedure Dropped_Command_Response (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Command_Response_History.Push (Arg);
   end Dropped_Command_Response;

   -- A tick was dropped due to a full queue
   overriding procedure Dropped_Tick (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Tick_History.Push (Arg);
   end Dropped_Tick;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command response was received with an unrecognized source ID.
   overriding procedure Unexpected_Command_Response (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      Self.Unexpected_Command_Response_History.Push (Arg);
   end Unexpected_Command_Response;

   -- A Kill_All_Sequences command was executed and all running sequences were halted.
   overriding procedure Killed_All_Sequences (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      Self.Killed_All_Sequences_History.Push (Arg);
   end Killed_All_Sequences;

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

end Component.Simple_Command_Sequencer.Implementation.Tester;
