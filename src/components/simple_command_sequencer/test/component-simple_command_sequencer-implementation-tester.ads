--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Simple_Command_Sequencer_Reciprocal;
with Printable_History;
with Command.Representation;
with Command_Response.Representation;
with Packet.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Sequence_Event_Info.Representation;
with Sequence_Step_Event_Info.Representation;
with Sequence_Sleep_Event_Info.Representation;
with Sequence_Step_Command_Event_Info.Representation;
with Packed_U32.Representation;
with Packed_U16.Representation;
with Command_Header.Representation;
with Tick.Representation;
with Invalid_Command_Info.Representation;
with Data_Product;
with Data_Product.Representation;

-- The Command Sequencer component executes predefined sequences of commands.
-- It receives high-level sequence commands and breaks them down into individual
-- sub-commands that are sent to the command router. The sequencer handles
-- command response tracking, timeouts, and failure modes.
package Component.Simple_Command_Sequencer.Implementation.Tester is

   use Component.Simple_Command_Sequencer_Reciprocal;
   -- Invoker connector history packages:
   package Command_T_Recv_Sync_History_Package is new Printable_History (Command.T, Command.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Sequence_Started_History_Package is new Printable_History (Sequence_Event_Info.T, Sequence_Event_Info.Representation.Image);
   package Sequence_Completed_History_Package is new Printable_History (Sequence_Event_Info.T, Sequence_Event_Info.Representation.Image);
   package Sequence_Aborted_History_Package is new Printable_History (Sequence_Step_Event_Info.T, Sequence_Step_Event_Info.Representation.Image);
   package Sequence_Timeout_History_Package is new Printable_History (Sequence_Step_Event_Info.T, Sequence_Step_Event_Info.Representation.Image);
   package Sequence_Out_Of_Range_Sleep_History_Package is new Printable_History (Sequence_Sleep_Event_Info.T, Sequence_Sleep_Event_Info.Representation.Image);
   package Sequence_Out_Of_Range_Timeout_History_Package is new Printable_History (Sequence_Step_Event_Info.T, Sequence_Step_Event_Info.Representation.Image);
   package Command_Failure_History_Package is new Printable_History (Sequence_Step_Command_Event_Info.T, Sequence_Step_Command_Event_Info.Representation.Image);
   package Invalid_Sequence_Id_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Unexpected_Register_Source_History_Package is new Printable_History (Natural, Natural'Image);
   package No_Frame_Available_History_Package is new Printable_History (Natural, Natural'Image);
   package Dropped_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Dropped_Command_Response_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Dropped_Tick_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Unexpected_Command_Response_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Killed_All_Sequences_History_Package is new Printable_History (Natural, Natural'Image);
   package Killed_Frame_History_Package is new Printable_History (Sequence_Event_Info.T, Sequence_Event_Info.Representation.Image);
   package Invalid_Frame_Id_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Invalid_Dynamic_Command_Argument_History_Package is new Printable_History (Sequence_Step_Command_Event_Info.T, Sequence_Step_Command_Event_Info.Representation.Image);
   package Frame_Not_Running_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Summary_Packet_Period_Set_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Invalid_Dynamic_Sleep_Argument_History_Package is new Printable_History (Sequence_Step_Event_Info.T, Sequence_Step_Event_Info.Representation.Image);

   -- Data product history packages:
   package Frame_Running_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Frame_Running_High_Water_Mark_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Sequences_Started_Count_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Sequences_Finished_Count_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Sequences_Failed_Count_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Commands_Sent_Count_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Last_Sequence_Started_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Last_Sequence_Finished_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Last_Sequence_Failed_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Summary_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Simple_Command_Sequencer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Simple_Command_Sequencer.Implementation.Instance;
      -- Connector histories:
      Command_T_Recv_Sync_History : Command_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Sequence_Started_History : Sequence_Started_History_Package.Instance;
      Sequence_Completed_History : Sequence_Completed_History_Package.Instance;
      Sequence_Aborted_History : Sequence_Aborted_History_Package.Instance;
      Sequence_Timeout_History : Sequence_Timeout_History_Package.Instance;
      Sequence_Out_Of_Range_Sleep_History : Sequence_Out_Of_Range_Sleep_History_Package.Instance;
      Sequence_Out_Of_Range_Timeout_History : Sequence_Out_Of_Range_Timeout_History_Package.Instance;
      Command_Failure_History : Command_Failure_History_Package.Instance;
      Invalid_Sequence_Id_History : Invalid_Sequence_Id_History_Package.Instance;
      Unexpected_Register_Source_History : Unexpected_Register_Source_History_Package.Instance;
      No_Frame_Available_History : No_Frame_Available_History_Package.Instance;
      Dropped_Command_History : Dropped_Command_History_Package.Instance;
      Dropped_Command_Response_History : Dropped_Command_Response_History_Package.Instance;
      Dropped_Tick_History : Dropped_Tick_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Unexpected_Command_Response_History : Unexpected_Command_Response_History_Package.Instance;
      Killed_All_Sequences_History : Killed_All_Sequences_History_Package.Instance;
      Killed_Frame_History : Killed_Frame_History_Package.Instance;
      Invalid_Frame_Id_History : Invalid_Frame_Id_History_Package.Instance;
      Invalid_Dynamic_Command_Argument_History : Invalid_Dynamic_Command_Argument_History_Package.Instance;
      Frame_Not_Running_History : Frame_Not_Running_History_Package.Instance;
      Summary_Packet_Period_Set_History : Summary_Packet_Period_Set_History_Package.Instance;
      Invalid_Dynamic_Sleep_Argument_History : Invalid_Dynamic_Sleep_Argument_History_Package.Instance;
      -- Data product histories:
      Frame_Running_Count_History : Frame_Running_Count_History_Package.Instance;
      Frame_Running_High_Water_Mark_History : Frame_Running_High_Water_Mark_History_Package.Instance;
      Sequences_Started_Count_History : Sequences_Started_Count_History_Package.Instance;
      Sequences_Finished_Count_History : Sequences_Finished_Count_History_Package.Instance;
      Sequences_Failed_Count_History : Sequences_Failed_Count_History_Package.Instance;
      Commands_Sent_Count_History : Commands_Sent_Count_History_Package.Instance;
      Last_Sequence_Started_History : Last_Sequence_Started_History_Package.Instance;
      Last_Sequence_Finished_History : Last_Sequence_Finished_History_Package.Instance;
      Last_Sequence_Failed_History : Last_Sequence_Failed_History_Package.Instance;
      -- Packet histories:
      Summary_Packet_History : Summary_Packet_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Command_Response_T_Send_Dropped : Boolean := False;
      Command_Response_T_Send_Dropped_Count : Natural := 0;
      Expect_Tick_T_Send_Dropped : Boolean := False;
      Tick_T_Send_Dropped_Count : Natural := 0;
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
   -- Sub-commands are sent out this connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- Sends the response to a Run_Sequence (or synthesised per-sequence) command
   -- back to the command router. The Send_After_Sequence_Completion path emits its
   -- deferred reply here on completion / abort / timeout / kill; the default
   -- Send_After_Sequence_Start path emits its reply here immediately at dispatch
   -- time.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The packet send connector, used for sending the periodic sequencer summary
   -- packet.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- Events are sent out of this connector
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T);

   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Simple Command Sequencer component
   -- A sequence has begun execution on a frame
   overriding procedure Sequence_Started (Self : in out Instance; Arg : in Sequence_Event_Info.T);
   -- A sequence has completed all steps successfully
   overriding procedure Sequence_Completed (Self : in out Instance; Arg : in Sequence_Event_Info.T);
   -- A sequence was aborted due to a command failure
   overriding procedure Sequence_Aborted (Self : in out Instance; Arg : in Sequence_Step_Event_Info.T);
   -- A sequence timed out while waiting for a command response
   overriding procedure Sequence_Timeout (Self : in out Instance; Arg : in Sequence_Step_Event_Info.T);
   -- A sequence has entered sleep state with either overflow or underflow.
   overriding procedure Sequence_Out_Of_Range_Sleep (Self : in out Instance; Arg : in Sequence_Sleep_Event_Info.T);
   -- Computing a sequence's command response deadline overflowed the system time
   -- representation.
   overriding procedure Sequence_Out_Of_Range_Timeout (Self : in out Instance; Arg : in Sequence_Step_Event_Info.T);
   -- A command sent by a sequence received a failure response
   overriding procedure Command_Failure (Self : in out Instance; Arg : in Sequence_Step_Command_Event_Info.T);
   -- A Run_Sequence command was received with an out of range sequence ID
   overriding procedure Invalid_Sequence_Id (Self : in out Instance; Arg : in Packed_U16.T);
   -- A Register_Source command response was received but all sequence frames
   -- already have a source ID.
   overriding procedure Unexpected_Register_Source (Self : in out Instance);
   -- A Run_Sequence command was received but all frames are in use
   overriding procedure No_Frame_Available (Self : in out Instance);
   -- A command was dropped due to a full queue
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T);
   -- A command response was dropped due to a full queue
   overriding procedure Dropped_Command_Response (Self : in out Instance; Arg : in Command_Response.T);
   -- A tick was dropped due to a full queue
   overriding procedure Dropped_Tick (Self : in out Instance; Arg : in Tick.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command response was received with a source ID that does not belong to any
   -- active sequence frame.
   overriding procedure Unexpected_Command_Response (Self : in out Instance; Arg : in Command_Response.T);
   -- A Kill_All_Sequences command was executed and all running sequences were
   -- halted.
   overriding procedure Killed_All_Sequences (Self : in out Instance);
   -- A Kill_Frame command was executed and the running sequence on the frame was
   -- halted.
   overriding procedure Killed_Frame (Self : in out Instance; Arg : in Sequence_Event_Info.T);
   -- A Kill_Frame command was received with an out of range frame ID.
   overriding procedure Invalid_Frame_Id (Self : in out Instance; Arg : in Packed_U32.T);
   -- A Command with a Dynamic Argument cannot be executed as the Argument is Invalid
   overriding procedure Invalid_Dynamic_Command_Argument (Self : in out Instance; Arg : in Sequence_Step_Command_Event_Info.T);
   -- A Kill_Frame command targeted a frame that was not running, so there was
   -- nothing to kill.
   overriding procedure Frame_Not_Running (Self : in out Instance; Arg : in Packed_U32.T);
   -- The summary packet period was set to a new value, in ticks.
   overriding procedure Summary_Packet_Period_Set (Self : in out Instance; Arg : in Packed_U16.T);
   -- A dynamic sleep step could not resolve its duration because the sequence's
   -- argument failed validation.
   overriding procedure Invalid_Dynamic_Sleep_Argument (Self : in out Instance; Arg : in Sequence_Step_Event_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Simple Command Sequencer component.
   -- The number of sequence frames currently running (not idle).
   overriding procedure Frame_Running_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The maximum number of concurrently running sequence frames observed since
   -- startup.
   overriding procedure Frame_Running_High_Water_Mark (Self : in out Instance; Arg : in Packed_U16.T);
   -- The total number of sequences started since startup.
   overriding procedure Sequences_Started_Count (Self : in out Instance; Arg : in Packed_U32.T);
   -- The total number of sequences that ran to completion successfully since
   -- startup.
   overriding procedure Sequences_Finished_Count (Self : in out Instance; Arg : in Packed_U32.T);
   -- The total number of sequences that ended in failure since startup.
   overriding procedure Sequences_Failed_Count (Self : in out Instance; Arg : in Packed_U32.T);
   -- The total number of sub-commands the sequencer has dispatched since startup.
   overriding procedure Commands_Sent_Count (Self : in out Instance; Arg : in Packed_U32.T);
   -- The sequence id of the most recently started sequence.
   overriding procedure Last_Sequence_Started (Self : in out Instance; Arg : in Packed_U16.T);
   -- The sequence id of the most recent sequence to run to completion successfully.
   overriding procedure Last_Sequence_Finished (Self : in out Instance; Arg : in Packed_U16.T);
   -- The sequence id of the most recent sequence to end in failure.
   overriding procedure Last_Sequence_Failed (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Simple Command Sequencer component.
   -- Periodic summary of all sequence frames. Contains one Sequence_Frame_Summary
   -- record per frame (Num_Concurrent_Sequences total, in frame order), reporting
   -- which sequence is running on each frame, its current step, its execution state,
   -- and whether an operator is still waiting on a deferred command response.
   -- Emitted every Summary_Packet_Period ticks; a period of zero (the default)
   -- disables emission. The period is set with the Set_Summary_Packet_Period
   -- command.
   overriding procedure Summary_Packet (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Simple_Command_Sequencer.Implementation.Tester;
