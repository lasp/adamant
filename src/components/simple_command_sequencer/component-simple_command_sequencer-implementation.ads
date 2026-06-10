--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Command_Response;
with Tick;
with Run_Sequence_Arg;
with Packed_U16;
with Interfaces;
with Basic_Types;
with Simple_Sequencer_Types;
with Sequence_Frame;

-- The Command Sequencer component executes predefined sequences of commands.
-- It receives high-level sequence commands and breaks them down into individual
-- sub-commands that are sent to the command router. The sequencer handles
-- command response tracking, timeouts, and failure modes.
package Component.Simple_Command_Sequencer.Implementation is

   -- The component class instance record:
   type Instance is new Simple_Command_Sequencer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Num_Concurrent_Sequences : Interfaces.Unsigned_32 - Denotes the Number of
   -- Sequences that can be running at the same time. Any Run_Sequence commands that
   -- are sent that would increase the number of concurrent sequences beyond this
   -- number will be rejected.
   -- Sequences : Simple_Sequencer_Types.Sequences_Access - Access to statically
   -- defined sequence list.
   --
   overriding procedure Init (Self : in out Instance; Num_Concurrent_Sequences : in Interfaces.Unsigned_32; Sequences : in Simple_Sequencer_Types.Sequences_Access);

private
   type Sequence_Frame_Array is array (Interfaces.Unsigned_32 range <>) of Sequence_Frame.T;
   type Sequence_Frame_Array_Access is access all Sequence_Frame_Array;

   -- The component class instance record:
   type Instance is new Simple_Command_Sequencer.Base_Instance with record
      Sequence_Frames : Sequence_Frame_Array_Access := null;
      Sequences : Simple_Sequencer_Types.Sequences_Access := null;
      Summary_Packet_Period : Interfaces.Unsigned_16 := 0;
      -- Ticks elapsed since the last summary packet emission. Reset on
      -- emission and by Set_Summary_Packet_Period (so a new period starts a
      -- fresh phase).
      Summary_Packet_Tick_Count : Interfaces.Unsigned_16 := 0;
      -- Per-call response-context scratch set by Command_T_Recv_Async and
      -- consumed by the Run_Sequence handler. The active component's serial
      -- queue makes a side-channel through Self safe: only one inbound command
      -- is in dispatch at a time, so Pending_Operator_* is set on entry and
      -- read once before the next message is processed.
      --
      -- Pending_Defer is set by Run_Sequence when it claims a frame with
      -- Send_After_Sequence_Completion; Command_T_Recv_Async then suppresses
      -- the immediate reply and the sequence-completion paths emit it later.
      Pending_Operator_Source_Id : Command_Types.Command_Source_Id := 0;
      Pending_Operator_Command_Id : Command_Types.Command_Id := 0;
      Pending_Defer : Boolean := False;
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
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Sequence commands are received on this connector
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- Responses to sub-commands are received here
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T);
   -- This procedure is called when a Command_Response_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T);
   -- Tick for managing timeouts and delays
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T);
   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Static commands for the Simple Command Sequencer component. Per-sequence
   --    wrapper commands (one per declared sequence) are synthesised at assembly
   --    load time by gen/models/simple_sequencer_commands.py.
   -- Run a command sequence by ID. Per-sequence wrapper commands are the operator-
   -- friendly form; this is the underlying backbone they all dispatch through.
   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E;
   -- Halt every running sequence and return all frames to their initial state. Does
   -- not affect frames that were not running.
   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E;
   -- Set the period of the sequencer summary packet, in ticks. A period of zero
   -- disables the packet.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

   overriding procedure Register_Commands (Self : in out Instance; Arg : in Command_Registration_Request.T);
end Component.Simple_Command_Sequencer.Implementation;
