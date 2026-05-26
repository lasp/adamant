--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Command_Response;
with Tick;
with Sequence_Frame;
with Run_Sequence_Arg;
with Packed_U16;

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
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, ie. command registration, initial
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
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Register Stuffer component.
   -- Run a Command Sequence
   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E;

   -- Halt all running sequences and reset their frames.
   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E;

   -- Set the period of the sequencer summary packet. Wiring of the packet itself
   -- comes in Phase 4; for now this stores the value as a placeholder.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Simple_Command_Sequencer.Implementation;
