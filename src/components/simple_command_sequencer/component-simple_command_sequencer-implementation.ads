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
with Sequence_Enums;

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
   -- Config : Simple_Sequencer_Types.Sequencer_Config - The sequencer's static
   -- configuration, exported as the Config constant by the generated command
   -- sequences suite package. Carries the sequence table and the sizes of the
   -- two frame pools.
   --
   overriding procedure Init (Self : in out Instance; Config : in Simple_Sequencer_Types.Sequencer_Config);

private
   -- Response context of the command in dispatch, captured by
   -- Command_T_Recv_Async for Run_Sequence. Safe as a side channel because the
   -- active component's queue dispatches one command at a time.
   --
   -- Run_Sequence sets Defer_Command_Response when the claimed sequence is
   -- Send_After_Sequence_Completion; Command_T_Recv_Async then withholds its
   -- immediate reply and the sequence-end paths send it.
   type Caller_Context is record
      Source_Id : Command_Types.Command_Source_Id := 0;
      Command_Id : Command_Types.Command_Id := 0;
      Defer_Command_Response : Boolean := False;
   end record;

   -- One frame pool. Each pool owns its frames as a separate array, so the
   -- pools cannot overlap and the index subtype bounds every access; the
   -- array is indexed by the pool's global frame ids. An empty pool holds
   -- an empty array.
   type Frame_Pool is record
      Frames : Simple_Sequencer_Types.Sequence_Frame_Array_Access := null;
      -- Index into Frames where the next search for an idle frame starts: one
      -- past the frame Run_Sequence took most recently, so frames are handed
      -- out round robin and the frame released most recently is the last reused.
      Next_Search_Start : Simple_Sequencer_Types.Frame_Id_Type := 0;
   end record;
   type Frame_Pools is array (Sequence_Enums.Frame_Pool.E) of Frame_Pool;

   -- The component class instance record:
   type Instance is new Simple_Command_Sequencer.Base_Instance with record
      Sequences : Simple_Sequencer_Types.Sequences_Access := null;
      -- The two frame pools, laid out by Init: the waiting-for-response pool takes the
      -- first frame ids, the non-waiting-for-response pool the ids after them.
      Pools : Frame_Pools;
      Summary_Packet_Period : Interfaces.Unsigned_16 := 0;
      -- Ticks since the last summary packet. Reset on emission and by
      -- Set_Summary_Packet_Period.
      Summary_Packet_Tick_Count : Interfaces.Unsigned_16 := 0;
      Caller : Caller_Context;
      -- Data product counters. The high water mark is the peak number of
      -- concurrently running frames; the rest are totals since startup.
      Frame_Running_Hwm : Interfaces.Unsigned_16 := 0;
      Sequences_Started_Count : Interfaces.Unsigned_32 := 0;
      Sequences_Finished_Count : Interfaces.Unsigned_32 := 0;
      Sequences_Failed_Count : Interfaces.Unsigned_32 := 0;
      Commands_Sent_Count : Interfaces.Unsigned_32 := 0;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Sends out the initial values of all data products.
   overriding procedure Set_Up (Self : in out Instance);

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
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Static commands for the Simple Command Sequencer component. Per-sequence
   --    commands (one per declared sequence) are synthesised at assembly
   --    load time by gen/models/simple_command_sequencer_commands.py.
   -- Run a command sequence by ID. The synthesised per-sequence commands are the
   -- operator-friendly form; this is the backbone they all dispatch through.
   -- Response behavior is the sequence's static configuration from the
   -- sequences model.
   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E;
   -- Halt every running sequence and return all frames to their initial state. Does
   -- not affect frames that were not running.
   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E;
   -- Halt the sequence running on a single frame and return that frame to its
   -- initial state. Fails if the frame ID is out of range; killing a frame
   -- that is not running has no effect and succeeds.
   overriding function Kill_Frame (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;
   -- Set the period of the sequencer summary packet, in ticks. A period of zero
   -- disables the packet.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

   overriding procedure Register_Commands (Self : in out Instance; Arg : in Command_Registration_Request.T);
end Component.Simple_Command_Sequencer.Implementation;
