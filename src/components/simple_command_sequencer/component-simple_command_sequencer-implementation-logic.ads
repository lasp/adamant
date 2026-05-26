--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Implementation Logic
--------------------------------------------------------------------------------
-- Hand-written home of the component's real behaviour. The generated
-- implementation body (component-simple_command_sequencer-implementation.adb)
-- is a thin shell: each connector/command primitive forwards to the matching
-- routine here, and the autocoded per-sequence wrapper commands are emitted
-- inline. Keeping the logic in this child package (rather than in the generated
-- body) means it stays plain, compilable Ada -- and, being a child of the
-- Implementation package, it retains full visibility of Base_Instance's private
-- connector helpers (Self.Sys_Time_T_Get, Self.Command_T_Send, ...).
with Command;
with Command_Response;
with Tick;
with Run_Sequence_Arg;
with Packed_U16;
with Interfaces;
with Basic_Types;
with Simple_Sequencer_Types;
-- Command_Execution_Status (in Command_Enums) is already use-visible here via
-- the parent component package.

package Component.Simple_Command_Sequencer.Implementation.Logic is

   procedure Init (Self : in out Instance; Num_Concurrent_Sequences : in Interfaces.Unsigned_32; Sequences : in Simple_Sequencer_Types.Sequences_Access);

   procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T);
   procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T);
   procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T);
   procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T);

   function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E;
   function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E;
   function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Interfaces.Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Simple_Command_Sequencer.Implementation.Logic;
