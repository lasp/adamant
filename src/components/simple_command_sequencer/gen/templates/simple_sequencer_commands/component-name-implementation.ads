--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Implementation Spec
--------------------------------------------------------------------------------
-- Auto-generated. Do not edit by hand. The real behaviour lives in the
-- hand-written child package Component.Simple_Command_Sequencer.Implementation.Logic;
-- this body just forwards to it and adds the per-sequence wrapper command
-- handlers synthesised for this build's command sequences.

-- Includes:
with Command;
with Command_Response;
with Tick;
with Sequence_Frame;
with Run_Sequence_Arg;
with Packed_U16;
{% for w in sequence_wrappers %}
with {{ w.wrapped_package }};
{% endfor %}

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
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T);
   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T);
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T);
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is null;
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E;
   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E;
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;
{% for w in sequence_wrappers %}

   -- Synthesized wrapper for the '{{ w.name }}' command sequence.
   overriding function {{ w.name }} (Self : in out Instance; Arg : in {{ w.wrapped_package }}.T) return Command_Execution_Status.E;
{% endfor %}

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Simple_Command_Sequencer.Implementation;
