--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Component Implementation Body
--------------------------------------------------------------------------------
-- Auto-generated. Do not edit by hand. Every connector/command primitive
-- forwards to the hand-written Implementation.Logic child; the per-sequence
-- wrapper handlers below are synthesised from this build's command sequences and
-- dispatch through the generic Run_Sequence backbone.
with Component.Simple_Command_Sequencer.Implementation.Logic;
{% for w in sequence_wrappers %}
{% if w.has_arg %}
with {{ w.native_package }};
{% endif %}
{% endfor %}
package body Component.Simple_Command_Sequencer.Implementation is

   package Lgc renames Component.Simple_Command_Sequencer.Implementation.Logic;

   overriding procedure Init (Self : in out Instance; Num_Concurrent_Sequences : in Interfaces.Unsigned_32; Sequences : in Simple_Sequencer_Types.Sequences_Access) is
   begin
      Lgc.Init (Self, Num_Concurrent_Sequences, Sequences);
   end Init;

   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
   begin
      Lgc.Command_T_Recv_Async (Self, Arg);
   end Command_T_Recv_Async;

   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T) is
   begin
      Lgc.Command_T_Recv_Async_Dropped (Self, Arg);
   end Command_T_Recv_Async_Dropped;

   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      Lgc.Command_Response_T_Recv_Async (Self, Arg);
   end Command_Response_T_Recv_Async;

   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      Lgc.Command_Response_T_Recv_Async_Dropped (Self, Arg);
   end Command_Response_T_Recv_Async_Dropped;

   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T) is
   begin
      Lgc.Tick_T_Recv_Async (Self, Arg);
   end Tick_T_Recv_Async;

   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T) is
   begin
      Lgc.Tick_T_Recv_Async_Dropped (Self, Arg);
   end Tick_T_Recv_Async_Dropped;

   overriding function Run_Sequence (Self : in out Instance; Arg : in Run_Sequence_Arg.T) return Command_Execution_Status.E is
   begin
      return Lgc.Run_Sequence (Self, Arg);
   end Run_Sequence;

   overriding function Kill_All_Sequences (Self : in out Instance) return Command_Execution_Status.E is
   begin
      return Lgc.Kill_All_Sequences (Self);
   end Kill_All_Sequences;

   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
   begin
      return Lgc.Set_Summary_Packet_Period (Self, Arg);
   end Set_Summary_Packet_Period;
{% for w in sequence_wrappers %}

   -- Pack the typed argument for '{{ w.name }}' and dispatch through the generic
   -- Run_Sequence backbone (Sequences-table slot {{ w.index }}).
   overriding function {{ w.name }} (Self : in out Instance; Arg : in {{ w.wrapped_package }}.T) return Command_Execution_Status.E is
{% if w.has_arg %}
      Buffer : Simple_Sequencer_Types.Run_Sequence_Buffer_Type := [others => 0];
   begin
      Buffer (Buffer'First .. Buffer'First + {{ w.native_package }}.Serialization.Serialized_Length - 1) :=
         {{ w.native_package }}.Serialization.To_Byte_Array (Arg.User_Args);
      return Self.Run_Sequence
        ((Sequence_Id        => {{ w.index }},
          Response_Behavior  => Arg.Response_Behavior,
          Arg_Length         => {{ w.native_package }}.Serialization.Serialized_Length,
          Buffer_Arg         => Buffer));
{% else %}
   begin
      return Self.Run_Sequence
        ((Sequence_Id        => {{ w.index }},
          Response_Behavior  => Arg.Response_Behavior,
          Arg_Length         => 0,
          Buffer_Arg         => [others => 0]));
{% endif %}
   end {{ w.name }};
{% endfor %}

   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Lgc.Invalid_Command (Self, Cmd, Errant_Field_Number, Errant_Field);
   end Invalid_Command;

end Component.Simple_Command_Sequencer.Implementation;
