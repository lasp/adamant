-- simple_sequencer_types.ads
with Command_Types;
with Interfaces;
with Configuration;
with Basic_Types;
with Packet_Types;
with Sequence_Enums;
with Sequence_Frame_Summary;
with Sys_Time;
with Ada.Real_Time;

package Simple_Sequencer_Types is

   -- One entry in a sequence. Command_Step carries an argument serialized at
   -- codegen time; Runtime_Argument_Command_Step resolves it from the
   -- sequence's per-call argument via Resolver. Sleep pauses for a static
   -- millisecond count; Runtime_Sleep resolves the count (a Packed_Natural)
   -- from the per-call argument via Sleep_Resolver.
   type Step_Kind is (Command_Step, Runtime_Argument_Command_Step, Sleep, Runtime_Sleep);
   type Resolver_Access is access function (Bytes : Basic_Types.Byte_Array; Args : out Command_Types.Command_Arg_Buffer_Type) return Boolean;

   type Step (Kind : Step_Kind := Command_Step) is record
      Id         : Command_Types.Command_Id         := 0;
      Arg_Length : Command_Types.Command_Arg_Buffer_Length_Type := 0;
      case Kind is
         when Command_Step =>
            Arg       : Command_Types.Command_Arg_Buffer_Type := [others => 0];
         when Runtime_Argument_Command_Step =>
            Resolver  : Resolver_Access := null;
         when Sleep =>
            -- Milliseconds. Bounded to Natural by the model, so it always fits a Time_Span.
            Sleep_Arg : Natural := 0;
         when Runtime_Sleep =>
            Sleep_Resolver : Resolver_Access := null;
      end case;
   end record;

   -- 16-bit step index keeps the step counters compact on the wire. The model
   -- rejects longer sequences.
   type Step_Array is array (Interfaces.Unsigned_16 range <>) of Step;
   type Step_Array_Access is access constant Step_Array;

   -- The Run_Sequence passthrough buffer: the command argument buffer less the
   -- Sequence_Id and Arg_Length header fields.
   subtype Run_Sequence_Arg_Buffer_Length_Type is Command_Types.Command_Arg_Buffer_Length_Type range 0 .. (Configuration.Command_Buffer_Size - 4);
   subtype Run_Sequence_Arg_Buffer_Index_Type is Run_Sequence_Arg_Buffer_Length_Type range 0 .. Run_Sequence_Arg_Buffer_Length_Type'Last - 1;
   subtype Run_Sequence_Buffer_Type is Basic_Types.Byte_Array (Run_Sequence_Arg_Buffer_Index_Type);

   type Sequence_Type is record
      Wait_For_Cmd_Resp     : Boolean;
      Abort_On_Failed_Cmd   : Boolean;
      -- Converted from the model's milliseconds once, at elaboration.
      Command_Timeout       : Ada.Real_Time.Time_Span;
      -- Reply when the sequence starts, or defer the reply until it completes.
      Response_Behavior     : Sequence_Enums.Sequence_Response_Behavior.E;
      -- Serialized length of the sequence's argument type (0 if it has none).
      -- Run_Sequence rejects any other argument length.
      Arg_Length            : Run_Sequence_Arg_Buffer_Length_Type;
      Steps                 : Step_Array_Access;
   end record;

   -- Indexed by the 16-bit sequence id carried on the wire, so ids never need
   -- converting.
   type Sequences_Type is array (Interfaces.Unsigned_16 range <>) of Sequence_Type;
   type Sequences_Access is access constant Sequences_Type;

   -- Frames per sequencer instance. The upper bound is the most
   -- Sequence_Frame_Summary entries that fit in one summary packet.
   subtype Num_Concurrent_Sequences_Type is Interfaces.Unsigned_32 range 1 .. Interfaces.Unsigned_32 (Packet_Types.Packet_Buffer_Type'Length / Sequence_Frame_Summary.Size_In_Bytes);

   -- Frame ids index the frame pool, so they are bounded by the frame cap
   -- above and fit 16 bits like the sequence and step ids on the wire.
   subtype Frame_Id_Type is Interfaces.Unsigned_16 range 0 .. Interfaces.Unsigned_16 (Natural (Num_Concurrent_Sequences_Type'Last) - 1);

   -- Exported as the Config constant by each generated sequences suite package.
   -- Num_Concurrent_Sequences also sizes the suite's generated summary packet
   -- type, so the frame pool and the packet layout cannot disagree.
   type Sequencer_Config is record
      Sequences : not null Sequences_Access;
      Num_Concurrent_Sequences : Num_Concurrent_Sequences_Type;
   end record;

   -- One running sequence. Internal state only, never serialized; the
   -- downlinked view is Sequence_Frame_Summary. Run_Sequence re-seeds every
   -- per-run field when it claims a frame, and ending a sequence only sets
   -- Status to Not_Running, so an idle frame still reports its last run.
   type Sequence_Frame is record
      Sequence_Id : Interfaces.Unsigned_16 := 0;
      Frame_Id : Frame_Id_Type := 0;
      Step : Interfaces.Unsigned_16 := 0;
      Status : Sequence_Enums.Sequence_State.E := Sequence_Enums.Sequence_State.Not_Running;
      -- Wake time while Waiting_For_Time:
      Wait_Until : Sys_Time.T := (0, 0);
      -- Response deadline while Waiting_For_Cmd_Resp, stamped at dispatch:
      Timeout_Deadline : Sys_Time.T := (0, 0);
      -- Id of the sub-command awaited while Waiting_For_Cmd_Resp. Responses
      -- carrying any other id are stale and ignored.
      Pending_Command_Id : Command_Types.Command_Id := 0;
      -- Assigned by the command router's Register_Source handshake at startup:
      Source_Id : Command_Types.Command_Source_Id := 0;
      Has_Source_Id : Boolean := False;
      -- Copied from the sequence table at claim time for the summary packet.
      Response_Behavior : Sequence_Enums.Sequence_Response_Behavior.E := Sequence_Enums.Sequence_Response_Behavior.Send_After_Sequence_Start;
      -- Operator response context, used by the deferred-reply paths:
      Operator_Source_Id : Command_Types.Command_Source_Id := 0;
      Operator_Command_Id : Command_Types.Command_Id := 0;
      -- The caller's argument buffer, traversed by the Resolvers of dynamic steps.
      Dynamic_Arg : Run_Sequence_Buffer_Type := [others => 0];
   end record;

   type Sequence_Frame_Array is array (Frame_Id_Type range <>) of Sequence_Frame;
   type Sequence_Frame_Array_Access is access all Sequence_Frame_Array;
end Simple_Sequencer_Types;
