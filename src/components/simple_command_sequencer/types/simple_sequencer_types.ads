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

   ---------------------------------------------------------------------------
   -- A Step is one entry in a sequence. Four flavors:
   --
   --  Command_Step                  - statically known command + args. Args are
   --                                  serialized at codegen time and stored in
   --                                  Arg.
   --  Runtime_Argument_Command_Step - the command is known statically but the
   --                                  args are resolved at execution time from
   --                                  the sequence's per-call argument payload
   --                                  via the Resolver dispatch object.
   --  Sleep                         - pause the sequence for a statically known
   --                                  number of milliseconds.
   --  Runtime_Sleep                 - pause the sequence for a duration resolved
   --                                  at execution time from the sequence's
   --                                  per-call argument payload (a serialized
   --                                  Packed_U32 millisecond count).
   ---------------------------------------------------------------------------
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
            -- Milliseconds. Static sleeps are bounded to Natural by the model
            -- so they always fit an Ada.Real_Time.Time_Span by construction.
            Sleep_Arg : Natural := 0;
         when Runtime_Sleep =>
            Sleep_Resolver : Resolver_Access := null;
      end case;
   end record;

   -- Step tables are indexed by a 16-bit type -- 65535 steps is far beyond any
   -- realistic sequence and keeps the step counters compact on the wire (the
   -- model rejects longer sequences at build time).
   type Step_Array is array (Interfaces.Unsigned_16 range <>) of Step;
   type Step_Array_Access is access constant Step_Array;

   type Sequence_Type is record
      Wait_For_Cmd_Resp     : Boolean;
      Abort_On_Failed_Cmd   : Boolean;
      -- Timeout for sub-command responses. Converted from the model's
      -- milliseconds once, at elaboration, so no conversion is needed when
      -- computing response deadlines at runtime.
      Command_Timeout       : Ada.Real_Time.Time_Span;
      -- When the sequencer replies to this sequence's command: immediately on
      -- start or deferred until the sequence completes. Static per-sequence
      -- configuration from the sequences model.
      Response_Behavior     : Sequence_Enums.Sequence_Response_Behavior.E;
      Steps                 : Step_Array_Access;
   end record;

   -- Sequence tables are indexed by the same 16-bit type that carries sequence
   -- ids on the wire (Run_Sequence_Arg, events, data products, frame
   -- summaries), so ids never need converting. Elements are aliased so a
   -- claimed frame can hold an access to its sequence instead of copying
   -- configuration out of the table.
   type Sequences_Type is array (Interfaces.Unsigned_16 range <>) of aliased Sequence_Type;
   type Sequences_Access is access constant Sequences_Type;

   subtype Run_Sequence_Arg_Buffer_Length_Type is Command_Types.Command_Arg_Buffer_Length_Type range 0 .. (Configuration.Command_Buffer_Size - 4);
   subtype Run_Sequence_Arg_Buffer_Index_Type is Run_Sequence_Arg_Buffer_Length_Type range 0 .. Run_Sequence_Arg_Buffer_Length_Type'Last - 1;
   subtype Run_Sequence_Buffer_Type is Basic_Types.Byte_Array (Run_Sequence_Arg_Buffer_Index_Type);

   -- The number of sequence frames (concurrently running sequences) a sequencer
   -- instance may be configured with. The upper bound is the most frames whose
   -- Sequence_Frame_Summary entries fit in one summary packet, so a
   -- configuration whose summaries cannot be downlinked is rejected by the type
   -- system at Init.
   subtype Num_Concurrent_Sequences_Type is Interfaces.Unsigned_32 range 1 .. Interfaces.Unsigned_32 (Packet_Types.Packet_Buffer_Type'Length / Sequence_Frame_Summary.Size_In_Bytes);

   -- A sequencer instance's complete static configuration, exported as a
   -- Config constant by each generated command sequences suite package. The
   -- frame-pool size lives in the suite's model (num_concurrent_sequences),
   -- which also sizes the suite's generated summary packet ground type --
   -- bundling both here makes a mismatch between the frame pool and the
   -- packet type impossible by construction.
   type Sequencer_Config is record
      Sequences : not null Sequences_Access;
      Num_Concurrent_Sequences : Num_Concurrent_Sequences_Type;
   end record;

   ---------------------------------------------------------------------------
   -- A frame executes one running sequence. Frames are plain (unpacked)
   -- records -- they are internal component state, never serialized; the
   -- downlinked view of a frame is Sequence_Frame_Summary. All per-run state
   -- is seeded when Run_Sequence claims the frame, and ending a sequence only
   -- flips Status back to Not_Running, so an idle frame still reports its
   -- last run in the summary packet.
   ---------------------------------------------------------------------------
   type Sequence_Frame is record
      Sequence_Id : Interfaces.Unsigned_16 := 0;
      Frame_Id : Interfaces.Unsigned_32 := 0;
      Step : Interfaces.Unsigned_16 := 0;
      -- The sequence table entry executing (or last executed) on this frame.
      -- Null only until the frame's first claim.
      Sequence : access constant Sequence_Type := null;
      Status : Sequence_Enums.Sequence_State.E := Sequence_Enums.Sequence_State.Not_Running;
      -- Wake time while Waiting_For_Time:
      Wait_Until : Sys_Time.T := (0, 0);
      -- Response deadline while Waiting_For_Cmd_Resp, stamped when the
      -- sub-command is dispatched:
      Timeout_Deadline : Sys_Time.T := (0, 0);
      -- Source id allocated to this frame by the command router's
      -- Register_Source handshake at startup:
      Source_Id : Command_Types.Command_Source_Id := 0;
      Has_Source_Id : Boolean := False;
      -- Response behavior of the sequence on this frame, copied from the
      -- sequence table at claim time so the summary packet can report it
      -- even for a frame that has never run (Sequence is null).
      Response_Behavior : Sequence_Enums.Sequence_Response_Behavior.E := Sequence_Enums.Sequence_Response_Behavior.Send_After_Sequence_Start;
      -- Operator response context captured at command-receive time; read on
      -- the Send_After_Sequence_Completion deferred-reply paths.
      Operator_Source_Id : Command_Types.Command_Source_Id := 0;
      Operator_Command_Id : Command_Types.Command_Id := 0;
      -- The caller's buffer argument (used prefix only), traversed by the
      -- per-step Resolvers of dynamic steps.
      Arg_Length : Run_Sequence_Arg_Buffer_Length_Type := 0;
      Dynamic_Arg : Run_Sequence_Buffer_Type := [others => 0];
   end record;

   type Sequence_Frame_Array is array (Interfaces.Unsigned_32 range <>) of Sequence_Frame;
   type Sequence_Frame_Array_Access is access all Sequence_Frame_Array;
end Simple_Sequencer_Types;
