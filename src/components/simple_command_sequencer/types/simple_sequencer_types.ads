-- simple_sequencer_types.ads
with Command_Types;
with Packed_U32;
with Interfaces;
with Configuration;
with Basic_Types;
with Packet_Types;
with Sequence_Frame_Summary;

package Simple_Sequencer_Types is

   ---------------------------------------------------------------------------
   -- A Step is one entry in a sequence. Three flavors:
   --
   --  Command_Step                  - statically known command + args. Args are
   --                                  serialized at codegen time and stored in
   --                                  Arg.
   --  Runtime_Argument_Command_Step - the command is known statically but the
   --                                  args are resolved at execution time from
   --                                  the sequence's per-call argument payload
   --                                  via the Resolver dispatch object.
   --  Sleep                         - pause the sequence for Sleep_Arg
   --                                  milliseconds.
   ---------------------------------------------------------------------------
   type Step_Kind is (Command_Step, Runtime_Argument_Command_Step, Sleep);
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
            Sleep_Arg : Packed_U32.T;
      end case;
   end record;

   type Step_Array is array (Interfaces.Unsigned_32 range <>) of Step;
   type Step_Array_Access is access constant Step_Array;

   type Sequence_Type is record
      Wait_For_Cmd_Resp     : Boolean;
      Abort_On_Failed_Cmd   : Boolean;
      Command_Timeout_Millis : Interfaces.Unsigned_32;
      Steps                 : Step_Array_Access;
   end record;

   type Sequences_Type is array (Interfaces.Unsigned_32 range <>) of Sequence_Type;
   type Sequences_Access is access constant Sequences_Type;

   subtype Run_Sequence_Arg_Buffer_Length_Type is Command_Types.Command_Arg_Buffer_Length_Type range 0 .. (Configuration.Command_Buffer_Size - 5);
   subtype Run_Sequence_Arg_Buffer_Index_Type is Run_Sequence_Arg_Buffer_Length_Type range 0 .. Run_Sequence_Arg_Buffer_Length_Type'Last - 1;
   subtype Run_Sequence_Buffer_Type is Basic_Types.Byte_Array (Run_Sequence_Arg_Buffer_Index_Type);

   -- The number of sequence frames (concurrently running sequences) a sequencer
   -- instance may be configured with. The upper bound is the most frames whose
   -- Sequence_Frame_Summary entries fit in one summary packet, so a
   -- configuration whose summaries cannot be downlinked is rejected by the type
   -- system at Init.
   subtype Num_Concurrent_Sequences_Type is Interfaces.Unsigned_32 range 1 .. Interfaces.Unsigned_32 (Packet_Types.Packet_Buffer_Type'Length / Sequence_Frame_Summary.Size_In_Bytes);
end Simple_Sequencer_Types;
