-- simple_sequencer_types.ads
with Command_Types;
with Scs_Arg_Resolver;
with Sequence_Sleep_Arg;
with Interfaces;
with Configuration;
with Basic_Types;

package Simple_Sequencer_Types is

   ---------------------------------------------------------------------------
   -- A serialized argument buffer: same layout as Command.Arg_Buffer_Type.
   -- We store the raw bytes here and let the sequencer stamp in Source_Id
   -- and construct the final Command.T at runtime.
   ---------------------------------------------------------------------------
   type Step_Kind is (Command_Step, Dynamic_Command_Step, Sleep);

   type Step (Kind : Step_Kind := Command_Step) is record
      Id         : Command_Types.Command_Id         := 0;
      Arg_Length : Command_Types.Command_Arg_Buffer_Length_Type := 0;
      case Kind is
         when Command_Step =>
            Arg       : Command_Types.Command_Arg_Buffer_Type := [others => 0];
         when Dynamic_Command_Step =>
            Resolver  : Scs_Arg_Resolver.T_Access := null;
         when Sleep =>
            Sleep_Arg : Sequence_Sleep_Arg.T;
      end case;
   end record;

   type Step_Array is array (Interfaces.Unsigned_32 range <>) of Step;
   type Step_Array_Access is access constant Step_Array;

   type Sequence_Type is record
      Wait_For_Cmd_Resp   : Boolean;
      Abort_On_Failed_Cmd : Boolean;
      Steps               : Step_Array_Access;
   end record;

   type Sequences_Type is array (Interfaces.Unsigned_32 range <>) of Sequence_Type;
   type Sequences_Access is access constant Sequences_Type;

   subtype Run_Sequence_Arg_Buffer_Length_Type is Command_Types.Command_Arg_Buffer_Length_Type range 0 .. (Configuration.Command_Buffer_Size - 10);
   subtype Run_Sequence_Arg_Buffer_Index_Type is Run_Sequence_Arg_Buffer_Length_Type range 0 .. Run_Sequence_Arg_Buffer_Length_Type'Last - 1;
   subtype Run_Sequence_Buffer_Type is Basic_Types.Byte_Array (Run_Sequence_Arg_Buffer_Index_Type);
end Simple_Sequencer_Types;