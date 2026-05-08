-- sequence_arg_utils.ads
with Command_Types; use Command_Types;
with Basic_Types;

package Sequence_Arg_Utils is
   function To_Arg (Bytes : Basic_Types.Byte_Array)
      return Command_Arg_Buffer_Type;
end Sequence_Arg_Utils;