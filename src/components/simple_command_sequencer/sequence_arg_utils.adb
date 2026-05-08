-- sequence_arg_utils.adb
package body Sequence_Arg_Utils is
   function To_Arg (Bytes : Basic_Types.Byte_Array)
      return Command_Types.Command_Arg_Buffer_Type is
      Result : Command_Types.Command_Arg_Buffer_Type := [others => 0];
   begin
      Result (Result'First .. Result'First + Bytes'Length - 1) := Bytes;
      return Result;
   end To_Arg;
end Sequence_Arg_Utils;