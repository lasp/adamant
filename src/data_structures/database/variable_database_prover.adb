package body Variable_Database_Prover with SPARK_Mode => On is

   Element_Length : constant Natural := Example_Element'Object_Size / Basic_Types.Byte'Object_Size;

   function Serialized_Length (Src : in Example_Element; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status is
      pragma Unreferenced (Src);
   begin
      Num_Bytes_Serialized := Element_Length;
      return Serializer_Types.Success;
   end Serialized_Length;

   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status is
   begin
      Num_Bytes_Serialized := Element_Length;
      if Src'Length < Element_Length then
         return Serializer_Types.Failure;
      end if;
      return Serializer_Types.Success;
   end Serialized_Length;

end Variable_Database_Prover;
