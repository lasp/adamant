with Variable_Database;
with Basic_Types;
with Serializer_Types;
with Interfaces;

-- This package exists solely so that GNATprove analyzes an instance of the
-- generic Variable_Database package, since GNATprove analyzes generics only
-- at their instantiation points. Real instantiations elsewhere in a project
-- are verified at their own instantiation points when they occur in SPARK
-- analyzed code. Nothing references this package, so it contributes no code
-- to any build.
package Variable_Database_Prover with SPARK_Mode => On is

   -- A representative fixed size element and Id, similar in shape to the
   -- data product records and Ids that variable databases typically hold:
   type Example_Id is range 0 .. 99;
   type Example_Element is record
      Id : Interfaces.Unsigned_16 := 0;
      Value : Interfaces.Unsigned_32 := 0;
   end record;

   -- The serialized length functions a fixed size element provides:
   function Serialized_Length (Src : in Example_Element; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status
      with Side_Effects;
   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serializer_Types.Serialization_Status
      with Side_Effects;

   package Example_Database is new Variable_Database (Example_Id, Example_Element, Serialized_Length, Serialized_Length);

end Variable_Database_Prover;
