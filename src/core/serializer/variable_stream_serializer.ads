-- Standard includes:
with Ada.Streams; use Ada.Streams;
with Serializer_Types; use Serializer_Types;

-- Generic package which provides conversions from a variable length type into Streams
-- using overlays.
generic
   type T is private;
   with function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status;
package Variable_Stream_Serializer is
   Max_Serialized_Length : constant Natural := T'Object_Size / Stream_Element'Object_Size;
   -- Serialize to and deserialize from streams.
   function Serialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Input : in T) return Serialization_Status;
   -- Deserialize needs to know the minimum length of the variable type (usually the size of the header) in order to read that
   -- many bytes off the stream before determining how many additional bytes need to be read to deserialize the entire type.
   -- Passing the type's "Min_Serialized_Length" constant (found in the record autocoded ads) will suffice for this.
   function Deserialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Minimum_Length : in Natural; Output : out T) return Serialization_Status;
end Variable_Stream_Serializer;
