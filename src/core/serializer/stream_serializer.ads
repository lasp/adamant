-- Standard includes:
with Ada.Streams; use Ada.Streams;

-- Generic package which provides conversions from a given type into Streams
-- using overlays.
generic
   type T is private;
package Stream_Serializer is
   Serialized_Length : constant Natural := T'Object_Size / Stream_Element'Object_Size;
   -- Serialize to and deserialize from streams.
   -- You can provide the number of bytes to serialize/deserialize, otherwise enough bytes will be read
   -- to fill the entire object "T". A partial serialize/deserialize might be useful when reading or
   -- writing a subset of data to / from a larger array or buffer.
   procedure Serialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Input : in T; Num_Bytes_To_Serialize : in Natural := Serialized_Length);
   procedure Deserialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Output : out T; Num_Bytes_To_Deserialize : in Natural := Serialized_Length);
end Stream_Serializer;
