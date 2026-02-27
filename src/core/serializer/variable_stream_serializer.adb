with Stream_Serializer;

package body Variable_Stream_Serializer is

   -- Byte arrays don't have a "scalar storage order" since they are an array of single byte
   -- items. So this warning doesn't apply. We can safely overlay a byte array with any type
   -- no matter the underlying scalar storage order.
   pragma Warnings (Off, "overlay changes scalar storage order");

   -- Public functions:
   function Serialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Input : in T) return Serialization_Status is
      package Static_Serializer is new Stream_Serializer (T);
      Stat : Serialization_Status;
      Num_Bytes_Serialized : Natural := 0;
   begin
      -- Get the length of the type:
      Stat := Serialized_Length (Input, Num_Bytes_Serialized);
      if Stat /= Success then
         return Stat;
      end if;

      -- Serialize the type, only the bytes that are valid:
      Static_Serializer.Serialize (Stream, Input, Num_Bytes_To_Serialize => Num_Bytes_Serialized);

      return Success;
   end Serialize;

   function Deserialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Minimum_Length : in Natural; Output : out T) return Serialization_Status is
      Constrained_Stream_Array_Size : constant Stream_Element_Offset := Stream_Element_Offset (Max_Serialized_Length);
      subtype Constrained_Stream_Array_Index is Stream_Element_Offset range 0 .. (Constrained_Stream_Array_Size - 1);
      subtype Constrained_Stream_Array is Stream_Element_Array (Constrained_Stream_Array_Index);
      Bytes : Constrained_Stream_Array with
         Import,
         Convention => Ada,
         Address => Output'Address;
      Stat : Serialization_Status;
      Ignore : Stream_Element_Offset;
      Num_Bytes_Serialized : Natural := 0;
   begin
      -- Read the minimum length of bytes from the stream. We are trying to read the header of the variable type
      -- so that we can compute its full size.
      Ada.Streams.Read (Stream.all, Bytes (Constrained_Stream_Array'First .. Constrained_Stream_Array'First + Stream_Element_Offset (Minimum_Length - 1)), Ignore);

      -- Compute the variable type's full size:
      Stat := Serialized_Length (Output, Num_Bytes_Serialized);
      if Stat /= Success then
         return Stat;
      end if;

      -- Read off the remaining bytes:
      Ada.Streams.Read (Stream.all, Bytes (Constrained_Stream_Array'First + Stream_Element_Offset (Minimum_Length) .. Constrained_Stream_Array'First + Stream_Element_Offset (Num_Bytes_Serialized - 1)), Ignore);

      return Success;
   end Deserialize;

   pragma Warnings (On, "overlay changes scalar storage order");

end Variable_Stream_Serializer;
