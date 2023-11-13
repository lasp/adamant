package body Stream_Serializer is

   -- Private functions:
   Constrained_Stream_Array_Size : constant Stream_Element_Offset := Stream_Element_Offset (Serialized_Length);
   subtype Constrained_Stream_Array_Index is Stream_Element_Offset range 0 .. (Constrained_Stream_Array_Size - 1);
   subtype Constrained_Stream_Array is Stream_Element_Array (Constrained_Stream_Array_Index);

   -- Byte arrays don't have a "scalar storage order" since they are an array of single byte
   -- items. So this warning doesn't apply. We can safely overlay a byte array with any type
   -- no matter the underlying scalar storage order.
   pragma Warnings (Off, "overlay changes scalar storage order");

   -- Public functions:
   procedure Serialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Input : in T; Num_Bytes_To_Serialize : in Natural := Serialized_Length) is
      Bytes : Constrained_Stream_Array with Import, Convention => Ada, Address => Input'Address;
   begin
      Ada.Streams.Write (Stream.all, Bytes (Constrained_Stream_Array'First .. Constrained_Stream_Array'First + Stream_Element_Offset (Num_Bytes_To_Serialize - 1)));
   end Serialize;

   procedure Deserialize (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Output : out T; Num_Bytes_To_Deserialize : in Natural := Serialized_Length) is
      Bytes : Constrained_Stream_Array with Import, Convention => Ada, Address => Output'Address;
      Ignore : Stream_Element_Offset;
   begin
      Ada.Streams.Read (Stream.all, Bytes (Constrained_Stream_Array'First .. Constrained_Stream_Array'First + Stream_Element_Offset (Num_Bytes_To_Deserialize - 1)), Ignore);
   end Deserialize;

   pragma Warnings (On, "overlay changes scalar storage order");

end Stream_Serializer;
