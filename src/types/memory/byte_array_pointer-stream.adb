with Basic_Types;
with Stream_Serializer;

package body Byte_Array_Pointer.Stream is

   -- Copy a buffer onto the stream from the given start index. If the number of bytes is not specified, the entire buffer is copied.
   procedure Write (A_Stream : not null access Ada.Streams.Root_Stream_Type'Class; Self : in Instance) is
      use Basic_Types;
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;

         -- Stream serializer package:
      package Buffer_Stream_Serializer is new Stream_Serializer (Safe_Byte_Array_Type);
   begin
      -- Write buffer onto stream:
      Buffer_Stream_Serializer.Serialize (A_Stream, Safe_Byte_Array);
   end Write;

   -- Copy bytes from a stream onto the buffer at the given start index. If the number of bytes is not specified, the entire buffer is filled.
   procedure Read (A_Stream : not null access Ada.Streams.Root_Stream_Type'Class; Self : in out Instance) is
      use Basic_Types;
      subtype Safe_Byte_Array_Type is Byte_Array (0 .. Self.Length - 1);
      Safe_Byte_Array : Safe_Byte_Array_Type with Import, Convention => Ada, Address => Self.Address;

         -- Stream serializer package:
      package Buffer_Stream_Deserializer is new Stream_Serializer (Safe_Byte_Array_Type);
      Ignore : Stream_Element_Offset;
   begin
      -- Read from stream into buffer
      Buffer_Stream_Deserializer.Deserialize (A_Stream, Safe_Byte_Array);
   end Read;

end Byte_Array_Pointer.Stream;
