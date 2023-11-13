with Ada.Streams; use Ada.Streams;

-- Package which provides procedures for writing a pointer into a stream and reading
-- from a stream into a pointer.
package Byte_Array_Pointer.Stream is
   -- Copy a pointer onto the stream.
   procedure Write (A_Stream : not null access Ada.Streams.Root_Stream_Type'Class; Self : in Instance);
   -- Copy bytes from a stream onto the pointer.
   procedure Read (A_Stream : not null access Ada.Streams.Root_Stream_Type'Class; Self : in out Instance);
end Byte_Array_Pointer.Stream;
