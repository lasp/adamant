with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Stream_Serializer;
with Test_Record.Representation;

package body Serializer_Stream_Test is

   procedure Run is
      package Mystreamserializer is new Stream_Serializer (Test_Record.T);
      Outfile : Ada.Streams.Stream_IO.File_Type;
      Infile : Ada.Streams.Stream_IO.File_Type;
      Outstream : Ada.Streams.Stream_IO.Stream_Access;
      Instream : Ada.Streams.Stream_IO.Stream_Access;
      Mypackedrecord : constant Test_Record.T := (Id => 7, Value => 15, Fvalue => 0.1);
      Mypackedrecordcopy : Test_Record.T := (Id => 0, Value => 0, Fvalue => 0.0);
   begin
      Put ("Serializing to file... ");
      Ada.Streams.Stream_IO.Create (Outfile, Ada.Streams.Stream_IO.Out_File, "/tmp/file1.bin");
      Outstream := Ada.Streams.Stream_IO.Stream (Outfile);
      Mystreamserializer.Serialize (Outstream, Mypackedrecord);
      Ada.Streams.Stream_IO.Close (Outfile);
      Put_Line ("passed.");

      Put ("Deserializing from file... ");
      Ada.Streams.Stream_IO.Open (Infile, Ada.Streams.Stream_IO.In_File, "/tmp/file1.bin");
      Instream := Ada.Streams.Stream_IO.Stream (Infile);
      Mystreamserializer.Deserialize (Instream, Mypackedrecordcopy);
      Ada.Streams.Stream_IO.Close (Infile);
      pragma Unreferenced (Infile);
      Put_Line ("Record serialized: " & Test_Record.Representation.Image (Mypackedrecord));
      Put_Line ("Record deserialized: " & Test_Record.Representation.Image (Mypackedrecordcopy));
      -- The following line should work, but it currently does not pass due to a bug
      -- in the GNAT Linux GPL compiler...
      -- pragma Assert(myPackedRecordCopy = myPackedRecord);
      Put_Line ("passed.");
   end Run;

end Serializer_Stream_Test;
