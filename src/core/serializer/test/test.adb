-- Tell the compiler that we are using Ravenscar
pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Test_Record.Representation; use Test_Record;
with Test_Record2.Representation; use Test_Record2;
with Basic_Types; use Basic_Types;
with Ada.Streams.Stream_IO;
with Serializer;
with Word_Serializer;
with Stream_Serializer;
with System.Address_Image;
with Command;
with Command_Types;
with Variable_Serializer;
with Serializer_Types; use Serializer_Types;

procedure Test is

   -- Tests:
   procedure Test_Serializer is
      package Myserializer is new Serializer (Test_Record.T);
      Myrecord : constant Test_Record.U := (Id => 7, Value => 15, Fvalue => 0.1);
      Myrecordcopy : Test_Record.U := (Id => 0, Value => 0, Fvalue => 0.0);
      Mypackedrecord : Test_Record.T;
      Mypackedrecordcopy : Test_Record.T;
      Mybytearray : Myserializer.Byte_Array;
      Mybytearray2 : Myserializer.Byte_Array;
      Mybytearraycopy : Myserializer.Byte_Array;
      Mypackedrecord2 : constant Test_Record.T := (Id => 7, Value => 15, Fvalue => 0.1);
   begin
      Put ("Packing record... ");
      Mypackedrecord := Test_Record.T (Myrecord);
      Put_Line ("passed.");

      Put ("Converting record... ");
      Mybytearray := Myserializer.To_Byte_Array (Mypackedrecord);
      Put_Line ("passed.");

      Put ("Bytes in byte array:");
      for I in Mybytearray'Range loop
         Put (Natural (Mybytearray (I)));
      end loop;
      Put_Line ("");

      Put ("Copying record... ");
      Mybytearraycopy := Mybytearray;
      pragma Assert (Mybytearraycopy = Mybytearray);
      Put_Line ("passed.");

      Put ("Converting byte array... ");
      Mypackedrecordcopy := Myserializer.From_Byte_Array (Mybytearraycopy);
      pragma Assert (Mypackedrecordcopy = Mypackedrecord);
      Put_Line ("passed.");

      Put ("Unpacking record... ");
      Myrecordcopy := Test_Record.U (Mypackedrecordcopy);
      pragma Assert (Myrecordcopy = Myrecord);
      Put_Line ("passed.");

      Put ("Bytes in byte array:");
      Mybytearray2 := Myserializer.To_Byte_Array (Mypackedrecord2);
      for I in Mybytearray2'Range loop
         Put (Natural (Mybytearray2 (I)));
      end loop;
      pragma Assert (Mypackedrecord2 = Mypackedrecord);
   end Test_Serializer;

   procedure Test_Word_Serializer is
      package Mywordserializer is new Word_Serializer (Test_Record2.T);
      Myrecord : constant Test_Record2.U := (Id => 42, Status => 7, Value => 12345, Counter => 98765);
      Myrecordcopy : Test_Record2.U := (Id => 0, Status => 0, Value => 0, Counter => 0);
      Mypackedrecord : Test_Record2.T;
      Mypackedrecordcopy : Test_Record2.T;
      Mywordarray : Mywordserializer.Word_Array;
      Mywordarraycopy : Mywordserializer.Word_Array;
      Mywordarray_Le : Mywordserializer.Word_Array_Le;
      Mywordarray_Le_Copy : Mywordserializer.Word_Array_Le;
      Mywordarray_Be : Mywordserializer.Word_Array_Be;
      Mywordarray_Be_Copy : Mywordserializer.Word_Array_Be;
   begin
      Put ("Packing record... ");
      Mypackedrecord := Test_Record2.T (Myrecord);
      Put_Line ("passed.");

      Put ("Serialized length in words: ");
      Put (Mywordserializer.Serialized_Length);
      New_Line;
      Put ("Serialized length in bytes: ");
      Put (Mywordserializer.Serialized_Length_In_Bytes);
      New_Line;

      Put ("Converting record to word array (native)... ");
      Mywordarray := Mywordserializer.To_Word_Array (Mypackedrecord);
      Put_Line ("passed.");

      Put ("Copying word array... ");
      Mywordarraycopy := Mywordarray;
      pragma Assert (Mywordarraycopy = Mywordarray);
      Put_Line ("passed.");

      Put ("Converting word array to record... ");
      Mypackedrecordcopy := Mywordserializer.From_Word_Array (Mywordarraycopy);
      pragma Assert (Mypackedrecordcopy = Mypackedrecord);
      Put_Line ("passed.");
      Put_Line ("Record: " & Test_Record2.Representation.Image (Mypackedrecordcopy));

      Put ("Unpacking record... ");
      Myrecordcopy := Test_Record2.U (Mypackedrecordcopy);
      pragma Assert (Myrecordcopy = Myrecord);
      Put_Line ("passed.");

      Put ("Converting record to word array (little endian)... ");
      Mywordarray_Le := Mywordserializer.To_Word_Array_Le (Mypackedrecord);
      Put_Line ("passed.");

      Put ("Converting word array (LE) to record... ");
      Mywordarray_Le_Copy := Mywordarray_Le;
      Mypackedrecordcopy := Mywordserializer.From_Word_Array_Le (Mywordarray_Le_Copy);
      pragma Assert (Mypackedrecordcopy = Mypackedrecord);
      Put_Line ("passed.");
      Put_Line ("Record: " & Test_Record2.Representation.Image (Mypackedrecordcopy));

      Put ("Converting record to word array (big endian)... ");
      Mywordarray_Be := Mywordserializer.To_Word_Array_Be (Mypackedrecord);
      Put_Line ("passed.");

      Put ("Converting word array (BE) to record... ");
      Mywordarray_Be_Copy := Mywordarray_Be;
      Mypackedrecordcopy := Mywordserializer.From_Word_Array_Be (Mywordarray_Be_Copy);
      pragma Assert (Mypackedrecordcopy = Mypackedrecord);
      Put_Line ("passed.");
      Put_Line ("Record: " & Test_Record2.Representation.Image (Mypackedrecordcopy));
   end Test_Word_Serializer;

   procedure Test_Stream_Serializer is
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
   end Test_Stream_Serializer;

   procedure Test_Single_Element_Serialization is
      package Naturalserializer is new Serializer (Natural);
      N : constant Natural := 7;
      M : Natural;
      Naturalbytearray : Naturalserializer.Byte_Array;
   begin
      Put ("Natural test... ");
      Put ("Byte Array Size: ");
      Put (Naturalbytearray'Size);
      New_Line;
      Put ("Byte Array Length: ");
      Put (Naturalbytearray'Length);
      New_Line;
      Put ("Natural Size: ");
      Put (Natural'Size);
      New_Line;
      Put (Natural'Object_Size);
      New_Line;
      Put (Natural'Value_Size);
      New_Line;
      Put ("Byte Size: ");
      Put (Byte'Size);
      New_Line;
      Put ("compute: ");
      Put ((Natural'Object_Size) / (Byte'Object_Size));
      New_Line;
      Naturalbytearray := Naturalserializer.To_Byte_Array (N);
      M := Naturalserializer.From_Byte_Array (Naturalbytearray);
      pragma Assert (N = M);
      Put_Line ("passed.");
   end Test_Single_Element_Serialization;

   procedure Test_Errant_Serialization is
      package Myserializer is new Serializer (Test_Record.T);
      Good_Bytes : constant Byte_Array := [0, 7, 0, 0, 0, 15, 63, 185, 153, 153, 153, 153, 153, 154];
      Bad_Bytes : constant Byte_Array := [255, 7, 0, 0, 0, 15, 63, 185, 153, 153, 153, 153, 153, 154];
      Rec : Test_Record.T;
   begin
      Rec := Myserializer.From_Byte_Array (Good_Bytes);
      Put_Line ("Good record: " & Test_Record.Representation.Image (Rec));
      Rec := Myserializer.From_Byte_Array (Bad_Bytes);
      Put_Line ("Bad record: " & Test_Record.Representation.Image (Rec));
      pragma Assert (not Rec.Id'Valid, "field should not be valid");
      pragma Assert (Rec.Value'Valid, "field should be valid");
      pragma Assert (Rec.Fvalue'Valid, "field should be valid");
   end Test_Errant_Serialization;

   procedure Test_Exception_Handling is
   begin
      raise Constraint_Error;
      pragma Assert (False, "Execution should never get here!");
   exception
      when others =>
         Put_Line ("Handling exception."); -- We made it here, yay!
   end Test_Exception_Handling;

   procedure Test_Copy_By_Reference is
      package Myserializer is new Serializer (Command.T);
      Buffer : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 19];
      A_Command : constant Command.T := ((Source_Id => 0, Id => 15, Arg_Buffer_Length => 2), Arg_Buffer => Buffer);
      A_Byte_Array : Myserializer.Byte_Array := [others => 50];

      procedure Some_Inner_Subprogram (Bytes : in Basic_Types.Byte_Array) is
      begin
         Put_Line (System.Address_Image (Bytes'Address));
      end Some_Inner_Subprogram;

      procedure Some_Subprogram (Bytes : in Basic_Types.Byte_Array) is
      begin
         Put_Line (System.Address_Image (Bytes'Address));
         Some_Inner_Subprogram (Bytes);
      end Some_Subprogram;

      procedure Some_Other_Inner_Subprogram (Cmd : in Command.T) is
      begin
         Put_Line (System.Address_Image (Cmd'Address));
      end Some_Other_Inner_Subprogram;

      procedure Some_Other_Subprogram (Cmd : in Command.T) is
      begin
         Put_Line (System.Address_Image (Cmd'Address));
         Some_Other_Inner_Subprogram (Cmd);
      end Some_Other_Subprogram;
   begin
      Put_Line ("pass serialized command to func");
      Put_Line (System.Address_Image (A_Command'Address));
      Some_Subprogram (Myserializer.To_Byte_Array (A_Command));
      Put_Line ("pass array to func");
      Put_Line (System.Address_Image (A_Byte_Array'Address));
      Some_Subprogram (A_Byte_Array);
      Put_Line ("pass deserializer array to func");
      Put_Line (System.Address_Image (A_Byte_Array'Address));
      Some_Other_Subprogram (Myserializer.From_Byte_Array (A_Byte_Array));
      Put_Line ("pass command to func");
      Put_Line (System.Address_Image (A_Command'Address));
      Some_Other_Subprogram (A_Command);
   end Test_Copy_By_Reference;

   procedure Test_Variable_Serializer is
      package Myserializer is new Variable_Serializer (Command.T, Command.Serialized_Length, Command.Serialized_Length);
      Ignore_Stat : Serialization_Status;
      Buffer : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 19];
      A_Command : constant Command.T := ((Source_Id => 0, Id => 15, Arg_Buffer_Length => 2), Arg_Buffer => Buffer);
      Num_Bytes_Serialized : Natural;
      Dest : Basic_Types.Byte_Array := [4 .. 0 => 0];
   begin
      Put_Line ("now");
      Put_Line ("'First " & Integer'Image (Dest'First));
      Put_Line ("'Last" & Integer'Image (Dest'Last));
      Put_Line ("'Length " & Integer'Image (Dest'Length));
      Ignore_Stat := Myserializer.To_Byte_Array (Dest => Dest, Src => A_Command, Num_Bytes_Serialized => Num_Bytes_Serialized);
      pragma Unreferenced (Dest);
      pragma Unreferenced (Num_Bytes_Serialized);
   end Test_Variable_Serializer;

-- Run all tests:
begin
   Test_Serializer;
   Test_Word_Serializer;
   Test_Stream_Serializer;
   Test_Single_Element_Serialization;
   Test_Errant_Serialization;
   Test_Exception_Handling;
   Test_Copy_By_Reference;
   Test_Variable_Serializer;
end Test;
