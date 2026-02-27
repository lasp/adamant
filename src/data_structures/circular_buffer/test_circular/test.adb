with Ada.Text_IO; use Ada.Text_IO;
with Circular_Buffer;
with Basic_Types.Representation; use Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;
with Byte_Array_Pointer;

procedure Test is
   Heap_Buf : Circular_Buffer.Circular;
   Data_Buf : Circular_Buffer.Circular;
   Data : aliased Byte_Array := [0 .. 9 => 0];

   procedure Go (Buf : in out Circular_Buffer.Circular) is
      use Circular_Buffer;
      package Push_Assert is new Smart_Assert.Basic (Circular_Buffer.Push_Status, Circular_Buffer.Push_Status'Image);
      package Pop_Assert is new Smart_Assert.Basic (Circular_Buffer.Pop_Status, Circular_Buffer.Pop_Status'Image);

      Max_Bytes_Used : Natural := 0;
      procedure Check_Meta (The_Num_Bytes_Used : in Natural; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
      begin
         if The_Num_Bytes_Used > Max_Bytes_Used then
            Max_Bytes_Used := The_Num_Bytes_Used;
         end if;
         Natural_Assert.Eq (Buf.Num_Bytes_Used, The_Num_Bytes_Used, "Num_Bytes_Used is wrong.", Filename, Line);
         Natural_Assert.Eq (Buf.Max_Num_Bytes_Used, Max_Bytes_Used, "Max_Num_Bytes_Used is wrong.", Filename, Line);
         Natural_Assert.Eq (Buf.Num_Bytes_Free, Data'Length - The_Num_Bytes_Used, "Num_Bytes_Free is wrong.", Filename, Line);
      end Check_Meta;

      procedure Super_Peek (Bytes : in out Basic_Types.Byte_Array; Bytes_To_Compare : in Basic_Types.Byte_Array; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
         Num_Bytes : Natural := Natural'Last;
         Cnt : Natural := 0;
         Expected_Cnt : constant Natural := Bytes_To_Compare'Length;
      begin
         for Idx in Natural range 0 .. Expected_Cnt - 1 loop
            Pop_Assert.Eq (Buf.Peek (Bytes, Num_Bytes, Offset => Cnt), Success, "Peek not successful!", Filename, Line);
            Natural_Assert.Eq (Num_Bytes, Expected_Cnt - Cnt, "num_Bytes_Returned wrong!", Filename, Line);
            -- Put_Line(Basic_Types.Representation.Image(bytes(0 .. Num_Bytes - 1)));
            Byte_Array_Assert.Eq (Bytes (0 .. Num_Bytes - 1), Bytes_To_Compare (Bytes_To_Compare'First + Cnt .. Bytes_To_Compare'First + Cnt + Num_Bytes - 1), "byte compare failed at iteration: " & Natural'Image (Idx), Filename, Line);
            Cnt := @ + 1;
         end loop;
         Pop_Assert.Eq (Buf.Peek (Bytes, Num_Bytes, Offset => Cnt), Empty);
         Pop_Assert.Eq (Buf.Peek (Bytes, Num_Bytes, Offset => 0), Success, "Pop not successful!", Filename, Line);
      end Super_Peek;

      procedure Super_Dump (Bytes : in Basic_Types.Byte_Array; Mem_Bytes : in Basic_Types.Byte_Array; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
         use Byte_Array_Pointer;
         Bytes_To_Compare : Basic_Types.Byte_Array (0 .. Mem_Bytes'Length - 1);

         -- Function to copy pointer dump to byte array for comparison:
         function Copy_Dump (Dump : in Circular_Buffer.Pointer_Dump) return Natural is
            Cnt : Natural := 0;
         begin
            for D of Dump loop
               -- Copy pointers to Bytes_To_Compare:
               if (not Is_Null (D)) and then Length (D) > 0 then
                  Bytes_To_Compare (Cnt .. Cnt + Length (D) - 1) := To_Byte_Array (D);
                  Cnt := @ + Length (D);
               end if;
            end loop;
            return Cnt;
         end Copy_Dump;

         -- Dump entire buffer:
         Dump : Circular_Buffer.Pointer_Dump := Buf.Dump;
         Bytes_Dumped : Natural := 0;
      begin
         -- Copy entire dump:
         Bytes_Dumped := Copy_Dump (Dump);

         -- Check bytes:
         Natural_Assert.Eq (Bytes_Dumped, Buf.Num_Bytes_Used);
         Natural_Assert.Eq (Bytes_Dumped, Bytes'Length);
         Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Bytes, "Checking full dump failed!", Filename, Line);

         -- Check dump_newest:
         for Idx in Bytes'Range loop
            Dump := Buf.Dump_Newest (Idx);
            Bytes_Dumped := Copy_Dump (Dump);
            Natural_Assert.Eq (Bytes_Dumped, Idx);
            Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Bytes (Bytes'Last - Idx + 1 .. Bytes'Last), "Checking newest dump failed on iteration " & Natural'Image (Idx), Filename, Line);
         end loop;

         -- Check dump_newest too much:
         Dump := Buf.Dump_Newest (Bytes'Length);
         Bytes_Dumped := Copy_Dump (Dump);
         Natural_Assert.Eq (Bytes_Dumped, Bytes'Length);
         Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Bytes, "Checking full dump newest length!", Filename, Line);
         Dump := Buf.Dump_Newest (Bytes'Length + 10);
         Bytes_Dumped := Copy_Dump (Dump);
         Natural_Assert.Eq (Bytes_Dumped, Bytes'Length);
         Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Bytes, "Checking full dump newest length + 10!", Filename, Line);

         -- Check dump_oldest:
         for Idx in Bytes'Range loop
            Dump := Buf.Dump_Oldest (Idx);
            Bytes_Dumped := Copy_Dump (Dump);
            Natural_Assert.Eq (Bytes_Dumped, Idx);
            Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Bytes (Bytes'First .. Bytes'First + Bytes_Dumped - 1), "Checking oldest dump failed on iteration " & Natural'Image (Idx), Filename, Line);
         end loop;

         -- Check dump_newest too much:
         Dump := Buf.Dump_Oldest (Bytes'Length);
         Bytes_Dumped := Copy_Dump (Dump);
         Natural_Assert.Eq (Bytes_Dumped, Bytes'Length);
         Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Bytes, "Checking full dump oldest length!", Filename, Line);
         Dump := Buf.Dump_Oldest (Bytes'Length + 10);
         Bytes_Dumped := Copy_Dump (Dump);
         Natural_Assert.Eq (Bytes_Dumped, Bytes'Length);
         Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Bytes, "Checking full dump oldest length + 10!", Filename, Line);

         -- Check dump memory:
         Dump := Buf.Dump_Memory;
         Bytes_Dumped := Copy_Dump (Dump);
         Natural_Assert.Eq (Bytes_Dumped, Buf.Num_Bytes_Total);
         Byte_Array_Assert.Eq (Bytes_To_Compare (0 .. Bytes_Dumped - 1), Mem_Bytes, "Checking memory!", Filename, Line);
      end Super_Dump;

      Bytes : Byte_Array (0 .. 9) := [others => 0];
      Num_Bytes : Natural := 0;
   begin
      Put_Line ("Check initial sizes.");
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Small push, peek, and pop.");
      Push_Assert.Eq (Buf.Push ([1, 2, 3]), Success);
      Check_Meta (3);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 2), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 3);
      Byte_Array_Assert.Eq (Bytes, [1, 2, 3, 0, 0, 0, 0, 0, 0, 0]);
      Super_Peek (Bytes, Bytes_To_Compare => [1, 2, 3]);
      Check_Meta (3);
      Pop_Assert.Eq (Buf.Pop (Bytes (3 .. 5), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 3);
      Byte_Array_Assert.Eq (Bytes, [1, 2, 3, 1, 2, 3, 0, 0, 0, 0]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Peek and pop empty.");
      Bytes := [others => 0];
      Pop_Assert.Eq (Buf.Peek (Bytes (1 .. 0), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 0);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 0), Num_Bytes), Empty);
      Natural_Assert.Eq (Num_Bytes, 0);
      Byte_Array_Assert.Eq (Bytes, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
      Check_Meta (0);
      Pop_Assert.Eq (Buf.Pop (Bytes (4 .. 3), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 0);
      Pop_Assert.Eq (Buf.Pop (Bytes (3 .. 3), Num_Bytes), Empty);
      Natural_Assert.Eq (Num_Bytes, 0);
      Byte_Array_Assert.Eq (Bytes, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Push, peek, and pop zero length arrays.");
      Buf.Clear;
      Data := [others => 0];
      Push_Assert.Eq (Buf.Push (Bytes (1 .. 0)), Success);
      Byte_Array_Assert.Eq (Data, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
      Check_Meta (0);
      Pop_Assert.Eq (Buf.Peek (Bytes (1 .. 0), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 0);
      Byte_Array_Assert.Eq (Bytes, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
      Check_Meta (0);
      Pop_Assert.Eq (Buf.Pop (Bytes (3 .. 1), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 0);
      Byte_Array_Assert.Eq (Bytes, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
      Byte_Array_Assert.Eq (Data, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Push full.");
      Buf.Clear;
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Success);
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 9), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Super_Peek (Bytes, Bytes_To_Compare => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Push_Assert.Eq (Buf.Push ([3 => 11]), Too_Full);
      Check_Meta (10);
      -- zero length should be successful still
      Push_Assert.Eq (Buf.Push (Bytes (1 .. 0)), Success);
      Check_Meta (10);
      -- Check dump:
      Super_Dump ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Peek and pop too much.");
      Bytes := [others => 0];
      Pop_Assert.Eq (Buf.Peek (Bytes (1 .. 1), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 1);
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Pop (Bytes (2 .. 2), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 1);
      Byte_Array_Assert.Eq (Bytes, [0, 1, 1, 0, 0, 0, 0, 0, 0, 0]);
      Check_Meta (9);
      Pop_Assert.Eq (Buf.Peek (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 9);
      Check_Meta (9);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Push too much.");
      Push_Assert.Eq (Buf.Push ([1, 2]), Too_Full);
      Check_Meta (9);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test rollover.");
      Buf.Clear;
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7]), Success);
      Check_Meta (7);
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 2), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 3);
      Byte_Array_Assert.Eq (Bytes (0 .. 2), [1, 2, 3]);
      Check_Meta (4);
      -- Push, and cause error:
      Put_Line (Basic_Types.Representation.Image (Data));
      Push_Assert.Eq (Buf.Push ([8, 9, 10, 11, 12, 13, 14]), Too_Full);
      Check_Meta (4);
      -- Push some:
      Push_Assert.Eq (Buf.Push ([8, 9]), Success);
      Check_Meta (6);
      -- Cause roll over:
      Push_Assert.Eq (Buf.Push ([10, 11, 12, 13, 14]), Too_Full);
      Check_Meta (6);
      Push_Assert.Eq (Buf.Push ([10, 11, 12]), Success);
      Check_Meta (9);
      Bytes := [others => 0];
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 8), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 9);
      Byte_Array_Assert.Eq (Bytes (0 .. 8), [4, 5, 6, 7, 8, 9, 10, 11, 12]);
      Byte_Array_Assert.Eq (Bytes, [4, 5, 6, 7, 8, 9, 10, 11, 12, 0]);
      Put_Line (Basic_Types.Representation.Image (Data));
      Super_Peek (Bytes, Bytes_To_Compare => [4, 5, 6, 7, 8, 9, 10, 11, 12]);
      Check_Meta (9);
      -- Check dump:
      Super_Dump ([4, 5, 6, 7, 8, 9, 10, 11, 12], [11, 12, 3, 4, 5, 6, 7, 8, 9, 10]);
      Bytes := [others => 0];
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 8), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 9);
      Byte_Array_Assert.Eq (Bytes, [4, 5, 6, 7, 8, 9, 10, 11, 12, 0]);
      Check_Meta (0);
      Super_Dump (Bytes (1 .. 0), [11, 12, 3, 4, 5, 6, 7, 8, 9, 10]);
      Put_Line (Basic_Types.Representation.Image (Data));
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test multiple rollover.");
      Buf.Clear;
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9]), Success);
      Check_Meta (9);
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 7), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 8);
      Byte_Array_Assert.Eq (Bytes (0 .. 7), [1, 2, 3, 4, 5, 6, 7, 8]);
      Check_Meta (1);
      Push_Assert.Eq (Buf.Push ([10, 11, 12, 13, 14, 15, 16, 17]), Success);
      Check_Meta (9);
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 7), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 8);
      Byte_Array_Assert.Eq (Bytes (0 .. 7), [9, 10, 11, 12, 13, 14, 15, 16]);
      Check_Meta (1);
      Put_Line (Basic_Types.Representation.Image (Data));
      Push_Assert.Eq (Buf.Push ([18, 19, 20, 21, 22, 23, 24, 25, 26, 27]), Too_Full);
      Push_Assert.Eq (Buf.Push ([18, 19, 20, 21, 22, 23, 24]), Success);
      Push_Assert.Eq (Buf.Push ([4 .. 0 => 0]), Success);
      Push_Assert.Eq (Buf.Push ([4 .. 1 => 0]), Success);
      Check_Meta (8);
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 6), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 7);
      Byte_Array_Assert.Eq (Bytes (0 .. 6), [17, 18, 19, 20, 21, 22, 23]);
      Check_Meta (1);
      Put_Line (Basic_Types.Representation.Image (Data));
      Super_Dump ([0 => 24], [21, 22, 23, 24, 15, 16, 17, 18, 19, 20]);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test multiple empty.");
      -- do complete zeroing of internal data:
      Buf.Clear;
      Push_Assert.Eq (Buf.Push ([0, 0, 0, 0, 0, 0, 0, 0, 0, 0], Overwrite => True), Success);
      Pop_Assert.Eq (Buf.Pop (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      -- start test:
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9]), Success);
      Check_Meta (9);
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 8), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 9);
      Byte_Array_Assert.Eq (Bytes (0 .. 8), [1, 2, 3, 4, 5, 6, 7, 8, 9]);
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([10, 11, 12, 13, 14, 15, 16, 17]), Success);
      Check_Meta (8);
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 7), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 8);
      Byte_Array_Assert.Eq (Bytes (0 .. 7), [10, 11, 12, 13, 14, 15, 16, 17]);
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([18, 19, 20, 21, 22, 23, 24, 25]), Success);
      Check_Meta (8);
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 7), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 8);
      Byte_Array_Assert.Eq (Bytes (0 .. 7), [18, 19, 20, 21, 22, 23, 24, 25]);
      Check_Meta (0);
      Put_Line (Basic_Types.Representation.Image (Data));
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test make full.");
      Buf.Make_Full;
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Pop (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes, [18, 19, 20, 21, 22, 23, 24, 25, 9, 0]);
      Check_Meta (0);
      Buf.Make_Full (2);
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Pop (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes, [20, 21, 22, 23, 24, 25, 9, 0, 18, 19]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test long overwrite.");
      Buf.Clear;
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15], Overwrite => True), Too_Full);
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9], Overwrite => True), Success);
      Push_Assert.Eq (Buf.Push ([10, 11, 12, 13, 14, 15], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Peek (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 9), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes (0 .. 9), [6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
      Pop_Assert.Eq (Buf.Peek (Bytes, Num_Bytes), Success);
      Super_Peek (Bytes, Bytes_To_Compare => [6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
      Natural_Assert.Eq (Num_Bytes, 10);
      Pop_Assert.Eq (Buf.Pop (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes (0 .. 9), [6, 7, 8, 9, 10, 11, 12, 13, 14, 15]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test multiple small overwrites.");
      Buf.Clear;
      Data := [others => 0];
      Check_Meta (0);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (5);
      Push_Assert.Eq (Buf.Push ([6, 7], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (7);
      Push_Assert.Eq (Buf.Push ([10, 11, 12, 13], Overwrite => False), Too_Full);
      Push_Assert.Eq (Buf.Push ([10, 11, 12, 13], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 9), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 6), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 7);
      Byte_Array_Assert.Eq (Bytes (0 .. 6), [2, 3, 4, 5, 6, 7, 10]);
      Put_Line (Basic_Types.Representation.Image (Data));
      Push_Assert.Eq (Buf.Push ([15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 29, 30, 31, 32], Overwrite => True), Too_Full);
      Push_Assert.Eq (Buf.Push ([15, 16, 17, 18, 19, 20, 21], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 9), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes (0 .. 9), [11, 12, 13, 15, 16, 17, 18, 19, 20, 21]);
      Push_Assert.Eq (Buf.Push ([21, 22, 23, 24, 25, 26, 27, 28], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Peek (Bytes (0 .. 9), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes (0 .. 9), [20, 21, 21, 22, 23, 24, 25, 26, 27, 28]);
      Push_Assert.Eq (Buf.Push ([1 => 29], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Push_Assert.Eq (Buf.Push ([5 => 30], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Pop_Assert.Eq (Buf.Pop (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Byte_Array_Assert.Eq (Bytes, [21, 22, 23, 24, 25, 26, 27, 28, 29, 30]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test more overwrites.");
      Data := [others => 0];
      Check_Meta (0);
      -- Lots of overwrites:
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5]), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (5);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6]), Too_Full);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7]), Too_Full);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8]), Too_Full);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Push_Assert.Eq (Buf.Push ([9, 9, 9, 9]), Too_Full);
      Push_Assert.Eq (Buf.Push ([9, 9, 9, 9], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      -- Lots of wrapping peeks with offsets.
      Pop_Assert.Eq (Buf.Peek (Bytes, Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 10);
      Super_Peek (Bytes, Bytes_To_Compare => [3, 4, 5, 6, 7, 8, 9, 9, 9, 9]);
      -- Dealloc some from beginning.
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 1), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 2);
      Byte_Array_Assert.Eq (Bytes (0 .. 1), [3, 4]);
      Check_Meta (8);
      -- Dealloc wrap.
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 4), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 5);
      Byte_Array_Assert.Eq (Bytes (0 .. 4), [5, 6, 7, 8, 9]);
      Check_Meta (3);
      -- Push overwrite
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8]), Too_Full);
      Push_Assert.Eq (Buf.Push ([1, 2, 3, 4, 5, 6, 7, 8], Overwrite => True), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      -- Dealloc some from beginning.
      Pop_Assert.Eq (Buf.Pop (Bytes (0 .. 4), Num_Bytes), Success);
      Natural_Assert.Eq (Num_Bytes, 5);
      Byte_Array_Assert.Eq (Bytes (0 .. 4), [9, 9, 1, 2, 3]);
      Check_Meta (5);
      Push_Assert.Eq (Buf.Push ([66, 66, 66, 66]), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (9);
      Super_Peek (Bytes, Bytes_To_Compare => [4, 5, 6, 7, 8, 66, 66, 66, 66]);
      Push_Assert.Eq (Buf.Push ([77, 77]), Too_Full);
      Push_Assert.Eq (Buf.Push ([77, 77, 77], Overwrite => True), Success);
      Push_Assert.Eq (Buf.Push ([4 .. 0 => 0], Overwrite => True), Success);
      Push_Assert.Eq (Buf.Push ([4 .. 0 => 0], Overwrite => False), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (10);
      Super_Peek (Bytes, Bytes_To_Compare => [6, 7, 8, 66, 66, 66, 66, 77, 77, 77]);
      Super_Dump ([6, 7, 8, 66, 66, 66, 66, 77, 77, 77], [66, 66, 77, 77, 77, 6, 7, 8, 66, 66]);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test make full / dump scenario.");
      Buf.Make_Full;
      while Buf.Pop (Bytes (0 .. 2), Num_Bytes) = Success loop
         Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Num_Bytes - 1)));
      end loop;
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test serialization.");
      Put_Line ("Passed.");
      Put_Line ("");

   end Go;
begin
   Put_Line ("Create heap buffer.");
   Heap_Buf.Init (10);
   Put_Line ("Passed.");
   Put_Line ("");

   Put_Line ("----------------------------------");
   Put_Line ("Testing heap buffer.");
   Put_Line ("----------------------------------");
   Go (Heap_Buf);
   Put_Line ("----------------------------------");
   Put_Line ("");

   Put_Line ("Destroy heap buffer.");
   Heap_Buf.Destroy;
   Put_Line ("Passed.");
   Put_Line ("");

   Put_Line ("Create data buffer.");
   Data_Buf.Init (Data'Unchecked_Access);
   Put_Line ("Passed.");
   Put_Line ("");

   Put_Line ("----------------------------------");
   Put_Line ("Testing data buffer.");
   Put_Line ("----------------------------------");
   Go (Data_Buf);
   Put_Line ("----------------------------------");
   Put_Line ("");

   Put_Line ("Destroy data buffer.");
   Data_Buf.Destroy;
   pragma Unreferenced (Data_Buf);
   Put_Line ("Passed.");
end Test;
