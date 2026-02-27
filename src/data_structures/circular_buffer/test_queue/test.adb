with Ada.Text_IO; use Ada.Text_IO;
with Circular_Buffer;
with Basic_Types.Representation; use Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;

procedure Test is
   Data_Queue : Circular_Buffer.Queue;
   Data : aliased Byte_Array := [0 .. 29 => 0];
   Ignore : Natural;

   procedure Go (Queue : in out Circular_Buffer.Queue) is
      use Circular_Buffer;
      package Push_Assert is new Smart_Assert.Basic (Circular_Buffer.Push_Status, Circular_Buffer.Push_Status'Image);
      package Pop_Assert is new Smart_Assert.Basic (Circular_Buffer.Pop_Status, Circular_Buffer.Pop_Status'Image);
      Bytes : Byte_Array (0 .. 29) := [others => 0];
      Len : Natural;
      Max_Count : Natural := 0;

      procedure Check_Meta (Cnt : in Natural; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
      begin
         if Cnt > Max_Count then
            Max_Count := Cnt;
         end if;
         Natural_Assert.Eq (Queue.Get_Count, Cnt, "Get_Count failed.", Filename, Line);
         Natural_Assert.Eq (Queue.Get_Max_Count, Max_Count, "Get_Count failed.", Filename, Line);
      end Check_Meta;

   begin
      Put_Line ("Check initial sizes.");
      Check_Meta (0);
      Pop_Assert.Eq (Queue.Peek_Length (Len), Empty);
      Pop_Assert.Eq (Queue.Peek (Bytes, Length => Len), Empty);
      Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Empty);
      Pop_Assert.Eq (Queue.Pop, Empty);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Small push, peek, and pop.");
      Push_Assert.Eq (Queue.Push ([1, 2, 3]), Success);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (1);
      Pop_Assert.Eq (Queue.Peek_Length (Len), Success);
      Natural_Assert.Eq (Len, 3);
      Check_Meta (1);
      Len := 0;
      Pop_Assert.Eq (Queue.Peek (Bytes, Length => Len), Success);
      Natural_Assert.Eq (Len, 3);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [1, 2, 3]);
      Check_Meta (1);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Peek and pop empty.");
      Queue.Clear;
      Bytes := [others => 0];
      Pop_Assert.Eq (Queue.Peek (Bytes (1 .. 0), Length => Len), Empty);
      Natural_Assert.Eq (Len, 0);
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 0), Length => Len), Empty);
      Natural_Assert.Eq (Len, 0);
      Byte_Array_Assert.Eq (Bytes, [0 .. 29 => 0]);
      Check_Meta (0);
      Pop_Assert.Eq (Queue.Pop (Bytes (3 .. 3), Ignore), Empty);
      Byte_Array_Assert.Eq (Bytes, [0 .. 29 => 0]);
      Check_Meta (0);
      Pop_Assert.Eq (Queue.Pop, Empty);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Push, peek, and pop zero length arrays.");
      Queue.Clear;
      Data := [others => 0];
      Push_Assert.Eq (Queue.Push (Bytes (1 .. 0)), Success);
      Byte_Array_Assert.Eq (Data, [0 .. 29 => 0]);
      Check_Meta (1);
      Pop_Assert.Eq (Queue.Peek_Length (Len), Success);
      Natural_Assert.Eq (Len, 0);
      Check_Meta (1);
      Pop_Assert.Eq (Queue.Peek (Bytes (1 .. 0), Length => Len), Success);
      Natural_Assert.Eq (Len, 0);
      Byte_Array_Assert.Eq (Bytes, [0 .. 29 => 0]);
      Check_Meta (1);
      Pop_Assert.Eq (Queue.Pop (Bytes (3 .. 1), Length => Len), Success);
      Natural_Assert.Eq (Len, 0);
      Byte_Array_Assert.Eq (Bytes, [0 .. 29 => 0]);
      Byte_Array_Assert.Eq (Data, [0 .. 29 => 0]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Push full.");
      Queue.Clear;
      Push_Assert.Eq (Queue.Push ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), Success);
      Check_Meta (1);
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 9), Ignore), Success);
      Byte_Array_Assert.Eq (Bytes (0 .. 9), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 14);
      Push_Assert.Eq (Queue.Push ([3 .. 10 => 11]), Success);
      Check_Meta (2);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 26);
      Push_Assert.Eq (Queue.Push ([0 .. 0 => 5]), Too_Full);
      Push_Assert.Eq (Queue.Push (Bytes (1 .. 0)), Success);
      Check_Meta (3);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 30);
      Push_Assert.Eq (Queue.Push (Bytes (1 .. 0)), Too_Full);
      Check_Meta (3);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Peek and pop too much.");
      Bytes := [others => 0];
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 4), Length => Len), Success);
      Natural_Assert.Eq (Len, 5);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [1, 2, 3, 4, 5]);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes, Length => Len), Success);
      Natural_Assert.Eq (Len, 10);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes, Length => Len, Offset => 2), Success);
      Natural_Assert.Eq (Len, 8);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [3, 4, 5, 6, 7, 8, 9, 10]);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 1), Length => Len, Offset => 2), Success);
      Natural_Assert.Eq (Len, 2);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [3, 4]);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 1), Length => Len, Offset => 9), Success);
      Natural_Assert.Eq (Len, 1);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [1 => 10]);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 1), Length => Len, Offset => 10), Success);
      Natural_Assert.Eq (Len, 0);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 1), Length => Len, Offset => 11), Success);
      Natural_Assert.Eq (Len, 0);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes (0 .. 1), Length => Len, Offset => 2_000), Success);
      Natural_Assert.Eq (Len, 0);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Peek (Bytes (1 .. 0), Length => Len, Offset => 1), Success);
      Natural_Assert.Eq (Len, 0);
      Check_Meta (3);
      Pop_Assert.Eq (Queue.Pop (Bytes (0 .. 6), Length => Len), Success);
      Natural_Assert.Eq (Len, 7);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [1, 2, 3, 4, 5, 6, 7]);
      Check_Meta (2);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 16);
      Put_Line (Basic_Types.Representation.Image (Data));
      Pop_Assert.Eq (Queue.Peek_Length (Length => Len), Success);
      Natural_Assert.Eq (Len, 8);
      Pop_Assert.Eq (Queue.Pop (Bytes (0 .. 3), Length => Len, Offset => 5), Success);
      Natural_Assert.Eq (Len, 3);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [11, 11, 11]);
      Check_Meta (1);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 4);
      Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Success);
      Natural_Assert.Eq (Len, 0);
      Check_Meta (0);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 0);
      Pop_Assert.Eq (Queue.Peek (Bytes, Ignore), Empty);
      Check_Meta (0);
      Pop_Assert.Eq (Queue.Pop (Bytes, Ignore), Empty);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Push too much.");
      Push_Assert.Eq (Queue.Push ([0 .. 30 => 5]), Too_Full);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

      Put_Line ("Test rollover.");
      Queue.Clear;
      Check_Meta (0);
      Push_Assert.Eq (Queue.Push ([0 .. 9 => 255, 10 .. 21 => 254]), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 26);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (1);
      Pop_Assert.Eq (Queue.Pop (Bytes (0 .. 8), Ignore), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 0);
      Byte_Array_Assert.Eq (Bytes (0 .. 8), [0 .. 8 => 255]);
      Check_Meta (0);
      Push_Assert.Eq (Queue.Push ([0 .. 9 => 10]), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 14);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (1);
      Push_Assert.Eq (Queue.Push ([0 .. 9 => 11]), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 28);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (2);
      Push_Assert.Eq (Queue.Push ([0 .. 9 => 12]), Too_Full);
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 28);
      Check_Meta (2);
      Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 14);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [0 .. Len - 1 => 10]);
      Check_Meta (1);
      -- Push, and cause rollover:
      Push_Assert.Eq (Queue.Push ([0 .. 7 => 12]), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 26);
      Put_Line (Basic_Types.Representation.Image (Data));
      Check_Meta (2);
      Push_Assert.Eq (Queue.Push ([0 .. 0 => 12]), Too_Full);
      Check_Meta (2);
      -- Pop and check:
      Pop_Assert.Eq (Queue.Pop, Success);
      -- Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 12);
      -- Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), (0 .. Len - 1 => 11));
      Check_Meta (1);
      Pop_Assert.Eq (Queue.Pop (Bytes, Length => Len), Success);
      Put_Line ("Num_Bytes_Used: " & Natural'Image (Queue.Num_Bytes_Used));
      Natural_Assert.Eq (Queue.Num_Bytes_Used, 0);
      Byte_Array_Assert.Eq (Bytes (0 .. Len - 1), [0 .. Len - 1 => 12]);
      Check_Meta (0);
      Put_Line ("Passed.");
      Put_Line ("");

   end Go;
begin
--   Put_Line("Create heap queue.");
--   Heap_Queue.Init (30);
--   Put_Line("Passed.");
--   Put_Line("");
--
--   Put_Line("----------------------------------");
--   Put_Line("Testing heap queue.");
--   Put_Line("----------------------------------");
--   Go (Heap_Queue);
--   Put_Line("----------------------------------");
--   Put_Line("");
--
--   Put_Line("Destroy heap queue.");
--   Heap_Queue.Destroy;
--   Put_Line("Passed.");
--   Put_Line("");
--
   Put_Line ("Create data queue.");
   Data_Queue.Init (Data'Unchecked_Access);
   Put_Line ("Passed.");
   Put_Line ("");

   Put_Line ("----------------------------------");
   Put_Line ("Testing data queue.");
   Put_Line ("----------------------------------");
   Go (Data_Queue);
   Put_Line ("----------------------------------");
   Put_Line ("");

   Put_Line ("Destroy data queue.");
   Data_Queue.Destroy;
   pragma Unreferenced (Data_Queue);
   Put_Line ("Passed.");
end Test;
