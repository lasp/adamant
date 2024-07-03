with Basic_Types; use Basic_Types;
with Ada.Text_IO; use Ada.Text_IO;
with Byte_Array_Pointer.Representation;
with Byte_Array_Pointer.Assertion; use Byte_Array_Pointer.Assertion;
use Byte_Array_Pointer;
with System.Storage_Elements;
with String_Util;

-- Unit test which makes sure that the data in a buffer can be read/modified successfully.
procedure Test is

   -- Local vars:
   Heap_Ptr : Byte_Array_Pointer.Instance;
   Data_Ptr : Byte_Array_Pointer.Instance;
   B_Array : aliased Byte_Array := [0, 0, 0, 0, 0];
   Data_In : constant Byte_Array (0 .. 9) := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
   Data_Out : Byte_Array (0 .. 9) := [others => 0];

   procedure Go (Ptr : in out Byte_Array_Pointer.Instance) is
      procedure Reset_Buffer is
      begin
         Copy_To (Ptr, [0, 0, 0, 0, 0]);
      end Reset_Buffer;

      procedure Print_Data is
      begin
         Put_Line (String_Util.Bytes_To_String (Data_Out));
      end Print_Data;

      procedure Reset_Data is
      begin
         Data_Out := [others => 0];
      end Reset_Data;
      The_Slice : Byte_Array_Pointer.Instance;
   begin

      Put_Line ("Test copy to ptr full");
      Copy_To (Ptr, Data_In (0 .. 4));
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (Ptr, [1, 2, 3, 4, 5]);
      Reset_Buffer;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy to buffer not full");
      The_Slice := Slice (Ptr, 0, 3);
      Copy_To (The_Slice, Data_In (0 .. 3));
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (Ptr, [1, 2, 3, 4, 0]);
      Reset_Buffer;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy to buffer beginning");
      The_Slice := Slice (Ptr, 0, 2);
      Copy_To (The_Slice, [1, 2, 3]);
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (Ptr, [1, 2, 3, 0, 0]);
      Reset_Buffer;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy to buffer middle");
      The_Slice := Slice (Ptr, 1);
      Copy_To (The_Slice, [1, 2, 3, 0]);
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (The_Slice, [1, 2, 3, 0]);
      Byte_Array_Pointer_Assert.Eq (Ptr, [0, 1, 2, 3, 0]);
      Reset_Buffer;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy to buffer end");
      The_Slice := Slice (Ptr, 3);
      Copy_To (The_Slice, [1, 2]);
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (The_Slice, [1, 2]);
      Byte_Array_Pointer_Assert.Eq (Ptr, [0, 0, 0, 1, 2]);
      Reset_Buffer;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy to one size");
      The_Slice := Slice (Ptr, 3, 3);
      Copy_To (The_Slice, Data_In (4 .. 4));
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (Ptr, [0, 0, 0, 5, 0]);
      Reset_Buffer;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test zero size");
      The_Slice := Slice (Ptr, 3, 2);
      Copy_To (The_Slice, Data_In (4 .. 3));
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (Ptr, [0, 0, 0, 0, 0]);
      Reset_Buffer;
      The_Slice := Slice (Ptr, 19, 2);
      Copy_To (The_Slice, Data_In (4 .. 3));
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (Ptr, [0, 0, 0, 0, 0]);
      Reset_Buffer;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy to buffer full 2");
      The_Slice := Slice (Ptr, 0, 4);
      Copy_To (The_Slice, Data_In (0 .. 4));
      Put_Line (Byte_Array_Pointer.Representation.Image (Ptr));
      Byte_Array_Pointer_Assert.Eq (Ptr, [1, 2, 3, 4, 5]);
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from buffer full");
      Data_Out (0 .. 4) := To_Byte_Array (Ptr);
      Print_Data;
      pragma Assert (Data_Out = [1, 2, 3, 4, 5, 0, 0, 0, 0, 0]);
      Reset_Data;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from buffer full 2");
      The_Slice := Slice (Ptr, 0, 4);
      Data_Out (0 .. 4) := To_Byte_Array (The_Slice);
      Print_Data;
      pragma Assert (Data_Out = [1, 2, 3, 4, 5, 0, 0, 0, 0, 0]);
      Reset_Data;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from buffer not full");
      The_Slice := Slice (Ptr, 0, 3);
      Data_Out (0 .. 3) := To_Byte_Array (The_Slice);
      Print_Data;
      pragma Assert (Data_Out = [1, 2, 3, 4, 0, 0, 0, 0, 0, 0]);
      Reset_Data;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from middle");
      The_Slice := Slice (Ptr, 1, 3);
      Data_Out (1 .. 3) := To_Byte_Array (The_Slice);
      Print_Data;
      pragma Assert (Data_Out = [0, 2, 3, 4, 0, 0, 0, 0, 0, 0]);
      Reset_Data;
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from end");
      The_Slice := Slice (Ptr, 3, 4);
      Put_Line (Natural'Image (To_Byte_Array (The_Slice)'Length));
      Put_Line (Natural'Image (To_Byte_Array (The_Slice)'First));
      Put_Line (Natural'Image (To_Byte_Array (The_Slice)'Last));
      Data_Out (0 .. 1) := To_Byte_Array (The_Slice);
      Print_Data;
      pragma Assert (Data_Out = [4, 5, 0, 0, 0, 0, 0, 0, 0, 0]);
      Reset_Data;
      Put_Line ("Passed.");
      New_Line;

   end Go;

begin
   Put_Line ("Make sure we can create a byte array pointer at address zero:");
   Data_Ptr := Byte_Array_Pointer.From_Address (System.Storage_Elements.To_Address (0), B_Array'Length);
   pragma Assert (not Is_Null (Data_Ptr));
   Put_Line ("Passed.");

   Put_Line ("Create heap ptr");
   Heap_Ptr := Byte_Array_Pointer.Create_On_Heap (5, Init_To_Zero => True);
   Put_Line (Byte_Array_Pointer.Representation.Image (Heap_Ptr));
   Put_Line ("Passed.");
   New_Line;
   Go (Heap_Ptr);

   Put_Line ("Create data ptr");
   Data_Ptr := Byte_Array_Pointer.From_Address (B_Array (B_Array'First)'Address, B_Array'Length);
   Put_Line (Byte_Array_Pointer.Representation.Image (Data_Ptr));
   Put_Line ("Passed.");
   New_Line;
   Go (Data_Ptr);
   pragma Unreferenced (Data_Ptr);
end Test;
