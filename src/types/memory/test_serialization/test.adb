with Basic_Types; use Basic_Types;
with Ada.Text_IO; use Ada.Text_IO;
with Byte_Array_Pointer.Representation;
with Byte_Array_Pointer.Assertion; use Byte_Array_Pointer.Assertion;
use Byte_Array_Pointer;
with Simple_Variable.Assertion; use Simple_Variable.Assertion;
with Simple_Variable.Representation;
with Static.Assertion; use Static.Assertion;
with Static.Representation;
with Serializer_Types; use Serializer_Types;
with Basic_Assertions; use Basic_Assertions;

-- Unit test which makes sure that the data in a buffer can be read/modified successfully.
procedure Test is

   -- Local vars:
   Heap_Ptr : Byte_Array_Pointer.Instance;
   Data_Ptr : Byte_Array_Pointer.Instance;
   B_Array : aliased Byte_Array := [0 .. 49 => 0];

   procedure Go (Ptr : in out Byte_Array_Pointer.Instance) is
      The_Slice : Byte_Array_Pointer.Instance;

      -- Serialization functions:
      function From_Static_Access is new From_Typed_Access (Static.T, Static.T_Access);
      function To_Static is new To_Type (Static.T, Static.Serialization.From_Byte_Array);
      procedure Copy_From_Static is new Copy_From_Type (Static.T, Static.Serialization.To_Byte_Array);
      function From_Variable_Access is new From_Variable_Length_Typed_Access (Simple_Variable.T, Simple_Variable.T_Access, Simple_Variable.Serialized_Length);
      function To_Variable is new To_Variable_Length_Type (Simple_Variable.T, Simple_Variable.Serialization.From_Byte_Array);
      function Copy_From_Variable is new Copy_From_Variable_Length_Type (Simple_Variable.T, Simple_Variable.Serialization.To_Byte_Array);

      -- Vars:
      Static_Var : aliased Static.T := (3, 2, 1);
      Variable_Var : aliased Simple_Variable.T := (Length => 6, Buffer => [10, 9, 8, 7, 6, 5, others => 255]);
      Variable_Var_Bad : aliased Simple_Variable.T := (Length => 255, Buffer => [10, 9, 8, 7, 6, 5, others => 255]);
      Variable_Var2 : Simple_Variable.T := (Length => 0, Buffer => [others => 0]);
      Bytes : Simple_Variable.Serialization.Byte_Array;
      Stat : Serialization_Status;
      Num_Bytes : Natural;
      Num_Bytes2 : Natural;
   begin

      Put_Line ("Test create from static type:");
      The_Slice := From_Static_Access (Static_Var'Unchecked_Access);
      Put ("slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      Copy_From_Static (The_Slice, (One => 1, Two => 2, Three => 3));
      Put ("slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy static type to ptr:");
      The_Slice := Slice (Ptr, 0, Static.Serialization.Serialized_Length - 1);
      Copy_From_Static (The_Slice, (One => 1, Two => 2, Three => 3));
      Put ("ptr: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (Ptr));
      Put ("slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      Byte_Array_Pointer_Assert.Eq (The_Slice, Static.Serialization.To_Byte_Array ((One => 1, Two => 2, Three => 3)));
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from ptr to static type:");
      Static_Var := To_Static (The_Slice);
      Put_Line (Static.Representation.Image (Static_Var));
      Static_Assert.Eq (Static_Var, (One => 1, Two => 2, Three => 3));
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test create from variable length type:");
      Stat := From_Variable_Access (The_Slice, Variable_Var_Bad'Unchecked_Access);
      Put ("bad slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      pragma Assert (Stat = Failure);
      Stat := From_Variable_Access (The_Slice, Variable_Var'Unchecked_Access);
      pragma Assert (Stat = Success);
      Put ("slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy variable length type to ptr:");
      Stat := Simple_Variable.Serialized_Length (Variable_Var, Num_Bytes);
      The_Slice := Slice (Ptr, 15, 15 + Num_Bytes - 1);
      Stat := Copy_From_Variable (The_Slice, Variable_Var);
      pragma Assert (Stat = Success);
      Put ("ptr: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (Ptr));
      Put ("slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      Stat := Simple_Variable.Serialization.To_Byte_Array (Bytes, Variable_Var, Num_Bytes2);
      pragma Assert (Stat = Success);
      Natural_Assert.Eq (Num_Bytes2, Num_Bytes);
      Byte_Array_Pointer_Assert.Eq (The_Slice, Bytes (0 .. Num_Bytes2 - 1));
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from ptr to variable length type:");
      Stat := To_Variable (Variable_Var2, The_Slice);
      pragma Assert (Stat = Success);
      Put_Line (Simple_Variable.Representation.Image (Variable_Var));
      Put_Line (Simple_Variable.Representation.Image (Variable_Var2));
      Simple_Variable_Assert.Eq (Variable_Var, Variable_Var2);
      Simple_Variable_Assert_All.Neq (Variable_Var, Variable_Var2);
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy variable length type to ptr error:");
      -- A slice that is too small should error.
      Stat := Simple_Variable.Serialized_Length (Variable_Var, Num_Bytes);
      The_Slice := Slice (Ptr, 25, 25 + Num_Bytes - 2);
      Stat := Copy_From_Variable (The_Slice, Variable_Var);
      pragma Assert (Stat = Failure);
      Put ("ptr: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (Ptr));
      Put ("slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      -- A slice that is too big should still work.
      The_Slice := Slice (Ptr, 25, 25 + Num_Bytes + 6);
      Stat := Copy_From_Variable (The_Slice, Variable_Var);
      pragma Assert (Stat = Success);
      Put ("ptr: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (Ptr));
      Put ("slice: ");
      Put_Line (Byte_Array_Pointer.Representation.To_Tuple_String (The_Slice));
      Stat := Simple_Variable.Serialization.To_Byte_Array (Bytes, Variable_Var, Num_Bytes2);
      pragma Assert (Stat = Success);
      Natural_Assert.Eq (Num_Bytes2, Num_Bytes);
      The_Slice := Slice (Ptr, 25, 25 + Num_Bytes - 1);
      Byte_Array_Pointer_Assert.Eq (The_Slice, Bytes (0 .. Num_Bytes2 - 1));
      Put_Line ("Passed.");
      New_Line;

      Put_Line ("Test copy from ptr to variable length type error:");
      -- A slice that is too small should fail:
      The_Slice := Slice (Ptr, 25, 25 + Num_Bytes - 2);
      Stat := To_Variable (Variable_Var2, The_Slice);
      pragma Assert (Stat = Failure);
      Put_Line (Simple_Variable.Representation.Image (Variable_Var));
      Put_Line (Simple_Variable.Representation.Image (Variable_Var2));
      -- A slice that is too big should still work.
      The_Slice := Slice (Ptr, 25, 25 + Num_Bytes + 6);
      Stat := To_Variable (Variable_Var2, The_Slice);
      pragma Assert (Stat = Success);
      Put_Line (Simple_Variable.Representation.Image (Variable_Var));
      Put_Line (Simple_Variable.Representation.Image (Variable_Var2));
      Simple_Variable_Assert.Eq (Variable_Var, Variable_Var2);
      Simple_Variable_Assert_All.Neq (Variable_Var, Variable_Var2);
      Put_Line ("Passed.");
      New_Line;

   end Go;

begin
   Put_Line ("Create heap ptr");
   Heap_Ptr := Byte_Array_Pointer.Create_On_Heap (50, Init_To_Zero => True);
   Put_Line (Byte_Array_Pointer.Representation.Image (Heap_Ptr));
   Put_Line ("Passed.");
   New_Line;
   Go (Heap_Ptr);
   Destroy (Heap_Ptr);

   Put_Line ("Create data ptr");
   Data_Ptr := Byte_Array_Pointer.From_Address (B_Array (B_Array'First)'Address, B_Array'Length);
   Put_Line (Byte_Array_Pointer.Representation.Image (Data_Ptr));
   Put_Line ("Passed.");
   New_Line;
   Go (Data_Ptr);
   Destroy (Data_Ptr);
   pragma Unreferenced (Data_Ptr);
end Test;
