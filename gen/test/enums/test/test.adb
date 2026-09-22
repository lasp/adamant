with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use type Interfaces.C.int;
with Test_Enums; use Test_Enums;
with Test_Enums.Representation; use Test_Enums.Representation;
with Test_Enums.Assertion; use Test_Enums.Assertion;

procedure Test is
   Fe : First_Enum.E := First_Enum.Black;
   Se : Second_Enum.E := Second_Enum.Hola;

   -- Read an E_C out of a C int object, the way a value returned from C
   -- arrives, and check that To_Ada maps it to the expected literal.
   procedure Check_From_C (Value : in Interfaces.C.int; Expected : in First_Enum.E) is
      Raw : aliased Interfaces.C.int := Value;
      From_C : First_Enum.C.E_C with Import, Address => Raw'Address;
   begin
      pragma Assert (From_C'Valid);
      First_Enum_Assert.Eq (First_Enum.C.To_Ada (From_C), Expected);
      Put_Line ("First_Enum.C.To_Ada mapped" & Interfaces.C.int'Image (Value) & " to " & First_Enum_Image (Expected));
   end Check_From_C;

   procedure Check_From_C (Value : in Interfaces.C.int; Expected : in Second_Enum.E) is
      Raw : aliased Interfaces.C.int := Value;
      From_C : Second_Enum.C.E_C with Import, Address => Raw'Address;
   begin
      pragma Assert (From_C'Valid);
      Second_Enum_Assert.Eq (Second_Enum.C.To_Ada (From_C), Expected);
      Put_Line ("Second_Enum.C.To_Ada mapped" & Interfaces.C.int'Image (Value) & " to " & Second_Enum_Image (Expected));
   end Check_From_C;

   -- To_Ada must reject a C int that is not a literal of the enumeration.
   procedure Check_Invalid_First_Enum (Value : in Interfaces.C.int) is
      Raw : aliased Interfaces.C.int := Value;
      From_C : First_Enum.C.E_C with Import, Address => Raw'Address;
      Ignore : First_Enum.E;
   begin
      pragma Assert (not From_C'Valid);
      Ignore := First_Enum.C.To_Ada (From_C);
      Put_Line ("FAIL: First_Enum.C.To_Ada accepted" & Interfaces.C.int'Image (Value));
   exception
      when Constraint_Error =>
         Put_Line ("First_Enum.C.To_Ada rejected" & Interfaces.C.int'Image (Value));
   end Check_Invalid_First_Enum;

   procedure Check_Invalid_Second_Enum (Value : in Interfaces.C.int) is
      Raw : aliased Interfaces.C.int := Value;
      From_C : Second_Enum.C.E_C with Import, Address => Raw'Address;
      Ignore : Second_Enum.E;
   begin
      pragma Assert (not From_C'Valid);
      Ignore := Second_Enum.C.To_Ada (From_C);
      Put_Line ("FAIL: Second_Enum.C.To_Ada accepted" & Interfaces.C.int'Image (Value));
   exception
      when Constraint_Error =>
         Put_Line ("Second_Enum.C.To_Ada rejected" & Interfaces.C.int'Image (Value));
   end Check_Invalid_Second_Enum;
begin
   Put_Line ("Testing enumerations: ");
   Put_Line ("");

   Put_Line ("First_Enum: " & First_Enum_Image (Fe));
   Put_Line ("Second_Enum: " & Second_Enum_Image (Se));

   Fe := First_Enum.Blue;
   Se := Second_Enum.Yellow;

   Put_Line ("First_Enum: " & First_Enum_Image (Fe));
   Put_Line ("Second_Enum: " & Second_Enum_Image (Se));

   First_Enum_Assert.Eq (Fe, First_Enum.Blue);
   Second_Enum_Assert.Eq (Se, Second_Enum.Yellow);

   Put_Line ("");
   Put_Line ("Testing C versions: ");
   Put_Line ("");

   -- The C version is passed to C as an int, so it must have the size of one.
   pragma Assert (First_Enum.C.E_C'Object_Size = Interfaces.C.int'Size);
   pragma Assert (Second_Enum.C.E_C'Object_Size = Interfaces.C.int'Size);
   Put_Line ("E_C'Object_Size: " & Natural'Image (First_Enum.C.E_C'Object_Size));

   -- Every literal converts to a C version literal with the same name and
   -- value, and converts back to itself.
   for Literal in First_Enum.E loop
      declare
         C_Literal : constant First_Enum.C.E_C := First_Enum.C.To_C (Literal);
      begin
         pragma Assert (First_Enum.C.E_C'Enum_Rep (C_Literal) = First_Enum.E'Enum_Rep (Literal));
         pragma Assert (First_Enum.C.E_C'Image (C_Literal) = First_Enum.E'Image (Literal));
         First_Enum_Assert.Eq (First_Enum.C.To_Ada (C_Literal), Literal);
         Put_Line ("First_Enum: " & First_Enum_Image (Literal) & " <-> " & First_Enum.C.E_C'Image (C_Literal));
      end;
   end loop;

   for Literal in Second_Enum.E loop
      declare
         C_Literal : constant Second_Enum.C.E_C := Second_Enum.C.To_C (Literal);
      begin
         pragma Assert (Second_Enum.C.E_C'Enum_Rep (C_Literal) = Second_Enum.E'Enum_Rep (Literal));
         pragma Assert (Second_Enum.C.E_C'Image (C_Literal) = Second_Enum.E'Image (Literal));
         Second_Enum_Assert.Eq (Second_Enum.C.To_Ada (C_Literal), Literal);
         Put_Line ("Second_Enum: " & Second_Enum_Image (Literal) & " <-> " & Second_Enum.C.E_C'Image (C_Literal));
      end;
   end loop;

   -- A C int holding a defined value maps to the matching literal.
   Check_From_C (0, First_Enum.Red);
   Check_From_C (3, First_Enum.Black);
   Check_From_C (0, Second_Enum.Hola);
   Check_From_C (2, Second_Enum.Off);
   Check_From_C (10, Second_Enum.Yellow);

   -- A C int that the enumeration does not define is rejected by To_Ada.
   -- First_Enum uses the contiguous values 0 .. 3, Second_Enum uses 0 .. 2
   -- and 10, so the gap and both ends of each are covered.
   Check_Invalid_First_Enum (4);
   Check_Invalid_First_Enum (-1);
   Check_Invalid_First_Enum (Interfaces.C.int'Last);
   Check_Invalid_Second_Enum (3);
   Check_Invalid_Second_Enum (9);
   Check_Invalid_Second_Enum (11);
   Check_Invalid_Second_Enum (-1);

   Put_Line ("");
   Put_Line ("Done.");
   --  Sentinel for the cross test runner.
   Put_Line ("=== ALL TESTS PASSED ===");
end Test;
