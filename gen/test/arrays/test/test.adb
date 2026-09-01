with Ada.Text_IO; use Ada.Text_IO;
with Always_Valid_Array;
with Always_Valid_Record_Array;
with Simple_Array.Representation;
with Complex_Array.Representation;
with Complex_Array_Le.Representation;
with Eight_Bit_Type_Array.Representation;
with Unaligned_Array.Representation;
with Enum_Array.Representation;
with Simple_Array.Validation;
with Simple_Array.C; use Simple_Array.C;
with Complex_Array.Validation;
with Complex_Array.C; use Complex_Array.C;
with Complex_Array_Le.C; use Complex_Array_Le.C;
with Eight_Bit_Type_Array.Validation;
with Unaligned_Array.Validation;
with Enum_Array.Validation;
with Float_Array.Validation;
with Complex_Float_Array.Validation;
with Complex_Array_Le.Validation;
with Float_Array;
with Float_Array.C;
with Complex_Float_Array;
with Ada.Unchecked_Conversion;
with Simple_Array.Assertion; use Simple_Array.Assertion;
with Float_Array.Assertion; use Float_Array.Assertion;
with Complex_Array.Assertion; use Complex_Array.Assertion;
with Complex_Array_Le.Assertion; use Complex_Array_Le.Assertion;
with Complex_Float_Array.Assertion; use Complex_Float_Array.Assertion;
with Eight_Bit_Type_Array.Assertion; use Eight_Bit_Type_Array.Assertion;
with Unaligned_Array.Assertion; use Unaligned_Array.Assertion;
with Enum_Array.Assertion; use Enum_Array.Assertion;
with Basic_Types; use Basic_Types;
with String_Util;
with Register_Array.Representation;
with Interfaces; use Interfaces;
with Test_Enums; use Test_Enums;

procedure Test is
   -- Helper packages:
   function Poly2bytestring is new String_Util.To_Byte_String (Poly_Type);

   -- Array definitions:
   S_Bytes : Simple_Array.Serialization.Byte_Array := [0 => 0, 1 => 1, 2 => 0, 3 => 1, 4 => 255, others => 0];
   C_Bytes : Complex_Array.Serialization.Byte_Array := [0 => 0, 1 => 19, 2 => 0, 3 => 6, 4 => 5, 5 => 9, others => 0];
   C_Le_Bytes : Complex_Array_Le.Serialization_Le.Byte_Array := [0 => 0, 1 => 19, 2 => 5, 3 => 0, 4 => 6, 5 => 0, others => 0];
   E_Bytes : Eight_Bit_Type_Array.Serialization.Byte_Array := [0 .. 10 => 1, 11 => 0, others => 1];
   U_Bytes : Unaligned_Array.Serialization.Byte_Array := [0 .. 2 => 255, others => 0];
   En_Bytes : Enum_Array.Serialization.Byte_Array := [0, 1, 3, 4, 233];

   -- Packed arrays:
   Simple : constant Simple_Array.T := [others => 1];
   Reg_Array : constant Register_Array.Atomic_T := [others => 1];
   Complex : constant Complex_Array.T := [others => (One => 0, Two => 19, Three => 5)];
   Complex_Le : constant Complex_Array_Le.T_Le := [others => (One => 0, Two => 19, Three => 5)];
   Eight : constant Eight_Bit_Type_Array.T := [others => [others => 1]];
   Unaligned : constant Unaligned_Array.T := [1 .. 4 => 1, others => 98];
   Enum : constant Enum_Array.T := [others => First_Enum.Black];
   Simple_Mut : Simple_Array.T := Simple;
   Simple_U : Simple_Array.U := [others => 2];
   Simple_C : Simple_Array.C.U_C := [others => 3];
   Simple_Le : Simple_Array.T_Le := [others => 2];
   Complex_Mut : Complex_Array.T := Complex;
   Complex_U : Complex_Array.U := [others => (One => 0, Two => 19, Three => 6)];
   Complex_C : Complex_Array.C.U_C := [others => (One => 0, Two => 20, Three => 7)];
   Complex_Le_Mut : Complex_Array_Le.T_Le := Complex_Le;
   Complex_Le_U : Complex_Array_Le.U := [others => (One => 0, Two => 19, Three => 6)];
   Complex_Le_C : Complex_Array_Le.C.U_C := [others => (One => 0, Two => 20, Three => 7)];
   Eight_Mut : Eight_Bit_Type_Array.T := Eight;
   Unaligned_Mut : Unaligned_Array.T := Unaligned;
   Enum_Mut : Enum_Array.T := Enum;
   Simple2 : Simple_Array.T := [others => 2];
   Complex2 : Complex_Array.T := [others => (One => 0, Two => 21, Three => 6)];
   Complex2_Le : Complex_Array_Le.T_Le := [others => (One => 0, Two => 21, Three => 6)];
   Eight2 : Eight_Bit_Type_Array.T := [others => [others => 2]];
   Unaligned2 : Unaligned_Array.T := [others => 0];
   Enum2 : Enum_Array.T := [others => First_Enum.Blue];
   Flt : constant Float_Array.T := [others => 1.1];
   Complex_Flt : constant Complex_Float_Array.T := [others => (Yo => 17, F => (One => 5, Two => 21.5, Three => 50.2345))];
   Flt_U : constant Float_Array.U := [others => 1.1];
   Flt_Le : constant Float_Array.T_Le := [others => 1.1];
   Complex_Flt_U : constant Complex_Float_Array.U := [others => (Yo => 17, F => (One => 5, Two => 21.5, Three => 50.2345))];

   -- Other local vars:
   Ignore : Unsigned_32;
   Field_Number : Unsigned_32;
begin
   -- Always_Valid compile-time checks for arrays:
   pragma Compile_Time_Error (not Always_Valid_Array.Always_Valid, "Expected Always_Valid = True for full-width scalar array");
   pragma Compile_Time_Error (not Always_Valid_Record_Array.Always_Valid, "Expected Always_Valid = True for array of always-valid records");
   -- Note: Eight_Bit_Type_Array uses an arrayed primitive element (U8x8), so
   -- Always_Valid conservatively evaluates to False (cannot introspect component type).
   pragma Compile_Time_Error (Eight_Bit_Type_Array.Always_Valid /= False, "Expected Always_Valid = False for arrayed primitive element");
   pragma Compile_Time_Error (Simple_Array.Always_Valid /= False, "Expected Always_Valid = False for bit-constrained scalar array");
   pragma Compile_Time_Error (Complex_Array.Always_Valid /= False, "Expected Always_Valid = False for array of bit-constrained records");
   pragma Compile_Time_Error (Complex_Array_Le.Always_Valid /= False, "Expected Always_Valid = False for LE array of bit-constrained records");
   pragma Compile_Time_Error (Float_Array.Always_Valid /= False, "Expected Always_Valid = False for float array");
   pragma Compile_Time_Error (Enum_Array.Always_Valid /= False, "Expected Always_Valid = False for enum array");

   -- Always_Valid runtime assertions for arrays:
   Put_Line ("Testing Always_Valid compile-time constant for arrays:");
   pragma Assert (Always_Valid_Array.Always_Valid);
   pragma Assert (Always_Valid_Record_Array.Always_Valid);
   pragma Assert (Eight_Bit_Type_Array.Always_Valid = False);
   pragma Assert (Simple_Array.Always_Valid = False);
   pragma Assert (Complex_Array.Always_Valid = False);
   pragma Assert (Complex_Array_Le.Always_Valid = False);
   pragma Assert (Float_Array.Always_Valid = False);
   pragma Assert (Enum_Array.Always_Valid = False);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Printing arrays: ");
   Put_Line ("Simple: ");
   Put_Line (Simple_Array.Representation.Image (Simple));
   Put_Line ("Simple Tuple: ");
   Put_Line (Simple_Array.Representation.To_Tuple_String (Simple));
   Put_Line ("Reg: ");
   Put_Line (Register_Array.Representation.To_Tuple_String (Register_Array.T (Reg_Array)));
   Put_Line ("Complex: ");
   Put_Line (Complex_Array.Representation.Image (Complex));
   Put_Line ("Complex Tuple: ");
   Put_Line (Complex_Array.Representation.To_Tuple_String (Complex));
   Put_Line ("Complex_Le: ");
   Put_Line (Complex_Array_Le.Representation.Image (Complex_Le));
   Put_Line ("Complex_Le Tuple: ");
   Put_Line (Complex_Array_Le.Representation.To_Tuple_String (Complex_Le));
   Put_Line ("Eight: ");
   Put_Line (Eight_Bit_Type_Array.Representation.Image (Eight));
   Put_Line ("Eight Tuple: ");
   Put_Line (Eight_Bit_Type_Array.Representation.To_Tuple_String (Eight));
   Put_Line ("Unaligned: ");
   Put_Line (Unaligned_Array.Representation.Image (Unaligned));
   Put_Line ("Unaligned Tuple: ");
   Put_Line (Unaligned_Array.Representation.To_Tuple_String (Unaligned));
   Put_Line ("Enum: ");
   Put_Line (Enum_Array.Representation.Image (Enum));
   Put_Line ("Enum Tuple: ");
   Put_Line (Enum_Array.Representation.To_Tuple_String (Enum));
   Put_Line ("passed.");

   Put_Line ("Testing simple array... ");
   Simple_Mut (5) := 27;
   Simple_Mut (12) := 8;
   Put_Line (Simple_Array.Representation.Image (Simple_Mut));
   Put_Line ("passed.");

   Put ("Testing complex array... ");
   Complex_Mut (5) := (One => 1, Two => 20, Three => 6);
   Complex_Mut (6) := (One => 2, Two => 21, Three => 7);
   Put_Line (Complex_Array.Representation.Image (Complex_Mut));
   Put_Line ("passed.");

   Put ("Testing complex array LE... ");
   Complex_Le_Mut (5) := (One => 1, Two => 20, Three => 6);
   Complex_Le_Mut (6) := (One => 2, Two => 21, Three => 7);
   Put_Line (Complex_Array_Le.Representation.Image (Complex_Le_Mut));
   Put_Line ("passed.");

   Put ("Testing unaligned array... ");
   Unaligned_Mut (1) := 44;
   Put_Line (Unaligned_Array.Representation.Image (Unaligned_Mut));
   Put_Line ("passed.");

   Put ("Testing enum array... ");
   Enum_Mut (1) := First_Enum.Red;
   Put_Line (Enum_Array.Representation.Image (Enum_Mut));
   Put_Line ("passed.");

   Put_Line ("Validating arrays: ");
   pragma Assert (Simple_Array.Validation.Valid (Simple_Array.Serialization.To_Byte_Array (Simple_Mut), Ignore), "Simple is not valid, but should be.");
   pragma Assert (Complex_Array.Validation.Valid (Complex_Array.Serialization.To_Byte_Array (Complex_Mut), Ignore), "Complex is not valid, but should be.");
   pragma Assert (Eight_Bit_Type_Array.Validation.Valid (Eight_Bit_Type_Array.Serialization.To_Byte_Array (Eight_Mut), Ignore), "Eight is not valid, but should be.");
   pragma Assert (Unaligned_Array.Validation.Valid (Unaligned_Array.Serialization.To_Byte_Array (Unaligned_Mut), Ignore), "Unaligned is not valid, but should be.");
   pragma Assert (Enum_Array.Validation.Valid (Enum_Array.Serialization.To_Byte_Array (Enum_Mut), Ignore), "Enum is not valid, but should be.");
   -- TODO Validation for variable
   Put_Line ("passed.");

   Put_Line ("Altering arrays to invalid ranges: ");
   Simple_Mut := Simple_Array.Serialization.From_Byte_Array (S_Bytes);
   Complex_Mut := Complex_Array.Serialization.From_Byte_Array (C_Bytes);
   Eight_Mut := Eight_Bit_Type_Array.Serialization.From_Byte_Array (E_Bytes);
   Unaligned_Mut := Unaligned_Array.Serialization.From_Byte_Array (U_Bytes);
   Enum_Mut := Enum_Array.Serialization.From_Byte_Array (En_Bytes);
   Put_Line ("passed.");

   Put_Line ("Validating arrays (expect failure): ");
   declare
      Simple_Inv : Simple_Array.Serialization.Byte_Array with Import, Convention => Ada, Address => Simple_Mut'Address;
      Complex_Inv : Complex_Array.Serialization.Byte_Array with Import, Convention => Ada, Address => Complex_Mut'Address;
      Eight_Inv : Eight_Bit_Type_Array.Serialization.Byte_Array with Import, Convention => Ada, Address => Eight_Mut'Address;
      Unaligned_Inv : Unaligned_Array.Serialization.Byte_Array with Import, Convention => Ada, Address => Unaligned_Mut'Address;
      Enum_Inv : Enum_Array.Serialization.Byte_Array with Import, Convention => Ada, Address => Enum_Mut'Address;
   begin
      pragma Assert (not Simple_Array.Validation.Valid (Simple_Inv, Field_Number), "Simple is valid, but should not be.");
      pragma Assert (Field_Number = 3, "Simple field_Number is wrong.");
      Put_Line (Poly2bytestring (Simple_Array.Validation.Get_Field (Simple_Inv, Field_Number)));
      pragma Assert (Simple_Array.Validation.Get_Field (Simple_Inv, Field_Number) = [0, 0, 0, 0, 0, 255, 0, 0], "Simple's polytype field is wrong."); -- represented by 32-bit number in little endian
      pragma Assert (not Complex_Array.Validation.Valid (Complex_Inv, Field_Number), "Complex is valid, but should not be.");
      pragma Assert (Field_Number = 5, "Complex field_Number is wrong.");
      Put_Line (Poly2bytestring (Complex_Array.Validation.Get_Field (Complex_Inv, Field_Number)));
      pragma Assert (Complex_Array.Validation.Get_Field (Complex_Inv, Field_Number) = [0, 0, 0, 0, 9, 0, 0, 0], "Complex's polytype field is wrong."); -- represented by 32-bit number in little endian
      pragma Assert (Complex_Array.Validation.Valid (Complex_Inv, Ignore, Complex_Mut'First, Complex_Mut'First), "Complex is invalid, but should not be.");
      pragma Assert (not Eight_Bit_Type_Array.Validation.Valid (Eight_Inv, Field_Number), "Eight is valid, but should not be.");
      pragma Assert (Field_Number = 2, "Eight field_Number is wrong.");
      Put_Line (Poly2bytestring (Eight_Bit_Type_Array.Validation.Get_Field (Eight_Inv, Field_Number)));
      pragma Assert (Eight_Bit_Type_Array.Validation.Get_Field (Eight_Inv, Field_Number) = [1, 1, 1, 00, 1, 1, 1, 1], "Eight's polytype field is wrong."); -- represented by 32-bit number in little endian
      pragma Assert (not Unaligned_Array.Validation.Valid (Unaligned_Inv, Field_Number), "Unaligned is valid, but should not be.");
      pragma Assert (Field_Number = 1, "Unaligned field_Number is wrong. ");
      Put_Line (Poly2bytestring (Unaligned_Array.Validation.Get_Field (Unaligned_Inv, Field_Number)));
      pragma Assert (Unaligned_Array.Validation.Get_Field (Unaligned_Inv, Field_Number) = [0, 0, 0, 00, 255, 03, 0, 0], "Unaligned's polytype field is wrong."); -- represented by 32-bit number in little endian
      pragma Assert (not Enum_Array.Validation.Valid (Enum_Inv, Field_Number), "Enum is valid, but should not be.");
      pragma Assert (Field_Number = 4, "Enum field_Number is wrong. " & Natural'Image (Natural (Field_Number)));
      Put_Line (Poly2bytestring (Enum_Array.Validation.Get_Field (Enum_Inv, Field_Number)));
      pragma Assert (Enum_Array.Validation.Get_Field (Enum_Inv, Field_Number) = [0, 0, 0, 00, 0, 0, 0, 04], "Enum's polytype field is wrong."); -- represented by 32-bit number in little endian
      pragma Assert (Enum_Array.Validation.Valid (Enum_Inv, Ignore, Enum_Mut'First, Enum_Mut'First + 1), "Enum is invalid, but should not be 1.");
      pragma Assert (Enum_Array.Validation.Valid (Enum_Inv, Ignore, Enum_Mut'First + 1, Enum_Mut'First + 2), "Enum is invalid, but should not be 2.");
      pragma Assert (Enum_Array.Validation.Valid (Enum_Inv, Ignore, Enum_Mut'First, Enum_Mut'Last - 3), "Enum is invalid, but should not be 3.");
      pragma Assert (Enum_Array.Validation.Valid (Enum_Inv, Ignore, Enum_Mut'First, Enum_Mut'Last - 2), "Enum is invalid, but should not be 4.");
      pragma Assert (not Enum_Array.Validation.Valid (Enum_Inv, Ignore, Enum_Mut'First, Enum_Mut'Last), "Enum is valid, but should not be 5.");
      pragma Assert (not Enum_Array.Validation.Valid (Enum_Inv, Ignore, Enum_Mut'First, Enum_Mut'Last), "Enum is valid, but should not be 6.");
      pragma Assert (not Enum_Array.Validation.Valid (Enum_Inv, Ignore, Enum_Mut'Last - 2, Enum_Mut'Last), "Enum is valid, but should not be 7.");
   end;
   Put_Line ("passed.");

   -- Verify that Get_Field maps field numbers to the correct array element
   Put_Line ("Testing Get_Field round-trip for packed array element index: ");
   declare
      -- Complex_Array elements are Aa.T with 3 sub-fields each (One, Two, Three).
      -- Craft bytes so element 0 has valid One/Two but invalid Three (= 3, out of range 5..2056).
      -- This makes field 3 (element 0, sub-field 3) the first invalid field.
      -- Field 3 is a multiple of num_fields, which exercises the element index edge case.
      C_Bytes_Rt : constant Complex_Array.Serialization.Byte_Array := [0 => 0, 1 => 19, 2 => 0, 3 => 3, others => 0];
      Complex_Rt : constant Complex_Array.T := Complex_Array.Serialization.From_Byte_Array (C_Bytes_Rt);
      Complex_Rt_Inv : Complex_Array.Serialization.Byte_Array with Import, Convention => Ada, Address => Complex_Rt'Address;
   begin
      pragma Assert (not Complex_Array.Validation.Valid (Complex_Rt_Inv, Field_Number), "Complex_Rt is valid, but should not be.");
      pragma Assert (Field_Number = 3, "Complex_Rt field_Number is wrong.");
      Put_Line (Poly2bytestring (Complex_Array.Validation.Get_Field (Complex_Rt_Inv, Field_Number)));
      pragma Assert (Complex_Array.Validation.Get_Field (Complex_Rt_Inv, Field_Number) = [0, 0, 0, 0, 3, 0, 0, 0], "Get_Field value mismatch.");
   end;
   Put_Line ("passed.");

   Put_Line ("Testing serialization/deserialization... ");
   S_Bytes := Simple_Array.Serialization.To_Byte_Array (Simple);
   Simple2 := Simple_Array.Serialization.From_Byte_Array (S_Bytes);
   Simple_Array_Assert.Eq (Simple, Simple2);
   C_Bytes := Complex_Array.Serialization.To_Byte_Array (Complex);
   Complex2 := Complex_Array.Serialization.From_Byte_Array (C_Bytes);
   Complex_Array_Assert.Eq (Complex, Complex2);
   C_Le_Bytes := Complex_Array_Le.Serialization_Le.To_Byte_Array (Complex_Le);
   Complex2_Le := Complex_Array_Le.Serialization_Le.From_Byte_Array (C_Le_Bytes);
   Complex_Array_Le_Le_Assert.Eq (Complex_Le, Complex2_Le);
   E_Bytes := Eight_Bit_Type_Array.Serialization.To_Byte_Array (Eight);
   Eight2 := Eight_Bit_Type_Array.Serialization.From_Byte_Array (E_Bytes);
   Eight_Bit_Type_Array_Assert.Eq (Eight, Eight2);
   U_Bytes := Unaligned_Array.Serialization.To_Byte_Array (Unaligned);
   Unaligned2 := Unaligned_Array.Serialization.From_Byte_Array (U_Bytes);
   Unaligned_Array_Assert.Eq (Unaligned, Unaligned2);
   En_Bytes := Enum_Array.Serialization.To_Byte_Array (Enum);
   Enum2 := Enum_Array.Serialization.From_Byte_Array (En_Bytes);
   Enum_Array_Assert.Eq (Enum, Enum2);
   Put_Line ("passed.");

   Put_Line ("Testing endianness... ");
   Put_Line (Unaligned_Array.Representation.Image (Unaligned));
   Put_Line (Unaligned_Array.Representation.Image (Unaligned_Array.T_Le (Unaligned)));
   Unaligned_Array_Le_Assert.Eq (Unaligned_Array.T_Le (Unaligned), Unaligned_Array.T_Le (Unaligned));
   Put_Line ("passed.");

   Put_Line ("Pack/unpack test: ");
   Simple_Mut := [others => 1];
   Simple_U := Simple_Array.Unpack (Simple_Mut);
   Put_Line ("Simple:");
   -- Put_Line (Simple_Mut'Image);
   Put_Line (Simple_Array.Representation.Image (Simple_Mut));
   Put_Line ("Simple_U:");
   -- Put_Line (Simple_U'Image);
   Put_Line (Simple_Array.Representation.Image (Simple_U));
   Simple_Array_U_Assert.Eq (Simple_U, [others => 1]);
   Simple_Mut := Simple_Array.Pack (Simple_U);
   Put_Line ("Simple:");
   Put_Line (Simple_Array.Representation.Image (Simple_Mut));
   Simple_Array_Assert.Eq (Simple_Mut, [others => 1]);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Swap endianness:");
   Simple_Le := Simple_Array.Swap_Endianness (Simple_Mut);
   Put_Line ("Simple_Le:");
   Put_Line (Simple_Array.Representation.Image (Simple_Le));
   Simple_Array_Le_Assert.Eq (Simple_Le, [others => 1]);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Pack/unpack test (nested): ");
   Complex_Mut := [others => (One => 0, Two => 19, Three => 5)];
   Complex_U := Complex_Array.Unpack (Complex_Mut);
   Put_Line ("Complex:");
   Put_Line (Complex_Array.Representation.Image (Complex_Mut));
   Put_Line ("Complex_U:");
   Put_Line (Complex_Array.Representation.Image (Complex_U));
   Complex_Array_U_Assert.Eq (Complex_U, [others => (One => 0, Two => 19, Three => 5)]);
   Complex_Mut := Complex_Array.Pack (Complex_U);
   Put_Line ("Complex:");
   Put_Line (Complex_Array.Representation.Image (Complex_Mut));
   Complex_Array_Assert.Eq (Complex_Mut, [others => (One => 0, Two => 19, Three => 5)]);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Pack/unpack test (nested LE): ");
   Complex_Le_Mut := [others => (One => 0, Two => 19, Three => 5)];
   Complex_Le_U := Complex_Array_Le.Unpack (Complex_Le_Mut);
   Put_Line ("Complex_Le:");
   Put_Line (Complex_Array_Le.Representation.Image (Complex_Le_Mut));
   Put_Line ("Complex_Le_U:");
   Put_Line (Complex_Array_Le.Representation.Image (Complex_Le_U));
   Complex_Array_Le_U_Assert.Eq (Complex_Le_U, [others => (One => 0, Two => 19, Three => 5)]);
   Complex_Le_Mut := Complex_Array_Le.Pack (Complex_Le_U);
   Put_Line ("Complex_Le:");
   Put_Line (Complex_Array_Le.Representation.Image (Complex_Le_Mut));
   Complex_Array_Le_Le_Assert.Eq (Complex_Le_Mut, [others => (One => 0, Two => 19, Three => 5)]);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("C conversion test: ");
   Simple_U := [others => 1];
   Simple_C := To_C (Simple_U);
   Put_Line ("Simple_U:");
   Put_Line (Simple_Array.Representation.Image (Simple_U));
   Put_Line ("Simple_C:");
   Put_Line (Simple_C'Image);
   Simple_C := [others => 3];
   Simple_U := To_Ada (Simple_C);
   Put_Line ("Simple_U:");
   Put_Line (Simple_Array.Representation.Image (Simple_U));
   Put_Line ("Simple_C:");
   Put_Line (Simple_C'Image);
   Simple_Array_U_Assert.Eq (Simple_U, [others => 3]);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("C conversion test 2: ");
   Complex_U := [others => (One => 0, Two => 19, Three => 6)];
   Complex_C := To_C (Complex_U);
   Put_Line ("Complex_U:");
   Put_Line (Complex_Array.Representation.Image (Complex_U));
   Put_Line ("Complex_C:");
   Put_Line (Complex_C'Image);
   Complex_C := [others => (One => 0, Two => 20, Three => 7)];
   Complex_U := To_Ada (Complex_C);
   Put_Line ("Complex_U:");
   Put_Line (Complex_Array.Representation.Image (Complex_U));
   Put_Line ("Complex_C:");
   Put_Line (Complex_C'Image);
   Complex_Array_U_Assert.Eq (Complex_U, [others => (One => 0, Two => 20, Three => 7)]);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("C conversion test LE: ");
   Complex_Le_U := [others => (One => 0, Two => 19, Three => 6)];
   Complex_Le_C := To_C (Complex_Le_U);
   Put_Line ("Complex_Le_U:");
   Put_Line (Complex_Array_Le.Representation.Image (Complex_Le_U));
   Put_Line ("Complex_Le_C:");
   Put_Line (Complex_Le_C'Image);
   Complex_Le_C := [others => (One => 0, Two => 20, Three => 7)];
   Complex_Le_U := To_Ada (Complex_Le_C);
   Put_Line ("Complex_Le_U:");
   Put_Line (Complex_Array_Le.Representation.Image (Complex_Le_U));
   Put_Line ("Complex_Le_C:");
   Put_Line (Complex_Le_C'Image);
   Complex_Array_Le_U_Assert.Eq (Complex_Le_U, [others => (One => 0, Two => 20, Three => 7)]);
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Floating point assertion test: ");
   Float_Array_Assert.Eq (Flt, [others => 5.0], Epsilon => 50.0);
   Float_Array_U_Assert.Eq (Flt_U, [others => 4.0], Epsilon => 50.0);
   Float_Array_Le_Assert.Eq (Flt_Le, [others => 3.0], Epsilon => 50.0);
   Float_Array_Le_Assert.Eq (Flt_Le, [0 => 1.1, 1 => 1.1, 2 => 5.1, 3 => 0.4, others => 3.0], Epsilon => 50.0);
   Complex_Float_Array_Assert.Eq (Complex_Flt, [others => (Yo => 17, F => (One => 5, Two => 21.5, Three => 50.23458))], Epsilon => 0.1);
   Complex_Float_Array_U_Assert.Eq (Complex_Flt_U, [others => (Yo => 17, F => (One => 5, Two => 21.5, Three => 50.23459))], Epsilon => 0.2);
   Put_Line ("passed.");
   Put_Line ("");

   --  Regression test for the .C.Pack / .C.Unpack round-trip across the
   --  Scalar_Storage_Order boundary on Short_Float arrays.
   --
   --  Background: GNAT for some targets (observed on bareboard) does not
   --  byte-swap Short_Float reads inside an iterated component association
   --  whose source array has Scalar_Storage_Order /= host's. The
   --  resulting unswapped element is then 'Valid-checked. For values
   --  whose bit pattern, byte-reversed, lands on a NaN exponent (sign+
   --  exponent = 0x7F8 / 0xFF8 with nonzero mantissa), the check fails
   --  and the unpack raises CONSTRAINT_ERROR : invalid data.
   --
   --  9.45784093e-4 has bit pattern 0x3A77EE7F whose byte reverse,
   --  0x7FEE773A, is exactly such a NaN. We exercise it at multiple
   --  positions in the array, plus its negation 0xBA77EE7F (also a
   --  NaN-on-reverse), and confirm Pack -> bytes are BE-encoded and
   --  Unpack -> values bit-exactly match the input.
   Put_Line ("Float_Array .C SSO byte-swap round-trip test:");
   declare
      function To_U32 is new Ada.Unchecked_Conversion (Short_Float, Unsigned_32);

      Trap_Pos : constant Short_Float :=  9.45784093e-4;  -- 0x3A77EE7F
      Trap_Neg : constant Short_Float := -9.45784093e-4;  -- 0xBA77EE7F
      Other    : constant Short_Float :=  3.24796274e-4;  -- 0x39AA496B (control)

      Source : constant Float_Array.C.U_C := [
         0  => Trap_Pos, 1 => Other,    2 => 0.0,      3 => Trap_Pos,
         4  => Trap_Neg, 5 => 1.0,      6 => Trap_Pos, 7 => Trap_Pos,
         8  => Trap_Pos, 9 => -1.5,    10 => Trap_Pos, 11 => Trap_Neg
      ];

      --  BE direction: this is the path that crashed in the field.
      Be_Packed : constant Float_Array.T   := Float_Array.C.Pack (Source);
      Be_Round  : constant Float_Array.C.U_C := Float_Array.C.Unpack (Be_Packed);

      --  Inspect raw bytes of the BE-packed array via overlay.
      Be_Bytes  : Basic_Types.Byte_Array (0 .. Float_Array.Size_In_Bytes - 1)
         with Import, Convention => Ada, Address => Be_Packed'Address;

      --  LE direction: same payload, opposite storage order. Should also
      --  round-trip; on a LE host this path involves no SSO crossing.
      Le_Packed : constant Float_Array.T_Le := Float_Array.C.Pack (Source);
      Le_Round  : constant Float_Array.C.U_C := Float_Array.C.Unpack (Le_Packed);
   begin
      --  First element is the trap value; verify BE-encoding directly.
      pragma Assert (Be_Bytes (0) = 16#3A# and then Be_Bytes (1) = 16#77#
                     and then Be_Bytes (2) = 16#EE# and then Be_Bytes (3) = 16#7F#,
         "Float_Array.C.Pack did not produce BE bytes for trap value");

      --  Bit-exact round-trip on every element, both directions.
      for J in Source'Range loop
         pragma Assert (To_U32 (Be_Round (J)) = To_U32 (Source (J)),
            "BE Pack/Unpack round-trip mismatch at index" & J'Image);
         pragma Assert (To_U32 (Le_Round (J)) = To_U32 (Source (J)),
            "LE Pack/Unpack round-trip mismatch at index" & J'Image);
      end loop;

      --  Same trap value at a non-zero offset to defeat any aggregate-
      --  level memcpy optimization that might happen to start aligned.
      declare
         Shifted : constant Float_Array.C.U_C := [
            0 => 0.0, 1 => 0.0, 2 => Trap_Pos, others => Other
         ];
         Be2 : constant Float_Array.T   := Float_Array.C.Pack (Shifted);
         R2  : constant Float_Array.C.U_C := Float_Array.C.Unpack (Be2);
      begin
         for J in Shifted'Range loop
            pragma Assert (To_U32 (R2 (J)) = To_U32 (Shifted (J)),
               "BE Pack/Unpack mismatch (shifted) at index" & J'Image);
         end loop;
      end;
   end;
   Put_Line ("passed.");
   Put_Line ("");

   Put_Line ("Testing conversions between CONSTRAINED and UNCONSTRAINED array types: ");
   Put_Line ("Testing Simple_Array.T (constrained) <-> Simple_Array.T_Unconstrained conversions...");
   declare
      Constrained : constant Simple_Array.T := [others => 42];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Simple_Array.T_Unconstrained := Simple_Array.T_Unconstrained (Constrained);
      -- Convert back to constrained
      Const_Back : constant Simple_Array.T := Simple_Array.T (Unc_From_Const);
   begin
      Put_Line ("Original constrained T: " & Simple_Array.Representation.Image (Constrained));
      Put_Line ("Converted back to constrained T: " & Simple_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Simple_Array_Assert.Eq (Constrained, Const_Back, "Constrained/Unconstrained T roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained T conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Array.T_Le (constrained) <-> Simple_Array.T_Le_Unconstrained conversions...");
   declare
      Constrained_Le : constant Simple_Array.T_Le := [others => 55];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Simple_Array.T_Le_Unconstrained := Simple_Array.T_Le_Unconstrained (Constrained_Le);
      -- Convert back to constrained
      Const_Back : constant Simple_Array.T_Le := Simple_Array.T_Le (Unc_From_Const);
   begin
      Put_Line ("Original constrained T_Le: " & Simple_Array.Representation.Image (Constrained_Le));
      Put_Line ("Converted back to constrained T_Le: " & Simple_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Simple_Array_Le_Assert.Eq (Constrained_Le, Const_Back, "Constrained/Unconstrained T_Le roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained T_Le conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Array.U (constrained) <-> Simple_Array.Unconstrained conversions...");
   declare
      Constrained_U : constant Simple_Array.U := [others => 100];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Simple_Array.Unconstrained := Simple_Array.Unconstrained (Constrained_U);
      -- Convert back to constrained
      Const_Back : constant Simple_Array.U := Simple_Array.U (Unc_From_Const);
   begin
      Put_Line ("Original constrained U: " & Simple_Array.Representation.Image (Constrained_U));
      Put_Line ("Converted back to constrained U: " & Simple_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Simple_Array_U_Assert.Eq (Constrained_U, Const_Back, "Constrained/Unconstrained U roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained U conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Array.T (constrained) <-> Complex_Array.T_Unconstrained conversions...");
   declare
      Constrained_T : constant Complex_Array.T := [others => (One => 1, Two => 20, Three => 300)];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Complex_Array.T_Unconstrained := Complex_Array.T_Unconstrained (Constrained_T);
      -- Convert back to constrained
      Const_Back : constant Complex_Array.T := Complex_Array.T (Unc_From_Const);
   begin
      Put_Line ("Original constrained complex T: " & Complex_Array.Representation.Image (Constrained_T));
      Put_Line ("Converted back to constrained complex T: " & Complex_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Complex_Array_Assert.Eq (Constrained_T, Const_Back, "Constrained/Unconstrained complex T roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained complex T conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Array.U (constrained) <-> Complex_Array.Unconstrained conversions...");
   declare
      Constrained_U : constant Complex_Array.U := [others => (One => 5, Two => 60, Three => 700)];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Complex_Array.Unconstrained := Complex_Array.Unconstrained (Constrained_U);
      -- Convert back to constrained
      Const_Back : constant Complex_Array.U := Complex_Array.U (Unc_From_Const);
   begin
      Put_Line ("Original constrained complex U: " & Complex_Array.Representation.Image (Constrained_U));
      Put_Line ("Converted back to constrained complex U: " & Complex_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Complex_Array_U_Assert.Eq (Constrained_U, Const_Back, "Constrained/Unconstrained complex U roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained complex U conversion successful.");
      Put_Line ("passed.");
   end;
   Put_Line ("");

   Put_Line ("Testing UNCONSTRAINED array pack/unpack and swap endianness:");
   Put_Line ("Pack/unpack test for Simple_Array.Unconstrained -> T_Unconstrained: ");
   declare
      -- Create an unpacked unconstrained array (different size than constrained to test flexibility)
      Unpacked_Unc : constant Simple_Array.Unconstrained (0 .. 9) := [others => 42];
      -- Pack it to get packed unconstrained array
      Packed_Unc : constant Simple_Array.T_Unconstrained := Simple_Array.Pack (Unpacked_Unc);
      -- Unpack it back
      Unpacked_Back : constant Simple_Array.Unconstrained := Simple_Array.Unpack (Packed_Unc);
   begin
      Put_Line ("Unpacked unconstrained array size: " & Natural'Image (Unpacked_Unc'Length));
      Put_Line ("After pack/unpack roundtrip size: " & Natural'Image (Unpacked_Back'Length));
      -- Verify roundtrip preserves values
      pragma Assert (Unpacked_Back'Length = Unpacked_Unc'Length, "Pack/Unpack should preserve array length");
      pragma Assert (Unpacked_Back'First = Unpacked_Unc'First, "Pack/Unpack should preserve array first index");
      pragma Assert (Unpacked_Back'Last = Unpacked_Unc'Last, "Pack/Unpack should preserve array last index");
      for I in Unpacked_Unc'Range loop
         pragma Assert (Unpacked_Back (I) = 42, "Pack/Unpack roundtrip should preserve values at index " & Natural'Image (I));
      end loop;
      Put_Line ("passed.");
   end;
   Put_Line ("");

   Put_Line ("Pack/unpack test for Simple_Array.Unconstrained -> T_Le_Unconstrained: ");
   declare
      -- Create an unpacked unconstrained array (different size than constrained to test flexibility)
      Unpacked_Unc : constant Simple_Array.Unconstrained (0 .. 9) := [others => 99];
      -- Pack it to get little-endian packed unconstrained array
      Packed_Le_Unc : constant Simple_Array.T_Le_Unconstrained := Simple_Array.Pack (Unpacked_Unc);
      -- Unpack it back
      Unpacked_Back : constant Simple_Array.Unconstrained := Simple_Array.Unpack (Packed_Le_Unc);
   begin
      Put_Line ("Unpacked unconstrained array size: " & Natural'Image (Unpacked_Unc'Length));
      Put_Line ("After LE pack/unpack roundtrip size: " & Natural'Image (Unpacked_Back'Length));
      -- Verify roundtrip preserves values
      pragma Assert (Unpacked_Back'Length = Unpacked_Unc'Length, "LE Pack/Unpack should preserve array length");
      pragma Assert (Unpacked_Back'First = Unpacked_Unc'First, "LE Pack/Unpack should preserve array first index");
      pragma Assert (Unpacked_Back'Last = Unpacked_Unc'Last, "LE Pack/Unpack should preserve array last index");
      for I in Unpacked_Unc'Range loop
         pragma Assert (Unpacked_Back (I) = 99, "LE Pack/Unpack roundtrip should preserve values at index " & Natural'Image (I));
      end loop;
      Put_Line ("passed.");
   end;
   Put_Line ("");

   Put_Line ("Swap endianness test for unconstrained arrays (T_Unconstrained <-> T_Le_Unconstrained): ");
   declare
      -- Create a packed big-endian unconstrained array (different size than constrained)
      Packed_Be_Unc : constant Simple_Array.T_Unconstrained (0 .. 9) := [others => 77];
      -- Swap to little-endian
      Packed_Le_Unc : constant Simple_Array.T_Le_Unconstrained := Simple_Array.Swap_Endianness (Packed_Be_Unc);
      -- Swap back to big-endian
      Packed_Be_Back : constant Simple_Array.T_Unconstrained := Simple_Array.Swap_Endianness (Packed_Le_Unc);
      -- Unpack to verify values
      Unpacked_Back : constant Simple_Array.Unconstrained := Simple_Array.Unpack (Packed_Be_Back);
   begin
      Put_Line ("Packed BE unconstrained array size: " & Natural'Image (Packed_Be_Unc'Length));
      Put_Line ("After endianness swap roundtrip size: " & Natural'Image (Packed_Be_Back'Length));
      -- Verify roundtrip preserves values
      pragma Assert (Packed_Be_Back'Length = Packed_Be_Unc'Length, "Swap_Endianness should preserve array length");
      pragma Assert (Packed_Be_Back'First = Packed_Be_Unc'First, "Swap_Endianness should preserve array first index");
      pragma Assert (Packed_Be_Back'Last = Packed_Be_Unc'Last, "Swap_Endianness should preserve array last index");
      for I in Unpacked_Back'Range loop
         pragma Assert (Unpacked_Back (I) = 77, "Swap_Endianness roundtrip should preserve values at index " & Natural'Image (I));
      end loop;
      Put_Line ("passed.");
   end;
   Put_Line ("");

   Put_Line ("Pack/unpack test for Complex_Array.Unconstrained -> T_Unconstrained (nested packed type): ");
   declare
      -- Create an unpacked unconstrained array with nested packed types (different size than constrained)
      Unpacked_Unc : constant Complex_Array.Unconstrained (0 .. 4) := [others => (One => 1, Two => 25, Three => 50)];
      -- Pack it to get packed unconstrained array
      Packed_Unc : constant Complex_Array.T_Unconstrained := Complex_Array.Pack (Unpacked_Unc);
      -- Unpack it back
      Unpacked_Back : constant Complex_Array.Unconstrained := Complex_Array.Unpack (Packed_Unc);
   begin
      Put_Line ("Unpacked unconstrained array (nested) size: " & Natural'Image (Unpacked_Unc'Length));
      Put_Line ("After pack/unpack roundtrip size: " & Natural'Image (Unpacked_Back'Length));
      -- Verify roundtrip preserves values
      pragma Assert (Unpacked_Back'Length = Unpacked_Unc'Length, "Pack/Unpack should preserve array length (nested)");
      pragma Assert (Unpacked_Back'First = Unpacked_Unc'First, "Pack/Unpack should preserve array first index (nested)");
      pragma Assert (Unpacked_Back'Last = Unpacked_Unc'Last, "Pack/Unpack should preserve array last index (nested)");
      for I in Unpacked_Unc'Range loop
         pragma Assert (Unpacked_Back (I).One = 1, "Pack/Unpack should preserve One field at index " & Natural'Image (I));
         pragma Assert (Unpacked_Back (I).Two = 25, "Pack/Unpack should preserve Two field at index " & Natural'Image (I));
         pragma Assert (Unpacked_Back (I).Three = 50, "Pack/Unpack should preserve Three field at index " & Natural'Image (I));
      end loop;
      Put_Line ("passed.");
   end;
   Put_Line ("");

   Put_Line ("Pack/unpack test for Complex_Array_Le.Unconstrained -> T_Le_Unconstrained (nested packed type): ");
   declare
      -- Create an unpacked unconstrained array with nested packed types (different size than constrained)
      Unpacked_Le_Unc : constant Complex_Array_Le.Unconstrained (0 .. 4) := [others => (One => 2, Two => 30, Three => 60)];
      -- Pack it to get little-endian packed unconstrained array
      Packed_Le_Unc : constant Complex_Array_Le.T_Le_Unconstrained := Complex_Array_Le.Pack (Unpacked_Le_Unc);
      -- Unpack it back
      Unpacked_Back : constant Complex_Array_Le.Unconstrained := Complex_Array_Le.Unpack (Packed_Le_Unc);
   begin
      Put_Line ("Unpacked unconstrained array LE (nested) size: " & Natural'Image (Unpacked_Le_Unc'Length));
      Put_Line ("After LE pack/unpack roundtrip size: " & Natural'Image (Unpacked_Back'Length));
      -- Verify roundtrip preserves values
      pragma Assert (Unpacked_Back'Length = Unpacked_Le_Unc'Length, "LE Pack/Unpack should preserve array length (nested)");
      pragma Assert (Unpacked_Back'First = Unpacked_Le_Unc'First, "LE Pack/Unpack should preserve array first index (nested)");
      pragma Assert (Unpacked_Back'Last = Unpacked_Le_Unc'Last, "LE Pack/Unpack should preserve array last index (nested)");
      for I in Unpacked_Le_Unc'Range loop
         pragma Assert (Unpacked_Back (I).One = 2, "LE Pack/Unpack should preserve One field at index " & Natural'Image (I));
         pragma Assert (Unpacked_Back (I).Two = 30, "LE Pack/Unpack should preserve Two field at index " & Natural'Image (I));
         pragma Assert (Unpacked_Back (I).Three = 60, "LE Pack/Unpack should preserve Three field at index " & Natural'Image (I));
      end loop;
      Put_Line ("passed.");
   end;
   Put_Line ("");

   -- Misalignment-resilience tests for Validation autocode
   --
   -- Per Ada 2022 RM 13.3(13/3):
   --   "If an Address is specified, it is the programmer's
   --   responsibility to ensure that the address is valid and
   --   appropriate for the entity and its use; otherwise, program
   --   execution is erroneous."
   --   (http://www.ada-auth.org/standards/22rm/html/RM-13-3.html)
   --
   -- Adamant's Validation autocode keeps us out of the erroneous case
   -- for packed-array types whose elements are byte-aligned
   -- (Short_Float, Unsigned_32, etc.) by one of three strategies,
   -- depending on call site:
   --   - direct overlay when Bytes' address already satisfies
   --     T'Alignment (Valid fast path)
   --   - aligned local copy when it doesn't (Valid slow path)
   --   - per-field slice-copy in Get_Field for byte-aligned elements
   Put_Line ("Validation misalignment tests: ");
   declare
      --  Big buffer, explicitly 4-aligned. Slicing it at byte offsets
      --  0..3 gives known-aligned and known-misaligned start addresses
      --  for the embedded Serialization.Byte_Array overlays below.
      --  Sized to comfortably hold the largest array exercised here
      --  (Complex_Float_Array, ~325 bytes) plus 4 bytes of slack for
      --  the offset.
      Big : aliased Basic_Types.Byte_Array (0 .. 511) := [others => 0];
      for Big'Alignment use 4;

      -- Helper: produce a Boolean image that fits in a single token.
      function B_Img (V : Boolean) return String is (if V then "TRUE" else "FALSE");
   begin
      -- Float_Array: 12 x Short_Float, F32 -- byte-aligned 4-byte
      -- elements.
      Put ("  Float_Array @ each offset 0..3: ");
      declare
         -- All elements 1.5 (0x3FC00000 BE). Valid floats.
         F_Bytes : constant Float_Array.Serialization.Byte_Array :=
            Float_Array.Serialization.To_Byte_Array ([others => 1.5]);
      begin
         for Offset in 0 .. 3 loop
            -- Copy known bytes to the buffer at this byte offset.
            Big (Offset .. Offset + F_Bytes'Length - 1) := F_Bytes;
            declare
               -- Overlay the Serialization.Byte_Array view at the
               -- offset. Runtime address depends on Offset:
               --   Offset 0 -> 4-aligned (Big is 4-aligned)
               --   Offset 1, 2, 3 -> misaligned by that many bytes.
               Slice : Float_Array.Serialization.Byte_Array
                  with Import, Convention => Ada,
                       Address => Big (Offset)'Address;
               Ignore_Errant : Unsigned_32;
               Ok : constant Boolean := Float_Array.Validation.Valid (Slice, Ignore_Errant);
            begin
               pragma Assert (Ok, "Float_Array.Valid returned False at offset" & Offset'Image);
               -- Verify Get_Field reads the correct bytes for each
               -- field index regardless of alignment.
               for Idx in 1 .. Float_Array.Length loop
                  declare
                     Got : constant Basic_Types.Poly_Type :=
                        Float_Array.Validation.Get_Field (Slice, Unsigned_32 (Idx));
                  begin
                     -- Short_Float 1.5 widens to 0x3FC00000 in the
                     -- Poly_Type's native byte order (LE on host),
                     -- so the polytype tail bytes should be
                     -- [00 00 00 00 00 00 C0 3F].
                     pragma Assert (Got = [0, 0, 0, 0, 0, 0, 16#C0#, 16#3F#],
                                    "Float_Array.Get_Field wrong at offset" & Offset'Image
                                    & " idx" & Idx'Image);
                  end;
               end loop;
            end;
            -- Zero the buffer back out for the next offset's copy.
            Big (Offset .. Offset + F_Bytes'Length - 1) := [others => 0];
         end loop;
      end;
      Put_Line ("ok.");

      -- Simple_Array: 17 x Short_Int (range 1..999), U16 -- byte-
      -- aligned 2-byte elements. Validates that the runtime check
      -- works when T'Alignment is 4 (Short_Int's underlying type)
      -- but element stride is 2.
      Put ("  Simple_Array (valid + invalid) @ each offset 0..3: ");
      declare
         Valid_Bytes : constant Simple_Array.Serialization.Byte_Array :=
            Simple_Array.Serialization.To_Byte_Array ([others => 5]);
         -- Element 2 is 0x0001 = 1 (valid), element 3 is 0x01FF = 511
         -- (valid), element ... actually craft byte 0,1=0,1 (1,
         -- valid) and bytes 2,3=0,1 (still 1)... we want one invalid
         -- element. Field 3 byte-pattern 0xFFFF = 65535, out of
         -- range 1..999.
         Invalid_Bytes : Simple_Array.Serialization.Byte_Array :=
            Valid_Bytes;
      begin
         -- Make element index 2 (1-indexed: field 3) invalid:
         -- bytes [4..5] of the buffer hold the third element (16-bit BE).
         Invalid_Bytes (4) := 16#FF#;
         Invalid_Bytes (5) := 16#FF#;

         for Offset in 0 .. 3 loop
            -- Valid path
            Big (Offset .. Offset + Valid_Bytes'Length - 1) := Valid_Bytes;
            declare
               Slice : Simple_Array.Serialization.Byte_Array
                  with Import, Convention => Ada, Address => Big (Offset)'Address;
               Ignore_Errant : Unsigned_32;
            begin
               pragma Assert (Simple_Array.Validation.Valid (Slice, Ignore_Errant),
                              "Simple_Array.Valid (valid) at offset" & Offset'Image);
            end;
            Big (Offset .. Offset + Valid_Bytes'Length - 1) := [others => 0];

            -- Invalid path
            Big (Offset .. Offset + Invalid_Bytes'Length - 1) := Invalid_Bytes;
            declare
               Slice : Simple_Array.Serialization.Byte_Array
                  with Import, Convention => Ada, Address => Big (Offset)'Address;
               Errant : Unsigned_32 := 0;
               Ok : constant Boolean := Simple_Array.Validation.Valid (Slice, Errant);
            begin
               pragma Assert (not Ok, "Simple_Array.Valid (invalid) at offset" & Offset'Image
                              & " returned " & B_Img (Ok));
               pragma Assert (Errant = 3, "Simple_Array errant_field wrong at offset"
                              & Offset'Image & " got " & Errant'Image);
               -- Get_Field on the offending index. Var is Short_Int
               -- (Natural, 32-bit) holding 0xFFFF; on a little-endian
               -- host its memory bytes are [0xFF, 0xFF, 0x00, 0x00].
               -- Safe_Right_Copy puts Src into the last N positions of
               -- Dest, so Poly_Type[4..7] = those 4 bytes.
               pragma Assert (Simple_Array.Validation.Get_Field (Slice, 3)
                              = [0, 0, 0, 0, 16#FF#, 16#FF#, 0, 0],
                              "Simple_Array.Get_Field wrong at offset" & Offset'Image);
            end;
            Big (Offset .. Offset + Invalid_Bytes'Length - 1) := [others => 0];
         end loop;
      end;
      Put_Line ("ok.");

      -- Unaligned_Array: 8 x Short_Int (range 0..999), U10 -- sub-
      -- byte-aligned (10-bit element). The template uses the direct
      -- overlay path here (no slice-copy, no runtime check needed
      -- since GNAT bit-extracts these). This test ensures that path
      -- still produces correct results across alignment offsets.
      Put ("  Unaligned_Array @ each offset 0..3: ");
      declare
         U_Valid : constant Unaligned_Array.Serialization.Byte_Array :=
            Unaligned_Array.Serialization.To_Byte_Array ([others => 7]);
      begin
         for Offset in 0 .. 3 loop
            Big (Offset .. Offset + U_Valid'Length - 1) := U_Valid;
            declare
               Slice : Unaligned_Array.Serialization.Byte_Array
                  with Import, Convention => Ada, Address => Big (Offset)'Address;
               Ignore_Errant : Unsigned_32;
            begin
               pragma Assert (Unaligned_Array.Validation.Valid (Slice, Ignore_Errant),
                              "Unaligned_Array.Valid (valid) at offset" & Offset'Image);
            end;
            Big (Offset .. Offset + U_Valid'Length - 1) := [others => 0];
         end loop;
      end;
      Put_Line ("ok.");

      -- Complex_Float_Array: array of records-with-floats. Exercises
      -- the packed-element delegation path (R(Idx)'Address -> inner
      -- Validation.Valid) with a record element whose own validation
      -- reads multi-byte fields. This is the chain the rate_control
      -- bug originally surfaced through (record validation delegated
      -- to inner array validation on a misaligned slice).
      Put ("  Complex_Float_Array @ each offset 0..3: ");
      declare
         Cf_Bytes : constant Complex_Float_Array.Serialization.Byte_Array :=
            Complex_Float_Array.Serialization.To_Byte_Array (
               [others => (Yo => 17, F => (One => 5, Two => 21.5, Three => 50.2345))]);
      begin
         for Offset in 0 .. 3 loop
            Big (Offset .. Offset + Cf_Bytes'Length - 1) := Cf_Bytes;
            declare
               Slice : Complex_Float_Array.Serialization.Byte_Array
                  with Import, Convention => Ada, Address => Big (Offset)'Address;
               Ignore_Errant : Unsigned_32;
            begin
               pragma Assert (Complex_Float_Array.Validation.Valid (Slice, Ignore_Errant),
                              "Complex_Float_Array.Valid at offset" & Offset'Image);
            end;
            Big (Offset .. Offset + Cf_Bytes'Length - 1) := [others => 0];
         end loop;
      end;
      Put_Line ("ok.");

      -- Complex_Array_Le: little-endian record-element array.
      -- Exercises the LE validation path (Valid_Le / Get_Field_Le)
      -- with the same misaligned-slice pattern.
      Put ("  Complex_Array_Le @ each offset 0..3: ");
      declare
         Cl_Bytes : constant Complex_Array_Le.Serialization_Le.Byte_Array :=
            Complex_Array_Le.Serialization_Le.To_Byte_Array (
               [others => (One => 0, Two => 19, Three => 5)]);
      begin
         for Offset in 0 .. 3 loop
            Big (Offset .. Offset + Cl_Bytes'Length - 1) := Cl_Bytes;
            declare
               Slice : Complex_Array_Le.Serialization_Le.Byte_Array
                  with Import, Convention => Ada, Address => Big (Offset)'Address;
               Ignore_Errant : Unsigned_32;
            begin
               pragma Assert (Complex_Array_Le.Validation.Valid_Le (Slice, Ignore_Errant),
                              "Complex_Array_Le.Valid_Le at offset" & Offset'Image);
            end;
            Big (Offset .. Offset + Cl_Bytes'Length - 1) := [others => 0];
         end loop;
      end;
      Put_Line ("ok.");
   end;
   Put_Line ("passed.");
   Put_Line ("");
   --  Sentinel for the cross test runner.
   Put_Line ("=== ALL TESTS PASSED ===");
end Test;
