with Ada.Text_IO; use Ada.Text_IO;
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
   pragma Assert (Simple_Array.Validation.Valid (Simple_Mut, Ignore), "Simple is not valid, but should be.");
   pragma Assert (Complex_Array.Validation.Valid (Complex_Mut, Ignore), "Complex is not valid, but should be.");
   pragma Assert (Eight_Bit_Type_Array.Validation.Valid (Eight_Mut, Ignore), "Eight is not valid, but should be.");
   pragma Assert (Unaligned_Array.Validation.Valid (Unaligned_Mut, Ignore), "Unaligned is not valid, but should be.");
   pragma Assert (Enum_Array.Validation.Valid (Enum_Mut, Ignore), "Enum is not valid, but should be.");
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
   pragma Assert (not Simple_Array.Validation.Valid (Simple_Mut, Field_Number), "Simple is valid, but should not be.");
   pragma Assert (Field_Number = 3, "Simple field_Number is wrong.");
   Put_Line (Poly2bytestring (Simple_Array.Validation.Get_Field (Simple_Mut, Field_Number)));
   pragma Assert (Simple_Array.Validation.Get_Field (Simple_Mut, Field_Number) = [0, 0, 0, 0, 0, 255, 0, 0], "Simple's polytype field is wrong."); -- represented by 32-bit number in little endian
   pragma Assert (not Complex_Array.Validation.Valid (Complex_Mut, Field_Number), "Complex is valid, but should not be.");
   pragma Assert (Field_Number = 5, "Complex field_Number is wrong.");
   Put_Line (Poly2bytestring (Complex_Array.Validation.Get_Field (Complex_Mut, Field_Number)));
   pragma Assert (Complex_Array.Validation.Get_Field (Complex_Mut, Field_Number) = [0, 0, 0, 0, 9, 0, 0, 0], "Complex's polytype field is wrong."); -- represented by 32-bit number in little endian
   pragma Assert (Complex_Array.Validation.Valid (Complex_Mut, Field_Number, Complex_Mut'First, Complex_Mut'First), "Complex is invalid, but should not be.");
   pragma Assert (not Eight_Bit_Type_Array.Validation.Valid (Eight_Mut, Field_Number), "Eight is valid, but should not be.");
   pragma Assert (Field_Number = 2, "Eight field_Number is wrong.");
   Put_Line (Poly2bytestring (Eight_Bit_Type_Array.Validation.Get_Field (Eight_Mut, Field_Number)));
   pragma Assert (Eight_Bit_Type_Array.Validation.Get_Field (Eight_Mut, Field_Number) = [1, 1, 1, 00, 1, 1, 1, 1], "Eight's polytype field is wrong."); -- represented by 32-bit number in little endian
   pragma Assert (not Unaligned_Array.Validation.Valid (Unaligned_Mut, Field_Number), "Unaligned is valid, but should not be.");
   pragma Assert (Field_Number = 1, "Unaligned field_Number is wrong. ");
   Put_Line (Poly2bytestring (Unaligned_Array.Validation.Get_Field (Unaligned_Mut, Field_Number)));
   pragma Assert (Unaligned_Array.Validation.Get_Field (Unaligned_Mut, Field_Number) = [0, 0, 0, 00, 255, 03, 0, 0], "Unaligned's polytype field is wrong."); -- represented by 32-bit number in little endian
   pragma Assert (not Enum_Array.Validation.Valid (Enum_Mut, Field_Number), "Enum is valid, but should not be.");
   pragma Assert (Field_Number = 4, "Enum field_Number is wrong. " & Natural'Image (Natural (Field_Number)));
   Put_Line (Poly2bytestring (Enum_Array.Validation.Get_Field (Enum_Mut, Field_Number)));
   pragma Assert (Enum_Array.Validation.Get_Field (Enum_Mut, Field_Number) = [0, 0, 0, 00, 0, 0, 0, 04], "Enum's polytype field is wrong."); -- represented by 32-bit number in little endian
   pragma Assert (Enum_Array.Validation.Valid (Enum_Mut, Field_Number, Enum_Mut'First, Enum_Mut'First + 1), "Enum is invalid, but should not be 1.");
   pragma Assert (Enum_Array.Validation.Valid (Enum_Mut, Field_Number, Enum_Mut'First + 1, Enum_Mut'First + 2), "Enum is invalid, but should not be 2.");
   pragma Assert (Enum_Array.Validation.Valid (Enum_Mut, Field_Number, Enum_Mut'First, Enum_Mut'Last - 3), "Enum is invalid, but should not be 3.");
   pragma Assert (Enum_Array.Validation.Valid (Enum_Mut, Field_Number, Enum_Mut'First, Enum_Mut'Last - 2), "Enum is invalid, but should not be 4.");
   pragma Assert (not Enum_Array.Validation.Valid (Enum_Mut, Field_Number, Enum_Mut'First, Enum_Mut'Last), "Enum is valid, but should not be 5.");
   pragma Assert (not Enum_Array.Validation.Valid (Enum_Mut, Field_Number, Enum_Mut'First, Enum_Mut'Last), "Enum is valid, but should not be 6.");
   pragma Assert (not Enum_Array.Validation.Valid (Enum_Mut, Field_Number, Enum_Mut'Last - 2, Enum_Mut'Last), "Enum is valid, but should not be 7.");
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
end Test;
