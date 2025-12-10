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
with Simple_Unconstrained_Array;
with Simple_Unconstrained_Array.Representation;
with Simple_Unconstrained_Array.Assertion;
with Complex_Unconstrained_Array;
with Complex_Unconstrained_Array.Representation;
with Complex_Unconstrained_Array.Assertion;
with Atomic_Unconstrained_Array;
with Integer_32_Unconstrained_Array;

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

   -- Unconstrained arrays:
   Simple_Unc : Simple_Unconstrained_Array.T_Unconstrained (0 .. 9) := [others => 100];
   Simple_Unc_Le : Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 4) := [others => 200];
   Simple_Unc_Var : Simple_Unconstrained_Array.T_Unconstrained (1 .. 5) := [1 => 10, 2 => 20, 3 => 30, 4 => 40, 5 => 50];
   Complex_Unc : Complex_Unconstrained_Array.T_Unconstrained (0 .. 7) := [others => (One => 1, Two => 19, Three => 5)];
   Complex_Unc_Le : Complex_Unconstrained_Array.T_Le_Unconstrained (5 .. 14) := [others => (One => 10, Two => 20, Three => 30)];

   -- Unconstrained special types (Volatile, Atomic, Register):
   Volatile_Simple_Unc : Simple_Unconstrained_Array.Volatile_T_Unconstrained (0 .. 3) := [others => 111];
   Volatile_Simple_Unc_Le : Simple_Unconstrained_Array.Volatile_T_Le_Unconstrained (0 .. 3) := [others => 222];
   -- Atomic and Register arrays cannot use aggregate initialization due to Atomic_Components.
   -- They must be initialized element by element.
   Atomic_Unc : Atomic_Unconstrained_Array.Atomic_T_Unconstrained (0 .. 7);
   Atomic_Unc_Le : Atomic_Unconstrained_Array.Atomic_T_Le_Unconstrained (0 .. 7);
   Register_Unc : Atomic_Unconstrained_Array.Register_T_Unconstrained (0 .. 3);
   Register_Unc_Le : Atomic_Unconstrained_Array.Register_T_Le_Unconstrained (0 .. 3);

   -- Integer_32 unconstrained special types:
   Volatile_Int32_Unc : Integer_32_Unconstrained_Array.Volatile_T_Unconstrained (0 .. 5) := [others => -1000];
   Volatile_Int32_Unc_Le : Integer_32_Unconstrained_Array.Volatile_T_Le_Unconstrained (0 .. 5) := [others => 2000];
   Atomic_Int32_Unc : Integer_32_Unconstrained_Array.Atomic_T_Unconstrained (0 .. 6);
   Atomic_Int32_Unc_Le : Integer_32_Unconstrained_Array.Atomic_T_Le_Unconstrained (0 .. 6);
   Register_Int32_Unc : Integer_32_Unconstrained_Array.Register_T_Unconstrained (0 .. 4);
   Register_Int32_Unc_Le : Integer_32_Unconstrained_Array.Register_T_Le_Unconstrained (0 .. 4);

   -- Other local vars:
   Ignore : Unsigned_32;
   Field_Number : Unsigned_32;
begin
   -- Initialize Atomic and Register arrays element by element (aggregate initialization
   -- doesn't work with Atomic_Components):
   for I in Atomic_Unc'Range loop
      Atomic_Unc (I) := 16#DEAD_BEEF#;
   end loop;
   for I in Atomic_Unc_Le'Range loop
      Atomic_Unc_Le (I) := 16#CAFE_BABE#;
   end loop;
   for I in Register_Unc'Range loop
      Register_Unc (I) := 16#1234_5678#;
   end loop;
   for I in Register_Unc_Le'Range loop
      Register_Unc_Le (I) := 16#8765_4321#;
   end loop;

   -- Initialize Integer_32 Atomic and Register arrays:
   for I in Atomic_Int32_Unc'Range loop
      Atomic_Int32_Unc (I) := -123456;
   end loop;
   for I in Atomic_Int32_Unc_Le'Range loop
      Atomic_Int32_Unc_Le (I) := 654321;
   end loop;
   for I in Register_Int32_Unc'Range loop
      Register_Int32_Unc (I) := -999999;
   end loop;
   for I in Register_Int32_Unc_Le'Range loop
      Register_Int32_Unc_Le (I) := 777777;
   end loop;

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

   Put_Line ("Testing unconstrained arrays: ");
   Put_Line ("Testing Simple_Unc basic operations...");
   pragma Assert (Simple_Unc'Length = 10, "Simple_Unc length incorrect");
   pragma Assert (Simple_Unc'First = 0, "Simple_Unc first index incorrect");
   pragma Assert (Simple_Unc'Last = 9, "Simple_Unc last index incorrect");
   pragma Assert (Simple_Unc (0) = 100, "Simple_Unc element access failed");
   pragma Assert (Simple_Unc (9) = 100, "Simple_Unc element access failed");
   Simple_Unc (5) := 500;
   pragma Assert (Simple_Unc (5) = 500, "Simple_Unc element modification failed");
   Put_Line ("Simple_Unc(5) = " & Natural'Image (Simple_Unc (5)));
   Put_Line ("passed.");

   Put_Line ("Testing Simple_Unc_Le (little endian variant)...");
   pragma Assert (Simple_Unc_Le'Length = 5, "Simple_Unc_Le length incorrect");
   pragma Assert (Simple_Unc_Le'First = 0, "Simple_Unc_Le first index incorrect");
   pragma Assert (Simple_Unc_Le'Last = 4, "Simple_Unc_Le last index incorrect");
   Simple_Unc_Le (2) := 250;
   pragma Assert (Simple_Unc_Le (2) = 250, "Simple_Unc_Le element modification failed");
   Put_Line ("Simple_Unc_Le(2) = " & Natural'Image (Simple_Unc_Le (2)));
   Put_Line ("passed.");

   Put_Line ("Testing Simple_Unc_Var (custom index range)...");
   pragma Assert (Simple_Unc_Var'Length = 5, "Simple_Unc_Var length incorrect");
   pragma Assert (Simple_Unc_Var'First = 1, "Simple_Unc_Var first index incorrect");
   pragma Assert (Simple_Unc_Var'Last = 5, "Simple_Unc_Var last index incorrect");
   pragma Assert (Simple_Unc_Var (1) = 10, "Simple_Unc_Var element (1) incorrect");
   pragma Assert (Simple_Unc_Var (3) = 30, "Simple_Unc_Var element (3) incorrect");
   pragma Assert (Simple_Unc_Var (5) = 50, "Simple_Unc_Var element (5) incorrect");
   Simple_Unc_Var (4) := 400;
   pragma Assert (Simple_Unc_Var (4) = 400, "Simple_Unc_Var element modification failed");
   Put_Line ("Simple_Unc_Var(4) = " & Natural'Image (Simple_Unc_Var (4)));
   Put_Line ("passed.");

   Put_Line ("Testing Complex_Unc (packed record elements)...");
   pragma Assert (Complex_Unc'Length = 8, "Complex_Unc length incorrect");
   pragma Assert (Complex_Unc'First = 0, "Complex_Unc first index incorrect");
   pragma Assert (Complex_Unc'Last = 7, "Complex_Unc last index incorrect");
   pragma Assert (Complex_Unc (0).One = 1, "Complex_Unc element field One incorrect");
   pragma Assert (Complex_Unc (0).Two = 19, "Complex_Unc element field Two incorrect");
   pragma Assert (Complex_Unc (0).Three = 5, "Complex_Unc element field Three incorrect");
   Complex_Unc (3) := (One => 11, Two => 22, Three => 33);
   pragma Assert (Complex_Unc (3).One = 11, "Complex_Unc element modification failed (One)");
   pragma Assert (Complex_Unc (3).Two = 22, "Complex_Unc element modification failed (Two)");
   pragma Assert (Complex_Unc (3).Three = 33, "Complex_Unc element modification failed (Three)");
   Put_Line ("Complex_Unc(3).One = " & Natural'Image (Natural (Complex_Unc (3).One)));
   Put_Line ("Complex_Unc(3).Two = " & Natural'Image (Natural (Complex_Unc (3).Two)));
   Put_Line ("Complex_Unc(3).Three = " & Natural'Image (Natural (Complex_Unc (3).Three)));
   Put_Line ("passed.");

   Put_Line ("Testing Complex_Unc_Le (little endian, custom range)...");
   pragma Assert (Complex_Unc_Le'Length = 10, "Complex_Unc_Le length incorrect");
   pragma Assert (Complex_Unc_Le'First = 5, "Complex_Unc_Le first index incorrect");
   pragma Assert (Complex_Unc_Le'Last = 14, "Complex_Unc_Le last index incorrect");
   pragma Assert (Complex_Unc_Le (5).One = 10, "Complex_Unc_Le element field One incorrect");
   pragma Assert (Complex_Unc_Le (5).Two = 20, "Complex_Unc_Le element field Two incorrect");
   pragma Assert (Complex_Unc_Le (5).Three = 30, "Complex_Unc_Le element field Three incorrect");
   Complex_Unc_Le (10) := (One => 15, Two => 222, Three => 233);
   pragma Assert (Complex_Unc_Le (10).One = 15, "Complex_Unc_Le element modification failed (One)");
   pragma Assert (Complex_Unc_Le (10).Two = 222, "Complex_Unc_Le element modification failed (Two)");
   pragma Assert (Complex_Unc_Le (10).Three = 233, "Complex_Unc_Le element modification failed (Three)");
   Put_Line ("Complex_Unc_Le(10).One = " & Natural'Image (Natural (Complex_Unc_Le (10).One)));
   Put_Line ("Complex_Unc_Le(10).Two = " & Natural'Image (Natural (Complex_Unc_Le (10).Two)));
   Put_Line ("Complex_Unc_Le(10).Three = " & Natural'Image (Natural (Complex_Unc_Le (10).Three)));
   Put_Line ("passed.");

   Put_Line ("Testing unconstrained array slicing...");
   declare
      Slice : constant Simple_Unconstrained_Array.T_Unconstrained := Simple_Unc (0 .. 4);
   begin
      pragma Assert (Slice'Length = 5, "Slice length incorrect");
      pragma Assert (Slice (0) = 100, "Slice element incorrect");
      pragma Assert (Slice (4) = 100, "Slice element incorrect");
      Put_Line ("Slice length = " & Natural'Image (Slice'Length));
      Put_Line ("passed.");
   end;

   Put_Line ("Testing endianness conversion with unconstrained arrays...");
   declare
      Simple_Be : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 2) := [0 => 100, 1 => 200, 2 => 300];
      Simple_Le : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 2) :=
         Simple_Unconstrained_Array.T_Le_Unconstrained (Simple_Be);
   begin
      pragma Assert (Simple_Le'Length = 3, "Converted array length incorrect");
      pragma Assert (Simple_Le (0) = 100, "Endianness conversion element (0) incorrect");
      pragma Assert (Simple_Le (1) = 200, "Endianness conversion element (1) incorrect");
      pragma Assert (Simple_Le (2) = 300, "Endianness conversion element (2) incorrect");
      Put_Line ("Endianness conversion successful");
      Put_Line ("passed.");
   end;

   Put_Line ("All unconstrained array tests passed!");
   Put_Line ("");

   Put_Line ("Testing 'Size to verify unconstrained arrays are packed: ");
   Put_Line ("Testing Simple_Unc (T_Unconstrained) packing...");
   pragma Assert (Simple_Unc'Component_Size = Simple_Unconstrained_Array.Element_Size,
      "Simple_Unc Component_Size should equal Element_Size");
   pragma Assert (Simple_Unc'Size = Simple_Unc'Length * Simple_Unconstrained_Array.Element_Size,
      "Simple_Unc should be packed with no gaps: Size = " & Natural'Image (Simple_Unc'Size) &
      " but expected " & Natural'Image (Simple_Unc'Length * Simple_Unconstrained_Array.Element_Size));
   Put_Line ("Simple_Unc'Size = " & Natural'Image (Simple_Unc'Size) & " bits (length=" &
      Natural'Image (Simple_Unc'Length) & ", element_size=" & Natural'Image (Simple_Unconstrained_Array.Element_Size) & ")");
   Put_Line ("passed.");

   Put_Line ("Testing Simple_Unc_Le (T_Le_Unconstrained) packing...");
   pragma Assert (Simple_Unc_Le'Component_Size = Simple_Unconstrained_Array.Element_Size,
      "Simple_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Simple_Unc_Le'Size = Simple_Unc_Le'Length * Simple_Unconstrained_Array.Element_Size,
      "Simple_Unc_Le should be packed with no gaps");
   Put_Line ("Simple_Unc_Le'Size = " & Natural'Image (Simple_Unc_Le'Size) & " bits (length=" &
      Natural'Image (Simple_Unc_Le'Length) & ", element_size=" & Natural'Image (Simple_Unconstrained_Array.Element_Size) & ")");
   Put_Line ("passed.");

   Put_Line ("Testing Simple_Unc_Var (custom range) packing...");
   pragma Assert (Simple_Unc_Var'Component_Size = Simple_Unconstrained_Array.Element_Size,
      "Simple_Unc_Var Component_Size should equal Element_Size");
   pragma Assert (Simple_Unc_Var'Size = Simple_Unc_Var'Length * Simple_Unconstrained_Array.Element_Size,
      "Simple_Unc_Var should be packed with no gaps");
   Put_Line ("Simple_Unc_Var'Size = " & Natural'Image (Simple_Unc_Var'Size) & " bits (length=" &
      Natural'Image (Simple_Unc_Var'Length) & ", element_size=" & Natural'Image (Simple_Unconstrained_Array.Element_Size) & ")");
   Put_Line ("passed.");

   Put_Line ("Testing Complex_Unc (T_Unconstrained with packed records) packing...");
   pragma Assert (Complex_Unc'Component_Size = Complex_Unconstrained_Array.Element_Size,
      "Complex_Unc Component_Size should equal Element_Size");
   pragma Assert (Complex_Unc'Size = Complex_Unc'Length * Complex_Unconstrained_Array.Element_Size,
      "Complex_Unc should be packed with no gaps: Size = " & Natural'Image (Complex_Unc'Size) &
      " but expected " & Natural'Image (Complex_Unc'Length * Complex_Unconstrained_Array.Element_Size));
   Put_Line ("Complex_Unc'Size = " & Natural'Image (Complex_Unc'Size) & " bits (length=" &
      Natural'Image (Complex_Unc'Length) & ", element_size=" & Natural'Image (Complex_Unconstrained_Array.Element_Size) & ")");
   Put_Line ("passed.");

   Put_Line ("Testing Complex_Unc_Le (T_Le_Unconstrained with packed records) packing...");
   pragma Assert (Complex_Unc_Le'Component_Size = Complex_Unconstrained_Array.Element_Size,
      "Complex_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Complex_Unc_Le'Size = Complex_Unc_Le'Length * Complex_Unconstrained_Array.Element_Size,
      "Complex_Unc_Le should be packed with no gaps");
   Put_Line ("Complex_Unc_Le'Size = " & Natural'Image (Complex_Unc_Le'Size) & " bits (length=" &
      Natural'Image (Complex_Unc_Le'Length) & ", element_size=" & Natural'Image (Complex_Unconstrained_Array.Element_Size) & ")");
   Put_Line ("passed.");

   Put_Line ("All unconstrained array packing tests passed!");
   Put_Line ("");

   Put_Line ("Testing unconstrained special types (Volatile, Atomic, Register): ");
   Put_Line ("Testing Volatile_T_Unconstrained...");
   pragma Assert (Volatile_Simple_Unc'Length = 4, "Volatile_Simple_Unc length incorrect");
   pragma Assert (Volatile_Simple_Unc (0) = 111, "Volatile_Simple_Unc element access failed");
   Volatile_Simple_Unc (2) := 333;
   pragma Assert (Volatile_Simple_Unc (2) = 333, "Volatile_Simple_Unc element modification failed");
   Put_Line ("Volatile_Simple_Unc(2) = " & Natural'Image (Volatile_Simple_Unc (2)));
   Put_Line ("passed.");

   Put_Line ("Testing Volatile_T_Le_Unconstrained...");
   pragma Assert (Volatile_Simple_Unc_Le'Length = 4, "Volatile_Simple_Unc_Le length incorrect");
   pragma Assert (Volatile_Simple_Unc_Le (0) = 222, "Volatile_Simple_Unc_Le element access failed");
   Volatile_Simple_Unc_Le (1) := 444;
   pragma Assert (Volatile_Simple_Unc_Le (1) = 444, "Volatile_Simple_Unc_Le element modification failed");
   Put_Line ("Volatile_Simple_Unc_Le(1) = " & Natural'Image (Volatile_Simple_Unc_Le (1)));
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Unconstrained...");
   pragma Assert (Atomic_Unc'Length = 8, "Atomic_Unc length incorrect");
   pragma Assert (Atomic_Unc (0) = 16#DEAD_BEEF#, "Atomic_Unc element access failed");
   Atomic_Unc (5) := 16#AAAA_BBBB#;
   pragma Assert (Atomic_Unc (5) = 16#AAAA_BBBB#, "Atomic_Unc element modification failed");
   Put_Line ("Atomic_Unc(5) = " & Unsigned_32'Image (Atomic_Unc (5)));
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Le_Unconstrained...");
   pragma Assert (Atomic_Unc_Le'Length = 8, "Atomic_Unc_Le length incorrect");
   pragma Assert (Atomic_Unc_Le (0) = 16#CAFE_BABE#, "Atomic_Unc_Le element access failed");
   Atomic_Unc_Le (3) := 16#5555_6666#;
   pragma Assert (Atomic_Unc_Le (3) = 16#5555_6666#, "Atomic_Unc_Le element modification failed");
   Put_Line ("Atomic_Unc_Le(3) = " & Unsigned_32'Image (Atomic_Unc_Le (3)));
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Unconstrained...");
   pragma Assert (Register_Unc'Length = 4, "Register_Unc length incorrect");
   pragma Assert (Register_Unc (0) = 16#1234_5678#, "Register_Unc element access failed");
   Register_Unc (2) := 16#ABCD_EF01#;
   pragma Assert (Register_Unc (2) = 16#ABCD_EF01#, "Register_Unc element modification failed");
   Put_Line ("Register_Unc(2) = " & Unsigned_32'Image (Register_Unc (2)));
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Le_Unconstrained...");
   pragma Assert (Register_Unc_Le'Length = 4, "Register_Unc_Le length incorrect");
   pragma Assert (Register_Unc_Le (0) = 16#8765_4321#, "Register_Unc_Le element access failed");
   Register_Unc_Le (1) := 16#FEDC_BA98#;
   pragma Assert (Register_Unc_Le (1) = 16#FEDC_BA98#, "Register_Unc_Le element modification failed");
   Put_Line ("Register_Unc_Le(1) = " & Unsigned_32'Image (Register_Unc_Le (1)));
   Put_Line ("passed.");

   Put_Line ("All unconstrained special type tests passed!");
   Put_Line ("");

   Put_Line ("Testing 'Size to verify special type unconstrained arrays are packed: ");
   Put_Line ("Testing Volatile_T_Unconstrained packing...");
   pragma Assert (Volatile_Simple_Unc'Component_Size = Simple_Unconstrained_Array.Element_Size,
      "Volatile_Simple_Unc Component_Size should equal Element_Size");
   pragma Assert (Volatile_Simple_Unc'Size = Volatile_Simple_Unc'Length * Simple_Unconstrained_Array.Element_Size,
      "Volatile_Simple_Unc should be packed with no gaps");
   Put_Line ("Volatile_Simple_Unc'Size = " & Natural'Image (Volatile_Simple_Unc'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Volatile_T_Le_Unconstrained packing...");
   pragma Assert (Volatile_Simple_Unc_Le'Component_Size = Simple_Unconstrained_Array.Element_Size,
      "Volatile_Simple_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Volatile_Simple_Unc_Le'Size = Volatile_Simple_Unc_Le'Length * Simple_Unconstrained_Array.Element_Size,
      "Volatile_Simple_Unc_Le should be packed with no gaps");
   Put_Line ("Volatile_Simple_Unc_Le'Size = " & Natural'Image (Volatile_Simple_Unc_Le'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Unconstrained packing...");
   pragma Assert (Atomic_Unc'Component_Size = Atomic_Unconstrained_Array.Element_Size,
      "Atomic_Unc Component_Size should equal Element_Size");
   pragma Assert (Atomic_Unc'Size = Atomic_Unc'Length * Atomic_Unconstrained_Array.Element_Size,
      "Atomic_Unc should be packed with no gaps");
   Put_Line ("Atomic_Unc'Size = " & Natural'Image (Atomic_Unc'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Le_Unconstrained packing...");
   pragma Assert (Atomic_Unc_Le'Component_Size = Atomic_Unconstrained_Array.Element_Size,
      "Atomic_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Atomic_Unc_Le'Size = Atomic_Unc_Le'Length * Atomic_Unconstrained_Array.Element_Size,
      "Atomic_Unc_Le should be packed with no gaps");
   Put_Line ("Atomic_Unc_Le'Size = " & Natural'Image (Atomic_Unc_Le'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Unconstrained packing...");
   pragma Assert (Register_Unc'Component_Size = Atomic_Unconstrained_Array.Element_Size,
      "Register_Unc Component_Size should equal Element_Size");
   pragma Assert (Register_Unc'Size = Register_Unc'Length * Atomic_Unconstrained_Array.Element_Size,
      "Register_Unc should be packed with no gaps");
   Put_Line ("Register_Unc'Size = " & Natural'Image (Register_Unc'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Le_Unconstrained packing...");
   pragma Assert (Register_Unc_Le'Component_Size = Atomic_Unconstrained_Array.Element_Size,
      "Register_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Register_Unc_Le'Size = Register_Unc_Le'Length * Atomic_Unconstrained_Array.Element_Size,
      "Register_Unc_Le should be packed with no gaps");
   Put_Line ("Register_Unc_Le'Size = " & Natural'Image (Register_Unc_Le'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("All special type unconstrained array packing tests passed!");
   Put_Line ("");

   Put_Line ("Testing Integer_32 unconstrained special types (Volatile, Atomic, Register): ");
   Put_Line ("Testing Volatile_T_Unconstrained (Integer_32)...");
   pragma Assert (Volatile_Int32_Unc'Length = 6, "Volatile_Int32_Unc length incorrect");
   pragma Assert (Volatile_Int32_Unc (0) = -1000, "Volatile_Int32_Unc element access failed");
   Volatile_Int32_Unc (3) := -5555;
   pragma Assert (Volatile_Int32_Unc (3) = -5555, "Volatile_Int32_Unc element modification failed");
   Put_Line ("Volatile_Int32_Unc(3) = " & Integer_32'Image (Volatile_Int32_Unc (3)));
   Put_Line ("passed.");

   Put_Line ("Testing Volatile_T_Le_Unconstrained (Integer_32)...");
   pragma Assert (Volatile_Int32_Unc_Le'Length = 6, "Volatile_Int32_Unc_Le length incorrect");
   pragma Assert (Volatile_Int32_Unc_Le (0) = 2000, "Volatile_Int32_Unc_Le element access failed");
   Volatile_Int32_Unc_Le (4) := 8888;
   pragma Assert (Volatile_Int32_Unc_Le (4) = 8888, "Volatile_Int32_Unc_Le element modification failed");
   Put_Line ("Volatile_Int32_Unc_Le(4) = " & Integer_32'Image (Volatile_Int32_Unc_Le (4)));
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Unconstrained (Integer_32)...");
   pragma Assert (Atomic_Int32_Unc'Length = 7, "Atomic_Int32_Unc length incorrect");
   pragma Assert (Atomic_Int32_Unc (0) = -123456, "Atomic_Int32_Unc element access failed");
   Atomic_Int32_Unc (5) := -987654;
   pragma Assert (Atomic_Int32_Unc (5) = -987654, "Atomic_Int32_Unc element modification failed");
   Put_Line ("Atomic_Int32_Unc(5) = " & Integer_32'Image (Atomic_Int32_Unc (5)));
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Le_Unconstrained (Integer_32)...");
   pragma Assert (Atomic_Int32_Unc_Le'Length = 7, "Atomic_Int32_Unc_Le length incorrect");
   pragma Assert (Atomic_Int32_Unc_Le (0) = 654321, "Atomic_Int32_Unc_Le element access failed");
   Atomic_Int32_Unc_Le (2) := 111222;
   pragma Assert (Atomic_Int32_Unc_Le (2) = 111222, "Atomic_Int32_Unc_Le element modification failed");
   Put_Line ("Atomic_Int32_Unc_Le(2) = " & Integer_32'Image (Atomic_Int32_Unc_Le (2)));
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Unconstrained (Integer_32)...");
   pragma Assert (Register_Int32_Unc'Length = 5, "Register_Int32_Unc length incorrect");
   pragma Assert (Register_Int32_Unc (0) = -999999, "Register_Int32_Unc element access failed");
   Register_Int32_Unc (3) := -456789;
   pragma Assert (Register_Int32_Unc (3) = -456789, "Register_Int32_Unc element modification failed");
   Put_Line ("Register_Int32_Unc(3) = " & Integer_32'Image (Register_Int32_Unc (3)));
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Le_Unconstrained (Integer_32)...");
   pragma Assert (Register_Int32_Unc_Le'Length = 5, "Register_Int32_Unc_Le length incorrect");
   pragma Assert (Register_Int32_Unc_Le (0) = 777777, "Register_Int32_Unc_Le element access failed");
   Register_Int32_Unc_Le (4) := 333444;
   pragma Assert (Register_Int32_Unc_Le (4) = 333444, "Register_Int32_Unc_Le element modification failed");
   Put_Line ("Register_Int32_Unc_Le(4) = " & Integer_32'Image (Register_Int32_Unc_Le (4)));
   Put_Line ("passed.");

   Put_Line ("Testing signed value handling (negative numbers)...");
   Volatile_Int32_Unc (1) := -2147483648; -- Integer_32'First
   pragma Assert (Volatile_Int32_Unc (1) = -2147483648, "Min value test failed");
   Atomic_Int32_Unc (1) := 2147483647; -- Integer_32'Last
   pragma Assert (Atomic_Int32_Unc (1) = 2147483647, "Max value test failed");
   Put_Line ("Negative and extreme value tests passed.");

   Put_Line ("All Integer_32 unconstrained special type tests passed!");
   Put_Line ("");

   Put_Line ("Testing 'Size to verify Integer_32 unconstrained arrays are packed: ");
   Put_Line ("Testing Volatile_T_Unconstrained (Integer_32) packing...");
   pragma Assert (Volatile_Int32_Unc'Component_Size = Integer_32_Unconstrained_Array.Element_Size,
      "Volatile_Int32_Unc Component_Size should equal Element_Size");
   pragma Assert (Volatile_Int32_Unc'Size = Volatile_Int32_Unc'Length * Integer_32_Unconstrained_Array.Element_Size,
      "Volatile_Int32_Unc should be packed with no gaps");
   Put_Line ("Volatile_Int32_Unc'Size = " & Natural'Image (Volatile_Int32_Unc'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Volatile_T_Le_Unconstrained (Integer_32) packing...");
   pragma Assert (Volatile_Int32_Unc_Le'Component_Size = Integer_32_Unconstrained_Array.Element_Size,
      "Volatile_Int32_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Volatile_Int32_Unc_Le'Size = Volatile_Int32_Unc_Le'Length * Integer_32_Unconstrained_Array.Element_Size,
      "Volatile_Int32_Unc_Le should be packed with no gaps");
   Put_Line ("Volatile_Int32_Unc_Le'Size = " & Natural'Image (Volatile_Int32_Unc_Le'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Unconstrained (Integer_32) packing...");
   pragma Assert (Atomic_Int32_Unc'Component_Size = Integer_32_Unconstrained_Array.Element_Size,
      "Atomic_Int32_Unc Component_Size should equal Element_Size");
   pragma Assert (Atomic_Int32_Unc'Size = Atomic_Int32_Unc'Length * Integer_32_Unconstrained_Array.Element_Size,
      "Atomic_Int32_Unc should be packed with no gaps");
   Put_Line ("Atomic_Int32_Unc'Size = " & Natural'Image (Atomic_Int32_Unc'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Atomic_T_Le_Unconstrained (Integer_32) packing...");
   pragma Assert (Atomic_Int32_Unc_Le'Component_Size = Integer_32_Unconstrained_Array.Element_Size,
      "Atomic_Int32_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Atomic_Int32_Unc_Le'Size = Atomic_Int32_Unc_Le'Length * Integer_32_Unconstrained_Array.Element_Size,
      "Atomic_Int32_Unc_Le should be packed with no gaps");
   Put_Line ("Atomic_Int32_Unc_Le'Size = " & Natural'Image (Atomic_Int32_Unc_Le'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Unconstrained (Integer_32) packing...");
   pragma Assert (Register_Int32_Unc'Component_Size = Integer_32_Unconstrained_Array.Element_Size,
      "Register_Int32_Unc Component_Size should equal Element_Size");
   pragma Assert (Register_Int32_Unc'Size = Register_Int32_Unc'Length * Integer_32_Unconstrained_Array.Element_Size,
      "Register_Int32_Unc should be packed with no gaps");
   Put_Line ("Register_Int32_Unc'Size = " & Natural'Image (Register_Int32_Unc'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("Testing Register_T_Le_Unconstrained (Integer_32) packing...");
   pragma Assert (Register_Int32_Unc_Le'Component_Size = Integer_32_Unconstrained_Array.Element_Size,
      "Register_Int32_Unc_Le Component_Size should equal Element_Size");
   pragma Assert (Register_Int32_Unc_Le'Size = Register_Int32_Unc_Le'Length * Integer_32_Unconstrained_Array.Element_Size,
      "Register_Int32_Unc_Le should be packed with no gaps");
   Put_Line ("Register_Int32_Unc_Le'Size = " & Natural'Image (Register_Int32_Unc_Le'Size) & " bits");
   Put_Line ("passed.");

   Put_Line ("All Integer_32 unconstrained array packing tests passed!");
   Put_Line ("");

   Put_Line ("Testing Representation packages for unconstrained arrays: ");
   Put_Line ("Testing Simple_Unconstrained_Array.Representation.Image for Unconstrained...");
   declare
      Img : constant String := Simple_Unconstrained_Array.Representation.Image (Simple_Unc);
   begin
      pragma Assert (Img'Length > 0, "Image should return non-empty string");
      Put_Line (Img);
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Unconstrained_Array.Representation.To_Tuple_String for Unconstrained...");
   declare
      Tuple_Str : constant String := Simple_Unconstrained_Array.Representation.To_Tuple_String (Simple_Unc);
   begin
      pragma Assert (Tuple_Str'Length > 0, "To_Tuple_String should return non-empty string");
      pragma Assert (Tuple_Str (Tuple_Str'First) = '[', "To_Tuple_String should start with '['");
      pragma Assert (Tuple_Str (Tuple_Str'Last) = ']', "To_Tuple_String should end with ']'");
      Put_Line (Tuple_Str);
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Unconstrained_Array.Representation for T_Unconstrained...");
   declare
      Simple_T_Unc : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 4) := [others => 150];
      Img : constant String := Simple_Unconstrained_Array.Representation.Image (Simple_T_Unc);
      Tuple_Str : constant String := Simple_Unconstrained_Array.Representation.To_Tuple_String (Simple_T_Unc);
   begin
      pragma Assert (Img'Length > 0, "Image should return non-empty string");
      pragma Assert (Tuple_Str'Length > 0, "To_Tuple_String should return non-empty string");
      Put_Line (Img);
      Put_Line (Tuple_Str);
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Unconstrained_Array.Representation for T_Le_Unconstrained...");
   declare
      Simple_T_Le_Unc : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 3) := [others => 175];
      Img : constant String := Simple_Unconstrained_Array.Representation.Image (Simple_T_Le_Unc);
      Tuple_Str : constant String := Simple_Unconstrained_Array.Representation.To_Tuple_String (Simple_T_Le_Unc);
   begin
      pragma Assert (Img'Length > 0, "Image should return non-empty string");
      pragma Assert (Tuple_Str'Length > 0, "To_Tuple_String should return non-empty string");
      Put_Line (Img);
      Put_Line (Tuple_Str);
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Unconstrained_Array.Representation for Unconstrained...");
   declare
      Img : constant String := Complex_Unconstrained_Array.Representation.Image (Complex_Unc);
      Tuple_Str : constant String := Complex_Unconstrained_Array.Representation.To_Tuple_String (Complex_Unc);
   begin
      pragma Assert (Img'Length > 0, "Image should return non-empty string");
      pragma Assert (Tuple_Str'Length > 0, "To_Tuple_String should return non-empty string");
      Put_Line (Img);
      Put_Line (Tuple_Str);
      Put_Line ("passed.");
   end;

   Put_Line ("All Representation package tests passed!");
   Put_Line ("");

   Put_Line ("Testing Assertion packages for unconstrained arrays: ");
   Put_Line ("Testing Simple_Unconstrained_Array.Assertion for Unconstrained...");
   declare
      Unc1 : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
      Unc2 : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
      Unc3 : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 99];
   begin
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_Unconstrained_Assert.Eq (Unc1, Unc2, "Arrays should be equal");
      Put_Line ("Equal arrays passed assertion.");

      -- Test inequality
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_Unconstrained_Assert.Neq (Unc1, Unc3, "Arrays should not be equal");
      Put_Line ("Unequal arrays passed neq assertion.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Unconstrained_Array.Assertion for T_Unconstrained...");
   declare
      T_Unc1 : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
      T_Unc2 : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
   begin
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_T_Unconstrained_Assert.Eq (T_Unc1, T_Unc2, "T_Unconstrained arrays should be equal");
      Put_Line ("Equal T_Unconstrained arrays passed assertion.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Unconstrained_Array.Assertion for T_Le_Unconstrained...");
   declare
      T_Le_Unc1 : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
      T_Le_Unc2 : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
   begin
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_T_Le_Unconstrained_Assert.Eq (T_Le_Unc1, T_Le_Unc2, "T_Le_Unconstrained arrays should be equal");
      Put_Line ("Equal T_Le_Unconstrained arrays passed assertion.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Unconstrained_Array.Assertion for Unconstrained...");
   declare
      Complex_Unc1 : constant Complex_Unconstrained_Array.Unconstrained (0 .. 2) := [others => (One => 5, Two => 20, Three => 25)];
      Complex_Unc2 : constant Complex_Unconstrained_Array.Unconstrained (0 .. 2) := [others => (One => 5, Two => 20, Three => 25)];
   begin
      Complex_Unconstrained_Array.Assertion.Complex_Unconstrained_Array_Unconstrained_Assert.Eq (Complex_Unc1, Complex_Unc2, "Complex arrays should be equal");
      Put_Line ("Equal complex arrays passed assertion.");
      Put_Line ("passed.");
   end;

   Put_Line ("All Assertion package tests passed!");
   Put_Line ("");

   Put_Line ("Testing Pack/Unpack functions for unconstrained arrays: ");
   Put_Line ("Testing Simple_Unconstrained_Array Pack/Unpack (Big Endian)...");
   declare
      Unc : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
      Packed : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Pack (Unc);
      Unpacked : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Unpack (Packed);
   begin
      Put_Line ("Original Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc));
      Put_Line ("Packed T_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Packed));
      Put_Line ("Unpacked again: " & Simple_Unconstrained_Array.Representation.Image (Unpacked));

      -- Verify pack/unpack roundtrip
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_Unconstrained_Assert.Eq (Unc, Unpacked, "Pack/Unpack roundtrip should preserve values");
      Put_Line ("Pack/Unpack roundtrip successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Unconstrained_Array Pack/Unpack (Little Endian)...");
   declare
      Unc : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := [0 => 50, 1 => 60, 2 => 70, 3 => 80];
      Packed_Le : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Pack (Unc);
      Unpacked : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Unpack (Packed_Le);
   begin
      Put_Line ("Original Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc));
      Put_Line ("Packed T_Le_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Packed_Le));
      Put_Line ("Unpacked again: " & Simple_Unconstrained_Array.Representation.Image (Unpacked));

      -- Verify pack/unpack roundtrip
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_Unconstrained_Assert.Eq (Unc, Unpacked, "Pack/Unpack LE roundtrip should preserve values");
      Put_Line ("Pack/Unpack LE roundtrip successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Swap_Endianness for unconstrained arrays...");
   declare
      T_Unc : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) := [0 => 100, 1 => 200, 2 => 300, 3 => 400];
      T_Le_Swapped : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Swap_Endianness (T_Unc);
      T_Unc_Back : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Swap_Endianness (T_Le_Swapped);
   begin
      Put_Line ("Original T_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Unc));
      Put_Line ("Swapped to T_Le_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Le_Swapped));
      Put_Line ("Swapped back to T_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Unc_Back));

      -- Verify swap roundtrip
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_T_Unconstrained_Assert.Eq (T_Unc, T_Unc_Back, "Swap_Endianness roundtrip should preserve values");
      Put_Line ("Swap_Endianness roundtrip successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Unconstrained_Array Pack/Unpack...");
   declare
      Unc : constant Complex_Unconstrained_Array.Unconstrained (0 .. 2) := [others => (One => 10, Two => 50, Three => 100)];
      Packed : constant Complex_Unconstrained_Array.T_Unconstrained (0 .. 2) := Complex_Unconstrained_Array.Pack (Unc);
      Unpacked : constant Complex_Unconstrained_Array.Unconstrained (0 .. 2) := Complex_Unconstrained_Array.Unpack (Packed);
   begin
      Put_Line ("Original Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (Unc));
      Put_Line ("Packed T_Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (Packed));
      Put_Line ("Unpacked again: " & Complex_Unconstrained_Array.Representation.Image (Unpacked));

      -- Verify pack/unpack roundtrip
      Complex_Unconstrained_Array.Assertion.Complex_Unconstrained_Array_Unconstrained_Assert.Eq (Unc, Unpacked, "Complex Pack/Unpack roundtrip should preserve values");
      Put_Line ("Complex Pack/Unpack roundtrip successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("All Pack/Unpack tests passed!");
   Put_Line ("");

   Put_Line ("Testing type conversions between constrained and unconstrained types: ");
   Put_Line ("Testing Unconstrained <-> T_Unconstrained conversions...");
   declare
      Unc : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := [0 => 10, 1 => 20, 2 => 30, 3 => 40];
      T_Unc : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) := Simple_Unconstrained_Array.T_Unconstrained (Unc);
      Unc_Back : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Unconstrained (T_Unc);
   begin
      Put_Line ("Original Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc));
      Put_Line ("Converted to T_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Unc));
      Put_Line ("Converted back to Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc_Back));

      -- Verify type conversion roundtrip
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_Unconstrained_Assert.Eq (Unc, Unc_Back, "Type conversion roundtrip should preserve values");
      Put_Line ("Type conversion roundtrip successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Unconstrained <-> T_Le_Unconstrained conversions...");
   declare
      Unc : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := [0 => 50, 1 => 60, 2 => 70, 3 => 80];
      T_Le_Unc : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 3) := Simple_Unconstrained_Array.T_Le_Unconstrained (Unc);
      Unc_Back : constant Simple_Unconstrained_Array.Unconstrained (0 .. 3) := Simple_Unconstrained_Array.Unconstrained (T_Le_Unc);
   begin
      Put_Line ("Original Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc));
      Put_Line ("Converted to T_Le_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Le_Unc));
      Put_Line ("Converted back to Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc_Back));

      -- Verify type conversion roundtrip
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_Unconstrained_Assert.Eq (Unc, Unc_Back, "Type conversion LE roundtrip should preserve values");
      Put_Line ("Type conversion LE roundtrip successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing T_Unconstrained <-> T_Le_Unconstrained conversions...");
   declare
      T_Unc : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) := [0 => 100, 1 => 200, 2 => 300, 3 => 400];
      -- Convert through Unconstrained as intermediate
      T_Le_Unc : constant Simple_Unconstrained_Array.T_Le_Unconstrained (0 .. 3) :=
         Simple_Unconstrained_Array.T_Le_Unconstrained (Simple_Unconstrained_Array.Unconstrained (T_Unc));
      T_Unc_Back : constant Simple_Unconstrained_Array.T_Unconstrained (0 .. 3) :=
         Simple_Unconstrained_Array.T_Unconstrained (Simple_Unconstrained_Array.Unconstrained (T_Le_Unc));
   begin
      Put_Line ("Original T_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Unc));
      Put_Line ("Converted to T_Le_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Le_Unc));
      Put_Line ("Converted back to T_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (T_Unc_Back));

      -- Verify type conversion roundtrip
      Simple_Unconstrained_Array.Assertion.Simple_Unconstrained_Array_T_Unconstrained_Assert.Eq (T_Unc, T_Unc_Back, "BE/LE type conversion roundtrip should preserve values");
      Put_Line ("BE/LE type conversion roundtrip successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Unconstrained_Array type conversions...");
   Put_Line ("Note: Complex arrays with packed elements require Pack/Unpack functions, not direct type conversions.");
   declare
      Unc : constant Complex_Unconstrained_Array.Unconstrained (0 .. 2) := [others => (One => 10, Two => 50, Three => 100)];
      -- For packed record elements, use Pack/Unpack instead of type conversion
      T_Unc : constant Complex_Unconstrained_Array.T_Unconstrained (0 .. 2) := Complex_Unconstrained_Array.Pack (Unc);
      Unc_Back : constant Complex_Unconstrained_Array.Unconstrained (0 .. 2) := Complex_Unconstrained_Array.Unpack (T_Unc);
   begin
      Put_Line ("Original Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (Unc));
      Put_Line ("Packed to T_Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (T_Unc));
      Put_Line ("Unpacked back to Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (Unc_Back));

      -- Verify Pack/Unpack roundtrip for complex arrays
      Complex_Unconstrained_Array.Assertion.Complex_Unconstrained_Array_Unconstrained_Assert.Eq (Unc, Unc_Back, "Complex Pack/Unpack roundtrip should preserve values");
      Put_Line ("Complex array conversion via Pack/Unpack successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("All type conversion tests passed!");
   Put_Line ("");

   Put_Line ("Testing conversions between CONSTRAINED and UNCONSTRAINED array types: ");
   Put_Line ("Testing Simple_Array.T (constrained) <-> Simple_Unconstrained_Array.T_Unconstrained conversions...");
   declare
      Constrained : constant Simple_Array.T := [others => 42];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Simple_Unconstrained_Array.T_Unconstrained := Simple_Unconstrained_Array.T_Unconstrained (Constrained);
      -- Convert back to constrained
      Const_Back : constant Simple_Array.T := Simple_Array.T (Unc_From_Const);
   begin
      Put_Line ("Original constrained T: " & Simple_Array.Representation.Image (Constrained));
      Put_Line ("Converted to unconstrained T_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc_From_Const));
      Put_Line ("Converted back to constrained T: " & Simple_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Simple_Array_Assert.Eq (Constrained, Const_Back, "Constrained/Unconstrained T roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained T conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Array.T_Le (constrained) <-> Simple_Unconstrained_Array.T_Le_Unconstrained conversions...");
   declare
      Constrained_Le : constant Simple_Array.T_Le := [others => 55];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Simple_Unconstrained_Array.T_Le_Unconstrained := Simple_Unconstrained_Array.T_Le_Unconstrained (Constrained_Le);
      -- Convert back to constrained
      Const_Back : constant Simple_Array.T_Le := Simple_Array.T_Le (Unc_From_Const);
   begin
      Put_Line ("Original constrained T_Le: " & Simple_Array.Representation.Image (Constrained_Le));
      Put_Line ("Converted to unconstrained T_Le_Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc_From_Const));
      Put_Line ("Converted back to constrained T_Le: " & Simple_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Simple_Array_Le_Assert.Eq (Constrained_Le, Const_Back, "Constrained/Unconstrained T_Le roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained T_Le conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Simple_Array.U (constrained) <-> Simple_Unconstrained_Array.Unconstrained conversions...");
   declare
      Constrained_U : constant Simple_Array.U := [others => 100];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Simple_Unconstrained_Array.Unconstrained := Simple_Unconstrained_Array.Unconstrained (Constrained_U);
      -- Convert back to constrained
      Const_Back : constant Simple_Array.U := Simple_Array.U (Unc_From_Const);
   begin
      Put_Line ("Original constrained U: " & Simple_Array.Representation.Image (Constrained_U));
      Put_Line ("Converted to unconstrained Unconstrained: " & Simple_Unconstrained_Array.Representation.Image (Unc_From_Const));
      Put_Line ("Converted back to constrained U: " & Simple_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Simple_Array_U_Assert.Eq (Constrained_U, Const_Back, "Constrained/Unconstrained U roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained U conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Array.T (constrained) <-> Complex_Unconstrained_Array.T_Unconstrained conversions...");
   declare
      Constrained_T : constant Complex_Array.T := [others => (One => 1, Two => 20, Three => 300)];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Complex_Unconstrained_Array.T_Unconstrained := Complex_Unconstrained_Array.T_Unconstrained (Constrained_T);
      -- Convert back to constrained
      Const_Back : constant Complex_Array.T := Complex_Array.T (Unc_From_Const);
   begin
      Put_Line ("Original constrained complex T: " & Complex_Array.Representation.Image (Constrained_T));
      Put_Line ("Converted to unconstrained complex T_Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (Unc_From_Const));
      Put_Line ("Converted back to constrained complex T: " & Complex_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Complex_Array_Assert.Eq (Constrained_T, Const_Back, "Constrained/Unconstrained complex T roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained complex T conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Array_Le.T_Le (constrained) <-> Complex_Unconstrained_Array.T_Le_Unconstrained conversions...");
   declare
      Constrained_T_Le : constant Complex_Array_Le.T_Le := [others => (One => 3, Two => 40, Three => 500)];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Complex_Unconstrained_Array.T_Le_Unconstrained := Complex_Unconstrained_Array.T_Le_Unconstrained (Constrained_T_Le);
      -- Convert back to constrained
      Const_Back : constant Complex_Array_Le.T_Le := Complex_Array_Le.T_Le (Unc_From_Const);
   begin
      Put_Line ("Original constrained complex T_Le: " & Complex_Array_Le.Representation.Image (Constrained_T_Le));
      Put_Line ("Converted to unconstrained complex T_Le_Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (Unc_From_Const));
      Put_Line ("Converted back to constrained complex T_Le: " & Complex_Array_Le.Representation.Image (Const_Back));

      -- Verify roundtrip
      Complex_Array_Le_Le_Assert.Eq (Constrained_T_Le, Const_Back, "Constrained/Unconstrained complex T_Le roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained complex T_Le conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("Testing Complex_Array.U (constrained) <-> Complex_Unconstrained_Array.Unconstrained conversions...");
   declare
      Constrained_U : constant Complex_Array.U := [others => (One => 5, Two => 60, Three => 700)];
      -- Convert constrained to unconstrained
      Unc_From_Const : constant Complex_Unconstrained_Array.Unconstrained := Complex_Unconstrained_Array.Unconstrained (Constrained_U);
      -- Convert back to constrained
      Const_Back : constant Complex_Array.U := Complex_Array.U (Unc_From_Const);
   begin
      Put_Line ("Original constrained complex U: " & Complex_Array.Representation.Image (Constrained_U));
      Put_Line ("Converted to unconstrained complex Unconstrained: " & Complex_Unconstrained_Array.Representation.Image (Unc_From_Const));
      Put_Line ("Converted back to constrained complex U: " & Complex_Array.Representation.Image (Const_Back));

      -- Verify roundtrip
      Complex_Array_U_Assert.Eq (Constrained_U, Const_Back, "Constrained/Unconstrained complex U roundtrip should preserve values");
      Put_Line ("Constrained <-> Unconstrained complex U conversion successful.");
      Put_Line ("passed.");
   end;

   Put_Line ("All constrained <-> unconstrained array type conversion tests passed!");
   Put_Line ("");
end Test;
