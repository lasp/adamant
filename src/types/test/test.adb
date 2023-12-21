pragma Profile (Ravenscar);
with Ada.Text_IO; use Ada.Text_IO;
with System;
with Interfaces; use Interfaces;

procedure Test is
   -- Float type:
   type Myfloat is digits 10;

   -- Unpacked type:
   type Unpacked is record
      Id : Natural range 0 .. 1_000;
      Value : Natural;
      Fvalue : Myfloat range 0.0 .. 1.0;
   end record;

   -- Packed type size (in bits):
   Size : constant Positive := 112;

   -- Packed type definition:
   type Packed is new Unpacked with
      Bit_Order => System.High_Order_First,
      Scalar_Storage_Order => System.High_Order_First,
      Size => Size,
      Object_Size => Size,
      Value_Size => Size,
      Alignment => 1;

      -- Packed type layout:
   for Packed use record
      Id at 0 range 0 .. 15;
      Value at 0 range 16 .. 47;
      Fvalue at 0 range 48 .. 111;
   end record;

   -- Length of packed type in bytes:
   Serialized_Length : constant Natural := Packed'Object_Size / Unsigned_8'Object_Size; -- in bytes
   -- Byte array type:
   type Byte_Array is array (Natural range 0 .. (Serialized_Length - 1)) of Unsigned_8;

   -- Function to convert packed type to byte array:
   function To_Byte_Array (Input : in Packed) return Byte_Array is
      pragma Warnings (Off, "overlay changes scalar storage order");
      Result : constant Byte_Array with Import, Convention => Ada, Address => Input'Address;
      pragma Warnings (On, "overlay changes scalar storage order");
   begin
      return Result;
   end To_Byte_Array;

   -- Function which prints the bytes in a byte array:
   procedure Print_Bytes (B_Array : Byte_Array) is
   begin
      Put ("[");
      for B of B_Array loop
         Put (Unsigned_8'Image (B) & " ");
      end loop;
      Put_Line ("]");
   end Print_Bytes;

   -- Unpacked record definition:
   My_Unpacked_Record : constant Unpacked := (Id => 7, Value => 15, Fvalue => 0.1);

   -- Packed record definitions:
   My_Converted_Record : Packed;
   My_Packed_Record : constant Packed := (Id => 7, Value => 15, Fvalue => 0.1);

   -- Byte Array definitions:
   Byte_Array_1 : Byte_Array;
   Byte_Array_2 : Byte_Array;
begin
   -- Convert an unpacked type to a packed type and then
   -- print out the bytes:
   My_Converted_Record := Packed (My_Unpacked_Record);
   Byte_Array_1 := To_Byte_Array (My_Converted_Record);
   Put ("Bytes in converted packed record: ");
   Print_Bytes (Byte_Array_1);

   -- Print out the bytes for the defined packed type:
   Put ("Bytes in defined packed record: ");
   Byte_Array_2 := To_Byte_Array (My_Packed_Record);
   Print_Bytes (Byte_Array_2);

   -- These records should be equal:
   pragma Assert (My_Packed_Record = My_Converted_Record);
   -- These byte arrays should be equal:
   pragma Assert (Byte_Array_1 = Byte_Array_2);
end Test;
