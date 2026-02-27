with Interfaces;
with System;

-- Basic types used by the framework
package Basic_Types is

   ----------------------------------------
   -- Byte array:
   ----------------------------------------

   -- Define a byte as an 8-bit mod type:
   subtype Byte is Interfaces.Unsigned_8
   with Object_Size => 8,
        Value_Size => 8;

   -- Define a collection of bytes indexed by a subtype of Natural.
   --
   -- We define the Byte_Array_Index type as the range of a Natural
   -- minus one value on the upper end. The reason for this is so
   -- That the 'Length attribute of a byte array will always return
   -- a value in the range of a Natural. This prevents a constraint
   -- error from being thrown when storing Length of a maximum
   -- sized byte array. This subtle issue was caught by GNAT SAS
   -- static analysis tool.
   subtype Byte_Array_Index is Natural range Natural'First .. Natural'Last - 1;

   -- Define the unconstrained byte array type.
   type Byte_Array is array (Byte_Array_Index range <>) of Byte
      with Scalar_Storage_Order => System.High_Order_First,
           Alignment => 1;

   -- "Thick pointer" access type will include an address
   -- to the bytes array in memory and an address to the bounds
   -- of the constrained byte array.
   type Byte_Array_Access is access all Byte_Array;

   -- Creating a constrained Byte array, so that the access type of
   -- this type will be a thin pointer, meaning just a raw address.
   -- The bounds of this type are the range of a Byte_Array_Index. Most
   -- likely, the bounds of the array in this type will be smaller
   -- than the range of a Byte_Array_Index. The user can gain insight into
   -- the bounds of the data stored in the array using the length
   -- field.
   subtype Constrained_Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);
   -- Thin pointer type. Bounds are set to the range of a Byte_Array_Index.
   type Unsafe_Byte_Array_Access is access all Constrained_Byte_Array;
   for Unsafe_Byte_Array_Access'Storage_Size use 0; -- Make thin.

   -- Volatile version of byte array, used for accessing hardware buffers:
   type Volatile_Byte_Array is new Byte_Array with
      Volatile => True;
   type Volatile_Byte_Array_Access is access all Volatile_Byte_Array;

   ----------------------------------------
   -- Positive integers (nonstandard size):
   ----------------------------------------

   subtype Positive_8 is Positive range Positive'First .. 255;
   subtype Positive_16 is Positive range Positive'First .. 65535;

   ----------------------------------------
   -- Positive floats:
   ----------------------------------------

   subtype Positive_Short_Float is Short_Float range Short_Float'Small .. Short_Float'Large;
   subtype Positive_Long_Float is Long_Float range Long_Float'Small .. Long_Float'Large;

   ----------------------------------------
   -- Special integers:
   ----------------------------------------

   -- A ranged integer type that is the same range as the 32-bit modular unsigned int or Interfaces.Unsigned 32.
   -- Because this is a range type and not a modular type, Ada detects overflow or underflow of the type, which
   -- is useful in some situations.
   --
   -- Note: In contrast, the Ada standard Natural uses only 31-bits of the 32-bit object size so
   -- that it can maintain its compatibility as a subtype of Integer. Basically a Natural only contains the positive
   -- portion of a two's complement 32-bit integer. The type below acts like a Natural, except uses the entire
   -- 32-bit object size and gives up the property of being the subtype of an integer.
   type Natural_32 is range 0 .. 4294967295;   -- 0 .. 2^32-1
   subtype Positive_32 is Natural_32 range 1 .. Natural_32'Last; -- 1 .. 2^32-1

   ----------------------------------------
   -- Polymorphic types:
   ----------------------------------------

   -- Define a 64 bit polymorphic type that can hold most primitive
   -- ada types. This can be useful in making things more generic.
   subtype Poly_64_Type is Byte_Array (Natural'First .. Natural'First + 8 - 1) with
      Object_Size => 64;

   -- Define a 32 bit polymorphic type that can hold many primitive
   -- ada types. This can be useful in making things more generic.
   subtype Poly_32_Type is Byte_Array (Natural'First .. Natural'First + 4 - 1) with
      Object_Size => 32;

   -- Generic poly type definition, set to 64-bit type.
   subtype Poly_Type is Poly_64_Type;

end Basic_Types;
