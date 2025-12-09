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
      with Component_Size => 8,
           Scalar_Storage_Order => System.High_Order_First,
           Alignment => 1;
   pragma Warnings (Off, "pragma Pack for ""Byte_Array"" ignored");
   pragma Pack (Byte_Array);
   pragma Warnings (On, "pragma Pack for ""Byte_Array"" ignored");

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
   type Volatile_Byte_Array is new Byte_Array
      with Volatile => True;
   type Volatile_Byte_Array_Access is access all Volatile_Byte_Array;

   ------------------------------------------------------------
   -- Word and Word arrays
   ------------------------------------------------------------

   -- Define a word as a 32-bit mod type
   subtype Word is Interfaces.Unsigned_32
      with Object_Size => 32,
           Value_Size => 32;

   -- Define index type similar to byte array
   subtype Word_Array_Index is Natural range Natural'First .. Natural'Last - 1;

   -- Native endianness word array definition
   type Word_Array is array (Word_Array_Index range <>) of Word
      with Component_Size => 32;
   pragma Warnings (Off, "pragma Pack for ""Word_Array"" ignored");
   pragma Pack (Word_Array);
   pragma Warnings (On, "pragma Pack for ""Word_Array"" ignored");

   -- Little endian word array definition
   type Word_Array_Le is new Word_Array
      with Component_Size => 32,
           Scalar_Storage_Order => System.Low_Order_First;
   pragma Warnings (Off, "pragma Pack for ""Word_Array_Le"" ignored");
   pragma Pack (Word_Array_Le);
   pragma Warnings (On, "pragma Pack for ""Word_Array_Le"" ignored");

   -- Big endian word array definition
   type Word_Array_Be is new Word_Array
      with Component_Size => 32,
           Scalar_Storage_Order => System.High_Order_First;
   pragma Warnings (Off, "pragma Pack for ""Word_Array_Be"" ignored");
   pragma Pack (Word_Array_Be);
   pragma Warnings (On, "pragma Pack for ""Word_Array_Be"" ignored");

   -- "Thick pointer" access type will include an address
   -- to the bytes array in memory and an address to the bounds
   -- of the constrained byte array.
   type Word_Array_Access is access all Word_Array;
   type Word_Array_Le_Access is access all Word_Array_Le;
   type Word_Array_Be_Access is access all Word_Array_Be;

   -- Volatile version of word array, used for accessing hardware buffers:
   type Volatile_Word_Array is new Word_Array
      with Volatile => True,
           Volatile_Components => True;
   type Volatile_Word_Array_Access is access all Volatile_Word_Array;

   -- Volatile version of LE word array, used for accessing hardware buffers:
   type Volatile_Word_Array_Le is new Word_Array_Le
      with Volatile => True,
           Volatile_Components => True;
   type Volatile_Word_Array_Le_Access is access all Volatile_Word_Array_Le;

   -- Volatile version of BE word array, used for accessing hardware buffers:
   type Volatile_Word_Array_Be is new Word_Array_Be
      with Volatile => True,
           Volatile_Components => True;
   type Volatile_Word_Array_Be_Access is access all Volatile_Word_Array_Be;

   -- Atomic version of word array, used for accessing memory mapped IO or hardware registers:
   type Atomic_Word_Array is new Word_Array
      with Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;
   type Atomic_Word_Array_Access is access all Atomic_Word_Array;

   -- Atomic version of LE word array, used for accessing memory mapped IO or hardware registers:
   type Atomic_Word_Array_Le is new Word_Array_Le
      with Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;
   type Atomic_Word_Array_Le_Access is access all Atomic_Word_Array_Le;

   -- Atomic version of BE word array, used for accessing memory mapped IO or hardware registers:
   type Atomic_Word_Array_Be is new Word_Array_Be
      with Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;
   type Atomic_Word_Array_Be_Access is access all Atomic_Word_Array_Be;

   -- Register version of word array (same as Atomic for 32-bit components):
   subtype Register_Word_Array is Atomic_Word_Array;
   subtype Register_Word_Array_Access is Atomic_Word_Array_Access;

   -- Register version of LE word array (same as Atomic for 32-bit components):
   subtype Register_Word_Array_Le is Atomic_Word_Array_Le;
   subtype Register_Word_Array_Le_Access is Atomic_Word_Array_Le_Access;

   -- Register version of BE word array (same as Atomic for 32-bit components):
   subtype Register_Word_Array_Be is Atomic_Word_Array_Be;
   subtype Register_Word_Array_Be_Access is Atomic_Word_Array_Be_Access;

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
   -- portion of a two's compliment 32-bit integer. The type below acts like a Natural, except uses the entire
   -- 32-bit object size and gives up the property of being the subtype of an integer.
   type Natural_32 is range 0 .. 4294967295;   -- 0 .. 2^32-1
   subtype Positive_32 is Natural_32 range 1 .. Natural_32'Last; -- 1 .. 2^32-1

   ----------------------------------------
   -- Polymorphic types:
   ----------------------------------------

   -- Define a 64 bit polymorphic type that can hold most primitive
   -- ada types. This can be useful in making things more generic.
   subtype Poly_64_Type is Byte_Array (Natural'First .. Natural'First + 8 - 1)
      with Object_Size => 64;

   -- Define a 32 bit polymorphic type that can hold many primitive
   -- ada types. This can be useful in making things more generic.
   subtype Poly_32_Type is Byte_Array (Natural'First .. Natural'First + 4 - 1)
      with Object_Size => 32;

   -- Generic poly type definition, set to 64-bit type.
   subtype Poly_Type is Poly_64_Type;

private

   --
   -- The Byte_Array and Word_Array types above do not technically guarantee
   -- that there are "no gaps" between components of the array. However, in
   -- Ada RM Implementation Advice (13.3(71-73)) it says:
   --
   --   https://www.adaic.org/resources/add_content/standards/12rm/html/RM-13-3.html
   --
   --   An implementation should support specified Component_Sizes that are factors
   --   and multiples of the word size. For such Component_Sizes, the array should
   --   contain no gaps between components. For other Component_Sizes (if supported),
   --   the array should contain no gaps between components when Pack is also specified;
   --   the implementation should forbid this combination in cases where it cannot
   --   support a no-gaps representation.
   --
   -- The word "should" is used here, not "shall". For GNAT in the RM it is
   -- explicitly recognized that the advice is followed. Since that is the most
   -- common Ada implementation used with Adamant, we can assume that the desired
   -- packing is respected. See:
   --
   --   https://gcc.gnu.org/onlinedocs/gnat_rm/RM-13-3-71-73-Component-Size-Clauses.html
   --
   -- In order to verify this, we can use the following assertions:
   --

   subtype Byte_Array_5 is Byte_Array (0 .. 4);
   subtype Word_Array_5 is Word_Array (0 .. 4);
   subtype Word_Array_Be_5 is Word_Array_Be (0 .. 4);
   subtype Word_Array_Le_5 is Word_Array_Le (0 .. 4);
   subtype Volatile_Word_Array_5 is Volatile_Word_Array (0 .. 4);
   subtype Volatile_Word_Array_Be_5 is Volatile_Word_Array_Be (0 .. 4);
   subtype Volatile_Word_Array_Le_5 is Volatile_Word_Array_Le (0 .. 4);
   subtype Atomic_Word_Array_5 is Atomic_Word_Array (0 .. 4);
   subtype Atomic_Word_Array_Be_5 is Atomic_Word_Array_Be (0 .. 4);
   subtype Atomic_Word_Array_Le_5 is Atomic_Word_Array_Le (0 .. 4);

   pragma Compile_Time_Error (Byte_Array'Component_Size /= 8,
      "Byte_Array component size must be 8 bits");
   pragma Compile_Time_Error (Byte_Array_5'Object_Size /= 5 * 8,
      "Byte_Array_5 object size must be 40 bits (5 bytes with no gaps)");

   pragma Compile_Time_Error (Word_Array'Component_Size /= 32,
      "Word_Array component size must be 32 bits");
   pragma Compile_Time_Error (Word_Array_5'Object_Size /= 5 * 32,
      "Word_Array_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Word_Array_Be'Component_Size /= 32,
      "Word_Array_Be component size must be 32 bits");
   pragma Compile_Time_Error (Word_Array_Be_5'Object_Size /= 5 * 32,
      "Word_Array_Be_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Word_Array_Le'Component_Size /= 32,
      "Word_Array_Le component size must be 32 bits");
   pragma Compile_Time_Error (Word_Array_Le_5'Object_Size /= 5 * 32,
      "Word_Array_Le_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Volatile_Word_Array'Component_Size /= 32,
      "Volatile_Word_Array component size must be 32 bits");
   pragma Compile_Time_Error (Volatile_Word_Array_5'Object_Size /= 5 * 32,
      "Volatile_Word_Array_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Volatile_Word_Array_Be'Component_Size /= 32,
      "Volatile_Word_Array_Be component size must be 32 bits");
   pragma Compile_Time_Error (Volatile_Word_Array_Be_5'Object_Size /= 5 * 32,
      "Volatile_Word_Array_Be_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Volatile_Word_Array_Le'Component_Size /= 32,
      "Volatile_Word_Array_Le component size must be 32 bits");
   pragma Compile_Time_Error (Volatile_Word_Array_Le_5'Object_Size /= 5 * 32,
      "Volatile_Word_Array_Le_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Atomic_Word_Array'Component_Size /= 32,
      "Atomic_Word_Array component size must be 32 bits");
   pragma Compile_Time_Error (Atomic_Word_Array_5'Object_Size /= 5 * 32,
      "Atomic_Word_Array_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Atomic_Word_Array_Be'Component_Size /= 32,
      "Atomic_Word_Array_Be component size must be 32 bits");
   pragma Compile_Time_Error (Atomic_Word_Array_Be_5'Object_Size /= 5 * 32,
      "Atomic_Word_Array_Be_5 object size must be 160 bits (5 words with no gaps)");

   pragma Compile_Time_Error (Atomic_Word_Array_Le'Component_Size /= 32,
      "Atomic_Word_Array_Le component size must be 32 bits");
   pragma Compile_Time_Error (Atomic_Word_Array_Le_5'Object_Size /= 5 * 32,
      "Atomic_Word_Array_Le_5 object size must be 160 bits (5 words with no gaps)");

   -- Note: Register types are subtypes of Atomic types, so they don't need separate checks

end Basic_Types;
