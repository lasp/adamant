-- Standard includes:
with Basic_Types;

-- Generic package which provides conversions from a given type into word arrays
-- using overlays. Supports Word_Array (native endianness), Word_Array_Le (little-endian),
-- and Word_Array_Be (big-endian) variants.
generic
   type T is private;
package Word_Serializer is

   -- Ensure that the type T has a size that is a multiple of 32 bits (word size).
   -- This prevents truncation or padding issues during serialization.
   pragma Compile_Time_Error (
      T'Object_Size mod Basic_Types.Word'Object_Size /= 0,
      "Type T must have an Object_Size that is a multiple of 32 bits (word size)"
   );

   -- Note: 'Object_Size is used here because it represents the actual
   -- size of the type when instantiated in memory, not the minimum
   -- size of the type (as the 'Size attribute specifies)
   -- See: https://gcc.gnu.org/onlinedocs/gnat_rm/Value_005fSize-and-Object_005fSize-Clauses.html

   -- The length in words of the serialized type.
   Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Word'Object_Size; -- in words

   -- The length in bytes of the serialized type (for convenience).
   Serialized_Length_In_Bytes : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes

   -- Word_Array types (native endianness):
   subtype Word_Array_Index is Natural range 0 .. (Serialized_Length - 1);
   subtype Word_Array is Basic_Types.Word_Array (Word_Array_Index);

   -- Word_Array_Le types (little-endian):
   subtype Word_Array_Le is Basic_Types.Word_Array_Le (Word_Array_Index);

   -- Word_Array_Be types (big-endian):
   subtype Word_Array_Be is Basic_Types.Word_Array_Be (Word_Array_Index);

   -- Note: All the subprograms below convert from a type to a word array or a word
   -- array to a type. They perform similarly to an unchecked conversion between the
   -- two types. In terms of performance, a copy is executed when converting one type
   -- to another. If this is not desired, the user can use an overlay directly instead
   -- in their own code. You are encouraged to use the Word_Array types, above, when
   -- performing the overlay in order to prevent any size mismatches. Examples of the
   -- proper overlays are shown below:
   --
   -- -- Overlay a type with a Word_Array
   -- T : T;
   -- Overlaid_Words : constant Word_Array with Import, Convention => Ada, Address => T'Address;
   --
   -- -- Overlay a Word_Array with a type:
   -- W : Word_Array -- make sure size is correct using Serialized_Length if necessary
   -- Overlaid_Type : constant T with Import, Convention => Ada, Address => W'Address;
   --
   -- The functions below use this pattern internally, but must perform a copy of the result
   -- to the caller, thus making them inefficient for some specific operations. When in
   -- doubt, use the functions below. Overlays can be easily misused.

   --
   -- Native endianness Word_Array conversions:
   --

   -- Convert type to word array (native endianness):
   procedure To_Word_Array (Dest : out Word_Array; Src : in T);
   function To_Word_Array (Src : in T) return Word_Array;

   -- Convert word array to type (native endianness):
   procedure From_Word_Array (Dest : out T; Src : in Word_Array);
   function From_Word_Array (Src : in Word_Array) return T;

   --
   -- Little-endian Word_Array_Le conversions:
   --

   -- Convert type to word array (little-endian):
   procedure To_Word_Array_Le (Dest : out Word_Array_Le; Src : in T);
   function To_Word_Array_Le (Src : in T) return Word_Array_Le;

   -- Convert word array to type (little-endian):
   procedure From_Word_Array_Le (Dest : out T; Src : in Word_Array_Le);
   function From_Word_Array_Le (Src : in Word_Array_Le) return T;

   --
   -- Big-endian Word_Array_Be conversions:
   --

   -- Convert type to word array (big-endian):
   procedure To_Word_Array_Be (Dest : out Word_Array_Be; Src : in T);
   function To_Word_Array_Be (Src : in T) return Word_Array_Be;

   -- Convert word array to type (big-endian):
   procedure From_Word_Array_Be (Dest : out T; Src : in Word_Array_Be);
   function From_Word_Array_Be (Src : in Word_Array_Be) return T;

end Word_Serializer;
