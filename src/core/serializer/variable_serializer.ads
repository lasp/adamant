-- Standard includes:
with Basic_Types;
with Serializer_Types; use Serializer_Types;

-- Generic package which provides conversions from a variable length type into byte arrays
-- using overlays. The length of the type is determined by the Serialized_Length function
-- provided at instantiation.
generic
   type T is private;
   with function Serialized_Length_T (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status;
   with function Serialized_Length_Byte_Array (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status;
package Variable_Serializer is

   -- The maximum length in bytes of the serialized type.
   Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes
   -- Byte_Array type for storing the maximum length:
   subtype Byte_Array_Index is Natural range 0 .. (Max_Serialized_Length - 1);
   subtype Byte_Array is Basic_Types.Byte_Array (Byte_Array_Index);

   -- All the subprograms below convert from a type to a byte array or a byte
   -- array to a type. They perform similarly to an unchecked conversion between the
   -- two types. In terms of performance, a copy is executed when converting one type
   -- to another. If this is not desired, the user can use an overlay directly instead
   -- in their own code. You are encouraged to use the Serialized Length function when
   -- performing the overlay in order to prevent any size mismatches. Examples of the
   -- proper overlays are shown below:
   --
   -- -- Overlay a type with a Byte_Array
   -- stat : constant Serialization_Status := Serialized_Length(src, Num_Bytes_Serialized);
   -- if stat = Success then
   --    declare
   --       subtype Sized_Byte_Array_Index is Natural range 0 .. (Num_Bytes_Serialized - 1);
   --       subtype Sized_Byte_Array is Basic_Types.Byte_Array(Sized_Byte_Array_Index);
   --       -- Perform overlay:
   --       overlay : constant Sized_Byte_Array with Import, Convention => Ada, Address => src'Address;
   --    begin
   --    end;
   -- end if;
   --
   -- -- Overlay a Byte_Array with a type:
   -- dest : constant T with Import, Convention => Ada, Address => src'Address;
   -- -- Get the serialized length of the destination:
   -- stat : constant Serialization_Status := Serialized_Length(dest, Num_Bytes_Serialized);
   --
   -- The functions below use this pattern internally, but must perform a copy of the result
   -- to the caller, thus making them inefficient for some specific operations. When in
   -- doubt, use the functions below. Overlays can be easily misused.

   -- Convert type to byte array, return number of bytes serialized:
   -- These functions may produce an error if the length of a variable field
   -- is invalid.
   function To_Byte_Array (Dest : out Basic_Types.Byte_Array; Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status;
   function To_Byte_Array (Dest : out Basic_Types.Byte_Array; Src : in T) return Serialization_Status;

   -- Convert byte array to type, return number of bytes deserialized:
   -- These functions may produce an error if the length of a variable field
   -- is invalid.
   function From_Byte_Array (Dest : out T; Src : in Basic_Types.Byte_Array; Num_Bytes_Deserialized : out Natural) return Serialization_Status;
   function From_Byte_Array (Dest : out T; Src : in Basic_Types.Byte_Array) return Serialization_Status;
end Variable_Serializer;
