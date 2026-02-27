with Basic_Types;

package Checksum_16 is

   -- 16-bit Checksum type. This type is just a 2 element 8-bit array. This
   -- is used instead of a 16-bit type to prevent endianness issues. We never
   -- want the order of these bytes to be swapped, on ANY architecture. They should
   -- be appended to a CCSDS packet in the order here, with the higher order bits in
   -- position 0 and the lower order bits in position 1. This also aids in easily
   -- appending this checksum to a byte array without conversion.
   subtype Checksum_16_Type is Basic_Types.Byte_Array (0 .. 1);

   -- Routine: compute_Checksum_16
   -- This function adds all the 16-bit words in the given byte array, starting with the given seed, and returns the
   -- result. If the number of bytes in the array is odd, then the calculation assumes a pad byte is added to the end with
   -- a value of 0x00. The addition and result is in big endian.
   function Compute_Checksum_16 (Bytes : in Basic_Types.Byte_Array; Seed : in Checksum_16_Type := [0 => 16#00#, 1 => 16#00#]) return Checksum_16_Type;

end Checksum_16;
