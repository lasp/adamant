with Basic_Types;

package Xor_8 is

   -- 8-bit XOR type. This type is just an 8-bit unsigned value which contains an 8-bit longitudinal parity
   -- byte computed over a byte array.
   subtype Xor_8_Type is Basic_Types.Byte;

   -- Routine: compute_Xor_8
   -- This function xors all the bytes in the given byte array, starting with the given seed, and returns the
   -- result. This procedure is an implementation of an 8-bit longitudinal parity calculation. Including this parity
   -- byte in the byte array to be checked will result in a result of zero when the longitudinal parity is calculated
   -- again. This method can be used to check for data corruption.
   function Compute_Xor_8 (Bytes : in Basic_Types.Byte_Array; Seed : in Xor_8_Type := 16#FF#) return Xor_8_Type;

end Xor_8;
