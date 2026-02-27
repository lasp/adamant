with Basic_Types;
with Byte_Array_Pointer;

package Crc_16 is

   -- 16-bit CRC type. This type is just a 2 element 8-bit array. This
   -- is used instead of a 16-bit type to prevent endianness issues. We never
   -- want the order of these bytes to be swapped, on ANY architecture. They should
   -- be appended to a CCSDS packet in the order here, with the higher order bits in
   -- position 0 and the lower order bits in position 1. This also aids in easily
   -- appending this checksum to a byte array without conversion.
   subtype Crc_16_Type is Basic_Types.Byte_Array (0 .. 1)
      with Object_Size => 2 * 8;

   -- Routine: compute_Crc_16
   --
   --    @brief This routine calculates the 16-bit CRC for a buffer, using an
   --               algorithm which gives the same results as the hardware circuit
   --               in the CCSDS telemetry CRC documentation.
   --               It uses the CCITT CRC polynomial: X^16 + X^12 + X^5 + 1.
   --               The accepted start value is 0xffff.
   --
   --    @inputs
   --               bytes -   A buffer of bytes to generate the CRC over.
   --
   --    Output: The function returns the 2-byte CRC.
   --
   --    Notes:
   --
   --       Dedicated to Gerald Grebowsky, who provided the algorithm for this
   --       function.   written by Stan Hilinski, Fall, 1995.
   --
   --       Converted to Ada and added seed capability by Kevin Dinkel, Summer, 2018.
   --
   function Compute_Crc_16 (Bytes : in Basic_Types.Byte_Array; Seed : in Crc_16_Type := [0 => 16#FF#, 1 => 16#FF#]) return Crc_16_Type;
   function Compute_Crc_16 (Byte_Ptr : in Byte_Array_Pointer.Instance; Seed : in Crc_16_Type := [0 => 16#FF#, 1 => 16#FF#]) return Crc_16_Type;

end Crc_16;
