with Sequence_Header;
with Crc_16;
with Memory_Region;

-- Contains useful subprograms for dealing with sequences
package Sequence_Util is

   -- CRC a sequence memory region. The assumption is that this memory region holds a sequence starting at the sequence
   -- header.
   type Crc_Status is (Valid, Length_Error, Crc_Error);
   function Crc_Sequence_Memory_Region (Region : in Memory_Region.T; Seq_Header : out Sequence_Header.T; Computed_Crc : out Crc_16.Crc_16_Type) return Crc_Status;

end Sequence_Util;
