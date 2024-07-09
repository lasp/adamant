with Basic_Types;

package body Sequence_Util is

   function Crc_Sequence_Memory_Region (Region : in Memory_Region.T; Seq_Header : out Sequence_Header.T; Computed_Crc : out Crc_16.Crc_16_Type) return Crc_Status is
   begin
      -- Initialize CRC output params:
      Seq_Header := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 0);
      Computed_Crc := [0, 0];

      -- Check the length and make sure it is large enough to hold a sequence header:
      if Region.Length < Sequence_Header.Size_In_Bytes then
         return Length_Error;
      else
         -- OK we know that the memory region is large enough to hold a minimum sized sequence.
         declare
            -- Overlay a header over the beginning of the memory region.
            Header : Sequence_Header.T with Import, Convention => Ada, Address => Region.Address;
            Sequence_Size : constant Natural := Header.Length;
         begin
            -- Set the header out parameter:
            Seq_Header := Header;

            -- First let's check the length of the sequence and make sure the memory region is at least as large.
            -- The length field in the header should also be at least as big as the size of the header since its
            -- included in the length.
            if Sequence_Size > Region.Length or else
                Sequence_Size < Sequence_Header.Size_In_Bytes
            then
               return Length_Error;
            else
               -- OK the length of the memory region is large enough to hold this sequence, so now we can
               -- safely compute a CRC over it.
               declare
                  use Basic_Types;
                  -- Overlay the sequence with a byte array to assist in CRCing it.
                  subtype Safe_Byte_Array is Basic_Types.Byte_Array (0 .. Sequence_Size - 1);
                  Sequence_Bytes : Safe_Byte_Array with Import, Convention => Ada, Address => Region.Address;
                  -- Compute CRC over entire sequence excluding the first 2 bytes which are the
                  -- CRC included in the header.
                  Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Sequence_Bytes (Sequence_Bytes'First + Crc_16.Crc_16_Type'Length .. Sequence_Bytes'Last));
               begin
                  -- Set the computed CRC out parameter:
                  Computed_Crc := Crc;

                  -- Compare the CRCs:
                  if Crc = Header.Crc then
                     return Valid;
                  else
                     return Crc_Error;
                  end if;
               end;
            end if;
         end;
      end if;
   end Crc_Sequence_Memory_Region;

end Sequence_Util;
