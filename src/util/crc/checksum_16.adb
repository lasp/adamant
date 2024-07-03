with Interfaces; use Interfaces;

package body Checksum_16 is

   -- This function computes the 16-bit checksum in terms of an unsigned 16 which is easier. It uses
   -- multiplies in order to be endian agnostic.
   function Compute_Checksum_16 (Bytes : in Basic_Types.Byte_Array; Seed : in Unsigned_16 := 16#0000#) return Unsigned_16 is
      To_Return : Unsigned_16 := Seed;
   begin
      -- Go through each byte. We need to add each set of two bytes as if it were a
      -- 16-bit integer. To do this intelligently, we know each even byte needs to be
      -- left shifted by 8-bits and each odd byte does not in the addition.
      for Idx in Bytes'Range loop
         if ((Idx - Bytes'First) mod 2) = 0 then
            To_Return := To_Return + Unsigned_16 (Bytes (Idx)) * 16#100#;
         else
            To_Return := To_Return + Unsigned_16 (Bytes (Idx));
         end if;
      end loop;

      return To_Return;
   end Compute_Checksum_16;

   function Compute_Checksum_16 (Bytes : in Basic_Types.Byte_Array; Seed : in Checksum_16_Type := [0 => 16#00#, 1 => 16#00#]) return Checksum_16_Type is
      Seed_Number : constant Unsigned_16 := Unsigned_16 (Seed (0)) * 256 + Unsigned_16 (Seed (1));
      Result : constant Unsigned_16 := Compute_Checksum_16 (Bytes, Seed_Number);
   begin
      -- Return the result as array of bytes:
      return [0 => Unsigned_8 (Result / 16#100#), 1 => Unsigned_8 (Result mod 16#100#)];
   end Compute_Checksum_16;

end Checksum_16;
