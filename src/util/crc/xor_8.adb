with Interfaces; use Interfaces;

package body Xor_8 is

   function Compute_Xor_8 (Bytes : in Basic_Types.Byte_Array; Seed : in Xor_8_Type := 16#FF#) return Xor_8_Type is
      To_Return : Xor_8_Type := Seed;
   begin
      for B of Bytes loop
         To_Return := @ xor B;
      end loop;
      return To_Return;
   end Compute_Xor_8;

end Xor_8;
