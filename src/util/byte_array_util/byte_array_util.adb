with Interfaces; use Interfaces;

package body Byte_Array_Util is

   function Safe_Right_Copy (Dest : in out Byte_Array; Src : in Byte_Array) return Natural is
   begin
      -- If the length of both arrays is positive we can do a copy. Otherwise
      -- there is no need to continue.
      if Is_Not_Empty (Src) and then Is_Not_Empty (Dest) then
         declare
            Num_Bytes_To_Copy : constant Natural :=
               Natural'Min (Src'Last - Src'First, Dest'Last - Dest'First) + 1;
         begin
            -- Help static analysis understand that the number of bytes to copy
            -- must be less than or equal to both the destination and src array
            -- lengths at this point. We know that both dest and src cannot be
            -- a null array.
            pragma Assert (Num_Bytes_To_Copy <= Signed_Length (Dest));
            pragma Assert (Num_Bytes_To_Copy <= Signed_Length (Src));

            -- Perform the copy:
            Dest (Dest'Last - Num_Bytes_To_Copy + 1 .. Dest'Last) := Src (Src'Last - Num_Bytes_To_Copy + 1 .. Src'Last);
            return Num_Bytes_To_Copy;
         end;
      else
         return Natural'First;
      end if;
   end Safe_Right_Copy;

   procedure Safe_Right_Copy (Dest : in out Byte_Array; Src : in Byte_Array) is
      Ignore : Natural;
   begin
      Ignore := Safe_Right_Copy (Dest, Src);
   end Safe_Right_Copy;

   function Safe_Left_Copy (Dest : in out Byte_Array; Src : in Byte_Array) return Natural is
   begin
      -- If the length of both arrays is positive we can do a copy. Otherwise
      -- there is no need to continue.
      if Is_Not_Empty (Src) and then Is_Not_Empty (Dest) then
         declare
            Num_Bytes_To_Copy : constant Natural :=
               Natural'Min (Src'Last - Src'First, Dest'Last - Dest'First) + 1;
         begin
            -- Help static analysis understand that the number of bytes to copy
            -- must be less than or equal to both the destination and src array
            -- lengths at this point. We know that both dest and src cannot be
            -- a null array.
            pragma Assert (Num_Bytes_To_Copy <= Signed_Length (Dest));
            pragma Assert (Num_Bytes_To_Copy <= Signed_Length (Src));

            -- Perform the copy:
            Dest (Dest'First .. Dest'First + Num_Bytes_To_Copy - 1) := Src (Src'First .. Src'First + Num_Bytes_To_Copy - 1);
            return Num_Bytes_To_Copy;
         end;
      else
         return Natural'First;
      end if;
   end Safe_Left_Copy;

   procedure Safe_Left_Copy (Dest : in out Byte_Array; Src : in Byte_Array) is
      Ignore : Natural;
   begin
      Ignore := Safe_Left_Copy (Dest, Src);
   end Safe_Left_Copy;

   -- Create a bit mask of ones starting from LSB. i.e. for a size of 6 create the mask
   -- 0x3F or 0b00111111
   function Bit_Mask (Num_Ones : in Natural) return Unsigned_8 is (Shift_Left (1, Num_Ones) - 1)
      with Inline => True;
   -- Does a modulo 8 operation but the result is between 1-8 instead of 0-7. So 8 is traded for 0.
   function Mod_1_8 (Val : in Natural) return Natural is (((Val - 1) mod 8) + 1)
      with Inline => True;

   function Extract_Poly_Type (Src : in Byte_Array; Offset : in Natural; Size : in Positive; Is_Signed : in Boolean; Value : out Poly_32_Type) return Extract_Poly_Type_Status is
   begin
      -- Initialize out parameter:
      Value := [0, 0, 0, 0];

      -- Validate offset and size. Size must be <= 32 bits. The size + offset must not overflow
      -- the size of the byte array. Size must be greater than zero.
      if Size > Poly_32_Type'Object_Size or else
          Offset + Size > Src'Length * Byte'Object_Size
      then
         return Error;
      end if;

      -- OK the we can safely extract the data from the byte array.
      declare
         -- Calculate indexes in the destination we need to copy to.
         Num_Dest_Bytes : constant Natural := (Size + Byte'Object_Size - 1) / Byte'Object_Size;
         First_Dest_Idx : constant Natural := Value'Last - Num_Dest_Bytes + 1;
         Last_Dest_Idx : constant Natural := Value'Last;
         -- Calculate the first byte we need to copy from the source, the offset rounded down to nearest byte:
         First_Src_Idx : constant Natural := Src'First + (Offset / Byte'Object_Size);
         -- Calculate the last byte we need to extract from the source to completely encapsulate the data we seek.
         Last_Src_Idx : constant Natural := Src'First + ((Offset + Size - 1) / Byte'Object_Size);
         -- Index for grabbing bytes out of source array.
         Src_Idx : Natural := Last_Src_Idx;
         -- Calculate the number of bits we need to shift everything right, this 8 minus the offset mod 8,
         -- i.e. the number of padding bits between the last bit we care about and the last whole byte we need
         -- data from.
         Left_Shift : constant Natural := (Offset + Size) mod Byte'Object_Size;
         Right_Shift : constant Natural := (Byte'Object_Size - Left_Shift) mod Byte'Object_Size;
         -- Create a mask for the shifts.
         Right_Mask : constant Unsigned_8 := Bit_Mask (Mod_1_8 (Left_Shift));
         Left_Mask : constant Unsigned_8 := 16#FF# xor Right_Mask;
         Last_Mask : constant Unsigned_8 := Bit_Mask (Mod_1_8 (Size));
      begin
         -- Copy from the byte array into the poly type, applying bit manipulation and masking
         -- where necessary to extract the bits we want.
         for Dest_Idx in reverse First_Dest_Idx .. Last_Dest_Idx loop
            -- Copy over the right most byte and shift it over to remove any padding.
            Value (Dest_Idx) := Shift_Right (Src (Src_Idx), Right_Shift) and Right_Mask;

            -- If we didn't just copy over the last destination byte then peek ahead...
            if Src_Idx > First_Src_Idx then
               -- Combine with relevant bits from the next byte to copy.
               Value (Dest_Idx) := @ or (Shift_Left (Src (Src_Idx - 1), Left_Shift) and Left_Mask);
               -- Decrement the source index:
               Src_Idx := @ - 1;
            end if;
         end loop;

         -- Apply the appropriate mask to the last byte copied:
         Value (First_Dest_Idx) := @ and Last_Mask;
      end;

      -- If the value we extracted is signed, then we need to handle the case where the value is
      -- negative (i.e. the left-most bit is set to 1). If this is the case, and we extracted some
      -- value with a size less than the 32-bit poly type, then we have leading zeros in the polytype
      -- which is not going to give the appropriate value for a 32-bit signed integer (or any integer
      -- of greater representation size than the size of the extracted value). In this case we need
      -- to set those leading bits to 1.
      if Is_Signed then
         declare
            -- Extract the left-most bit:
            Byte_Idx : constant Natural := Poly_32_Type'Last - ((Size - 1) / Byte'Object_Size);
            Bit_Idx : constant Natural := (Size - 1) mod Byte'Object_Size;
            Left_Most_Bit : constant Unsigned_32 := Unsigned_32 (Shift_Right (Value (Byte_Idx), Bit_Idx)) and 16#1#;
         begin
            -- If the left most bit is 1, then we need to set all the bits left of it to 1.
            if Left_Most_Bit /= 0 then
               -- Set all full bytes to 1.
               for Idx in Poly_32_Type'First .. Byte_Idx - 1 loop
                  Value (Idx) := Byte'Last;
               end loop;
               -- Set the lead of any partial byte to 1.
               Value (Byte_Idx) := @ or (Bit_Mask (Bit_Idx + 1) xor Byte'Last);
            end if;
         end;
      end if;

      return Success;
   end Extract_Poly_Type;

   function Set_Poly_Type (Dest : in out Byte_Array; Offset : in Natural; Size : in Positive; Value : in Poly_32_Type; Truncation_Allowed : Boolean := False) return Set_Poly_Type_Status is
      Value_To_Set : Poly_32_Type := Value;
   begin
      -- Validate offset and size. Size must be <= 32 bits. The size + offset must not overflow
      -- the size of the byte array. Size must be greater than zero.
      if Size > Poly_32_Type'Object_Size or else
          Offset + Size > (Dest'Last - Dest'First + 1) * Byte'Object_Size
      then
         return Error;
      end if;
      pragma Annotate (GNATSAS, Intentional, "condition predetermined", "Defensive check - no current callers use Size > 32, but kept for safety");

      -- Check the polytype value to see if it can be stored without truncation:
      declare
         -- Calculate the limit that, if exceeded, would mean truncation would occur.
         Limit : constant Unsigned_32 := Shift_Right (16#FFFFFFFF#, Unsigned_32'Object_Size - Size);
         -- Calculate the unsigned integer value of the polytype. We do this manually to
         -- avoid any dependency on a packed record in this low level package. This assumes
         -- the value stored is in big endian.
         Value_Int : constant Unsigned_32 := Unsigned_32 (Value (Poly_32_Type'First + 3)) +
            Shift_Left (Unsigned_32 (Value (Poly_32_Type'First + 2)), 8) +
            Shift_Left (Unsigned_32 (Value (Poly_32_Type'First + 1)), 16) +
            Shift_Left (Unsigned_32 (Value (Poly_32_Type'First + 0)), 24);
      begin
         -- See if truncation would occur:
         if Value_Int > Limit then
            if Truncation_Allowed then
               -- Perform the truncation of Value so we only set "Size" bits in the output:
               declare
                  Value_Int_Trunc : constant Unsigned_32 := Value_Int and Limit;
               begin
                  Value_To_Set := [
                     3 => Unsigned_8 (Value_Int_Trunc and 16#FF#),
                     2 => Unsigned_8 (Shift_Right (Value_Int_Trunc, 8) and 16#FF#),
                     1 => Unsigned_8 (Shift_Right (Value_Int_Trunc, 16) and 16#FF#),
                     0 => Unsigned_8 (Shift_Right (Value_Int_Trunc, 24) and 16#FF#)
                  ];
               end;
            else
               return Truncation_Error;
            end if;
         end if;
      end;

      -- OK, we can safely set the data from the byte polytype into the byte array.
      declare
         -- Calculate the number of bytes in the source that we need to copy from:
         Num_Src_Bytes : constant Natural := (Size + Byte'Object_Size - 1) / Byte'Object_Size;
         First_Src_Idx : constant Natural := Value_To_Set'Last - Num_Src_Bytes + 1;
         Last_Src_Idx : constant Natural := Value_To_Set'Last;

         -- Calculate indexes in the destination we need to copy to.
         First_Dest_Idx : constant Natural := Dest'First + (Offset / Byte'Object_Size);
         Last_Dest_Idx : constant Natural := Dest'First + (Size + Offset - 1) / Byte'Object_Size;

         -- Calculate the number of bits we need to shift everything left from the source as we copy to
         -- the destination.
         Right_Shift : constant Natural := (Offset + Size) mod Byte'Object_Size;
         Left_Shift : constant Natural := (Byte'Object_Size - Right_Shift) mod Byte'Object_Size;
         -- Create masks for the shifts.
         Left_Mask : constant Unsigned_8 := Shift_Left (16#FF#, Left_Shift);
         Right_Mask : constant Unsigned_8 := 16#FF# xor Left_Mask;
         Last_Mask : constant Unsigned_8 := Shift_Left (16#FF#, Byte'Object_Size - (Offset mod Byte'Object_Size)) and 16#FF#;

         -- Save off the first destination data, since there may be bits in here we don't want to overwrite, but
         -- the algorithm corrupts.
         First_Dest_Data : constant Byte := Dest (First_Dest_Idx);

         -- Index for selecting bytes to write in the destination array.
         Dest_Idx : Integer := Last_Dest_Idx;
      begin
         -- Copy each byte from the source poly type to the correct location in the destination byte array
         for Src_Idx in reverse First_Src_Idx .. Last_Src_Idx loop
            -- Write the source bits to the destination byte that apply here. Some of the bits in
            -- this source byte might belong in the next destination byte based on the offset. That
            -- case is handled below.
            Dest (Dest_Idx) := (Shift_Left (Value_To_Set (Src_Idx), Left_Shift) and Left_Mask) or
                                        (Dest (Dest_Idx) and Right_Mask);

            -- Some of the bits in the source byte might need to be written to the next byte of
            -- the destination. If there is still room left in the destination, write the necessary
            -- source bits.
            if Dest_Idx > First_Dest_Idx then
               Dest (Dest_Idx - 1) := Shift_Right (Value_To_Set (Src_Idx), Right_Shift) and Right_Mask;
               -- Decrement the source index:
               Dest_Idx := @ - 1;
            end if;
         end loop;

         -- Fix the last byte by restoring any data we overwrote that we should not have:
         Dest (First_Dest_Idx) := (First_Dest_Data and Last_Mask) or Dest (First_Dest_Idx);
      end;

      return Success;
   end Set_Poly_Type;

end Byte_Array_Util;
