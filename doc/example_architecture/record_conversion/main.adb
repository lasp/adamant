with Example_Record;

procedure Main is
   use Example_Record;
   -- A packed big-endian version of the type.
   Packed_Be_Type : Example_Record.T := (Value_1 => 2, Value_2 => -1, Value_3 => Green, Value_4 => 0.5);
   -- A packed little-endian version of the type.
   Packed_Le_Type : Example_Record.T_Le;
   -- An unpacked version of the type.
   Unpacked_Type : Example_Record.U;
begin
   -- Convert from big-endian packed to unpacked:
   Unpacked_Type := Unpack (Packed_Be_Type);
   -- Convert from unpacked to big-endian packed:
   Packed_Be_Type := Pack (Unpacked_Type);
   -- Convert from big-endian to little-endian:
   Packed_Le_Type := Swap_Endianness (Packed_Be_Type);
   -- Convert from little-endian to big-endian:
   Packed_Be_Type := Swap_Endianness (Packed_Le_Type);
   -- Convert from little-endian packed to unpacked:
   Unpacked_Type := Unpack (Packed_Le_Type);
end Main;
