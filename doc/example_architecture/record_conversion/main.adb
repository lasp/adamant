with Example_Record;

procedure Main is
   use Example_Record;
   -- A packed version of the type.
   Packed_Type : Example_Record.T := (Value_1 => 2, Value_2 => -1, Value_3 => Green, Value_4 => 0.5);
   -- An unpacked version of the type.
   Unpacked_Type : Example_Record.U;
begin
   -- Convert from packed to unpacked:
   Unpacked_Type := U (Packed_Type);
   -- Convert from unpacked to packed:
   Packed_Type := T (Unpacked_Type);
end Main;
