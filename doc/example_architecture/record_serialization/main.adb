with Example_Record;

procedure Main is
   use Example_Record;
   -- A packed version of the type.
   Packed_Type : Example_Record.T := (Value_1 => 2, Value_2 => -1, Value_3 => Green, Value_4 => 0.5);
   -- A byte array that is the same size as the type.
   Byte_Array : Example_Record.Serialization.Byte_Array;
begin
   -- Copy from the packed record to a byte array.
   Byte_Array := Example_Record.Serialization.To_Byte_Array (Packed_Type);
   -- Copy from a byte array to a packed record.
   Packed_Type := Example_Record.Serialization.From_Byte_Array (Byte_Array);
end Main;
