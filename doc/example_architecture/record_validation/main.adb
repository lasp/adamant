with Ada.Text_IO; use Ada.Text_IO;
with Example_Record.Validation, Interfaces;

procedure Main is
   use Example_Record, Interfaces;
   -- A valid packed record. To call Validation.Valid on it, convert it to a byte array first.
   Packed_Type_1 : constant Example_Record.T := (Value_1 => 2, Value_2 => -1, Value_3 => Green, Value_4 => 0.5);
   Bytes_1 : constant Example_Record.Serialization.Byte_Array := Example_Record.Serialization.To_Byte_Array (Packed_Type_1);
   -- A byte array containing all 0xFF -- out of range for the enumeration field, so invalid.
   Bytes_2 : constant Example_Record.Serialization.Byte_Array := [others => 255];
   Errant_Field : Unsigned_32 := 0;
begin
   if Example_Record.Validation.Valid (Bytes_1, Errant_Field) then
      null; -- Should go here, since record is valid.
   else
      pragma Assert (False, "Record 1 is invalid at field " & Unsigned_32'Image (Errant_Field) & "!");
   end if;

   if Example_Record.Validation.Valid (Bytes_2, Errant_Field) then
      null; -- Should not go here, since record is not valid.
   else
      Put_Line ("Record 2 is invalid at field " & Unsigned_32'Image (Errant_Field) & "!");
   end if;
end Main;
