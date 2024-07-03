with Ada.Text_IO; use Ada.Text_IO;
with Example_Record.Validation, Interfaces;

procedure Main is
   use Example_Record, Interfaces;
   -- A packed version of the type.
   Packed_Type_1 : constant Example_Record.T := (Value_1 => 2, Value_2 => -1, Value_3 => Green, Value_4 => 0.5);
   -- Deserialize a byte array of all 0xFF into the record.
   Packed_Type_2 : constant Example_Record.T := Example_Record.Serialization.From_Byte_Array ([255, 255, 255, 255, 255, 255]);
   Errant_Field : Unsigned_32 := 0;
begin
   if Example_Record.Validation.Valid (Packed_Type_1, Errant_Field) then
      null; -- Should go here, since record is valid.
   else
      pragma Assert (False, "Record 1 is invalid at field " & Unsigned_32'Image (Errant_Field) & "!");
   end if;

   if Example_Record.Validation.Valid (Packed_Type_2, Errant_Field) then
      null; -- Should not go here, since record is not valid.
   else
      Put_Line ("Record 2 is invalid at field " & Unsigned_32'Image (Errant_Field) & "!");
   end if;
end Main;
