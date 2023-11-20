with Example_Record.Assertion;

procedure Main is
   use Example_Record;
   use Example_Record.Assertion;
   -- A packed version of the type.
   Packed_Type_1 : constant Example_Record.T := (Value_1 => 2, Value_2 => -1, Value_3 => Green, Value_4 => 0.5);
   Packed_Type_2 : constant Example_Record.T := (Value_1 => 5, Value_2 => -1, Value_3 => Green, Value_4 => 0.4);
begin
   -- See if the records are not equal (should pass):
   Example_Record_Assert.Neq (Packed_Type_1, Packed_Type_2);

   -- See if the records are equal (should fail):
   Example_Record_Assert.Eq (Packed_Type_1, Packed_Type_2);
end Main;
