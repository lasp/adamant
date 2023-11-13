with Ada.Text_IO; use Ada.Text_IO;
with Example_Record.Representation;

procedure Main is
   use Example_Record;
   -- A packed version of the type.
   Packed_Type_1 : constant Example_Record.T := (Value_1 => 2, Value_2 => -1, Value_3 => Green, Value_4 => 0.5);
begin
   -- Print the packed record to the terminal in human readable form:
   Put_Line ("Human readable:");
   Put_Line (Example_Record.Representation.Image (Packed_Type_1));
   Put_Line ("");

   -- Print the packed record to the terminal in log friendly form:
   Put_Line ("Log friendly:");
   Put_Line (Example_Record.Representation.To_Tuple_String (Packed_Type_1));
   Put_Line ("");
end Main;
