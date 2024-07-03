with Ada.Text_IO; use Ada.Text_IO;
with Example_Array.Representation;

procedure Main is
   -- A packed version of the type.
   Packed_Type_1 : constant Example_Array.T := [11, 22, 33, 44, 55, 66, 77, 88];
begin
   -- Print the packed array to the terminal in human readable form:
   Put_Line ("Human readable:");
   Put_Line (Example_Array.Representation.Image (Packed_Type_1));
   Put_Line ("");

   -- Print the packed array to the terminal in log friendly form:
   Put_Line ("Log friendly:");
   Put_Line (Example_Array.Representation.To_Tuple_String (Packed_Type_1));
   Put_Line ("");
end Main;
