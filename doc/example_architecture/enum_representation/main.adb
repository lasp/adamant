with Ada.Text_IO; use Ada.Text_IO;
with Example_Enums.Representation;

procedure Main is
   use Example_Enums;
   use Example_Enums.Representation;
   use Example_Enums.First_Enum;
   -- A enum instantiation.
   Enum_1 : constant First_Enum.E := Blue;
begin
   -- Print the enum to the terminal in human readable form:
   Put_Line ("Human readable:");
   Put_Line (First_Enum_Image (Enum_1));
   Put_Line ("");

   -- Print the enum to the terminal in log friendly form:
   Put_Line ("Log friendly:");
   Put_Line (First_Enum_To_Tuple_String (Enum_1));
   Put_Line ("");
end Main;
