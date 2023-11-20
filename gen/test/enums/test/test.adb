with Ada.Text_IO; use Ada.Text_IO;
with Test_Enums; use Test_Enums;
with Test_Enums.Representation; use Test_Enums.Representation;
with Test_Enums.Assertion; use Test_Enums.Assertion;

procedure Test is
   Fe : First_Enum.E := First_Enum.Black;
   Se : Second_Enum.E := Second_Enum.Hola;
begin
   Put_Line ("Testing enumerations: ");
   Put_Line ("");

   Put_Line ("First_Enum: " & First_Enum_Image (Fe));
   Put_Line ("Second_Enum: " & Second_Enum_Image (Se));

   Fe := First_Enum.Blue;
   Se := Second_Enum.Yellow;

   Put_Line ("First_Enum: " & First_Enum_Image (Fe));
   Put_Line ("Second_Enum: " & Second_Enum_Image (Se));

   First_Enum_Assert.Eq (Fe, First_Enum.Blue);
   Second_Enum_Assert.Eq (Se, Second_Enum.Yellow);

   Put_Line ("Done.");
end Test;
