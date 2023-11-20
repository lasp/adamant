with Example_Enums.Assertion;

procedure Main is
   use Example_Enums;
   use Example_Enums.First_Enum;
   use Example_Enums.Assertion;
   -- A enum instantiation.
   Enum_1 : constant First_Enum.E := Green;
begin
   -- See if the enums are not equal (should pass):
   First_Enum_Assert.Neq (Enum_1, Blue);

   -- See if the enums are equal (should not pass):
   First_Enum_Assert.Eq (Enum_1, Purple);
end Main;
