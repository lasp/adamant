with Simple_Package;

procedure Test is
   A : constant Integer := 7;
   B : constant Integer := -15;
   C : Integer;
begin
   -- Run the test:
   C := Simple_Package.Add_Two_Numbers (A, B);

   -- Run check using the basic Ada assertion statement, this should pass.
   pragma Assert (C = -8, "Failed assertion 1, c = " & Integer'Image (C) & ".");

   -- Expect this check to fail.
   pragma Assert (C /= -8, "Failed assertion 2, c = " & Integer'Image (C) & ".");
end Test;
