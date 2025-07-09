pragma Profile (Ravenscar);
with Ada.Text_IO; use Ada.Text_IO;
with Option_Type;

procedure Test is
   -- Create an integer option type:
   package Integer_Option_Type is new Option_Type (Integer);

   -- Statically declare some constrained option types:
   A : Integer_Option_Type.Option (True);
   B : Integer_Option_Type.Option (False);

   -- Function which returns unconstrained option type:
   function Return_Option (Valid : in Boolean) return Integer_Option_Type.Option is
      C : Integer_Option_Type.Option (Valid);
   begin
      if C.Has_Element then
         C.Element := 12;
      end if;
      return C;
   end Return_Option;

   -- Create some dynamically set option types from function:
   D : constant Integer_Option_Type.Option := Return_Option (False);
   E : constant Integer_Option_Type.Option := Return_Option (True);

   -- Example which shows usual pattern:
   function Return_Status (Valid : in Boolean; Value : out Integer) return Boolean is
   begin
      -- Initialize out parameter
      Value := 0;

      if Valid then
         Value := 12;
      end if;

      -- Value is not initialized if valid /= True!!
      return Valid;
   end Return_Status;

   -- Compare usual pattern:
   F, G : Integer;
   Stat_1 : constant Boolean := Return_Status (True, F);
   Stat_2 : constant Boolean := Return_Status (False, G);
begin
   Put_Line ("Creating option types... ");

   A.Element := 10;
   if A.Has_Element then
      Put_Line ("a " & Integer'Image (A.Element));
   end if;

   if B.Has_Element then
      pragma Warnings (Off, "this code can never be executed and has been deleted");
      Put_Line ("b " & Integer'Image (B.Element));
      pragma Warnings (On, "this code can never be executed and has been deleted");
   end if;

   -- Put_Line("d" & Integer'Image(d.Element));
   -- ^ This would throw constraint error
   if D.Has_Element then
      Put_Line ("d " & Integer'Image (D.Element));
      -- ^ Not executed
   end if;

   if E.Has_Element then
      Put_Line ("e " & Integer'Image (E.Element));
   end if;

   if Stat_1 then
      Put_Line ("f " & Integer'Image (F));
   end if;

   -- The value of g is undefined here since it was not initialized, it is
   -- potentially dangerous to use if stat_2 is not checked.
   Put_Line ("g " & Integer'Image (G));
   if Stat_2 then
      Put_Line ("g " & Integer'Image (G));
   end if;

   Put_Line ("passed.");
end Test;
