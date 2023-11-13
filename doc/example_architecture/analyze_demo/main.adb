with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   Outer_Var : Integer := 6;
   Old : Integer;

   procedure Set_Global (New_Val : in Integer; Old_Val : out Integer) is
   begin
      if New_Val < 20 then
         Old_Val := Outer_Var;
      end if;
      Outer_Var := New_Val;
   end Set_Global;

begin
   Set_Global (29, Old);
   Put_Line ("Set to " & Integer'Image (Outer_Var) &
      ", old value was " & Integer'Image (Old));
end Main;
