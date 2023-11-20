--------------------------------------------------------------------------------
-- Queued Tests
--------------------------------------------------------------------------------

-- Tell the compiler that we are using Ravenscar
with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Test_Sequential;
with Test_Concurrent;
-- Make sure any terminating tasks are handled and an appropriate
-- error message is printed.
with Unit_Test_Termination_Handler;
pragma Unreferenced (Unit_Test_Termination_Handler);

procedure Test is
begin
   Put_Line ("----------------------------------------------");
   Put_Line ("Beginning sequential test.");
   Put_Line ("----------------------------------------------");
   Test_Sequential.Test;
   New_Line;
   Put_Line ("----------------------------------------------");
   Put_Line ("Beginning concurrent test.");
   Put_Line ("----------------------------------------------");
   Test_Concurrent.Test;
   New_Line;
end Test;
