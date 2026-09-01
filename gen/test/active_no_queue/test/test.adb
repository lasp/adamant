--------------------------------------------------------------------------------
-- Active_No_Queue Tests
--------------------------------------------------------------------------------

-- Tell the compiler that we are using Ravenscar
--pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;

with Component.Active_No_Queue.Implementation.Tester;

procedure Test is
   Tester : Component.Active_No_Queue.Implementation.Tester.Instance;
   pragma Unreferenced (Tester);
begin
   Put_Line ("If this compiled, it worked.");
   --  Sentinel for the cross test runner.
   Put_Line ("=== ALL TESTS PASSED ===");
end Test;
