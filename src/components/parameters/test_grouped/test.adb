--------------------------------------------------------------------------------
-- Parameters Grouped Tests
--------------------------------------------------------------------------------

with AUnit.Reporter.Text;
with AUnit.Run;
with Parameters_Grouped_Tests.Implementation.Suite;
-- Make sure any terminating tasks are handled and an appropriate
-- error message is printed.
with Unit_Test_Termination_Handler;
pragma Unreferenced (Unit_Test_Termination_Handler);

procedure Test is
   -- Create runner for test suite:
   procedure Runner is new AUnit.Run.Test_Runner (Parameters_Grouped_Tests.Implementation.Suite.Get);
   -- Use the text reporter:
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   -- Add color output to test run:
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   -- Run tests:
   Runner (Reporter);
end Test;
