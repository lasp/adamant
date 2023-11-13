--------------------------------------------------------------------------------
-- Parameter_Component Tests
--------------------------------------------------------------------------------

with AUnit.Reporter.Text;
with AUnit.Run;
with Parameter_Component_Tests.Implementation.Suite;

procedure Test is
   -- Create runner for test suite:
   procedure Runner is new AUnit.Run.Test_Runner (Parameter_Component_Tests.Implementation.Suite.Get);
   -- Use the text reporter:
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   -- Add color output to test run:
   AUnit.Reporter.Text.Set_Use_ANSI_Colors (Reporter, True);
   -- Run tests:
   Runner (Reporter);
end Test;
