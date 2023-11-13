--------------------------------------------------------------------------------
-- Provide_Modify Tests
--------------------------------------------------------------------------------

with AUnit.Reporter.Text;
with AUnit.Run;
with Unit_Tests.Implementation.Suite;

procedure Test is
   -- Create runner for test suite:
   procedure Runner is new AUnit.Run.Test_Runner (Unit_Tests.Implementation.Suite.Get);
   -- Use the text reporter:
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   -- Run tests:
   Runner (Reporter);
end Test;
