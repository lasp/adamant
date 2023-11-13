--------------------------------------------------------------------------------
-- Ccsds_Serial_Interface Tests
--------------------------------------------------------------------------------

--with AUnit.Reporter.Text;
with AUnit.Reporter.Null_Reporter;
with AUnit.Run;
with Ccsds_Serial_Interface_Tests.Implementation.Suite;

procedure Test is
   -- Create runner for test suite:
   procedure Runner is new AUnit.Run.Test_Runner (Ccsds_Serial_Interface_Tests.Implementation.Suite.Get);
   -- Use the text reporter:
   -- Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.Null_Reporter.My_Null_Reporter;
begin
   -- Run tests:
   Runner (Reporter);
end Test;
