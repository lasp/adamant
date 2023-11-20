--------------------------------------------------------------------------------
-- Simple_Package Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;

package body Simple_Package_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- TODO Insert custom set up code here.
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- TODO Insert custom cleanup code here.
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_That_Should_Pass (Self : in out Instance) is
      -- TODO declarations
   begin
      -- TODO replace the following with actual test code.
      Assert (False, "Test 'Test_That_Should_Pass' is unimplemented.");
   end Test_That_Should_Pass;

   overriding procedure Test_That_Should_Fail (Self : in out Instance) is
      -- TODO declarations
   begin
      -- TODO replace the following with actual test code.
      Assert (False, "Test 'Test_That_Should_Fail' is unimplemented.");
   end Test_That_Should_Fail;

end Simple_Package_Tests.Implementation;
