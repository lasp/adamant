--------------------------------------------------------------------------------
-- Simple_Package Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Simple_Package;

package body Simple_Package_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_That_Should_Pass (Self : in out Instance) is
      Ignore : Instance renames Self;
      A : constant Integer := 7;
      B : constant Integer := -15;
   begin
      -- Result should equal -8
      Integer_Assert.Eq (Simple_Package.Add_Two_Numbers (A, B), -8);
      -- Result not equal -9
      Integer_Assert.Neq (Simple_Package.Add_Two_Numbers (A, B), -9);
      -- Result should be less than 100
      Integer_Assert.Lt (Simple_Package.Add_Two_Numbers (A, B), 100);
      -- Result should be greater than or equal to -100
      Integer_Assert.Ge (Simple_Package.Add_Two_Numbers (A, B), -100);
   end Test_That_Should_Pass;

   overriding procedure Test_That_Should_Fail (Self : in out Instance) is
      Ignore : Instance renames Self;
      A : constant Integer := 7;
      B : constant Integer := -15;
   begin
      -- Result should equal 99? This test will fail, oh no!
      Integer_Assert.Neq (Simple_Package.Add_Two_Numbers (A, B), 99);
   end Test_That_Should_Fail;

end Simple_Package_Tests.Implementation;
