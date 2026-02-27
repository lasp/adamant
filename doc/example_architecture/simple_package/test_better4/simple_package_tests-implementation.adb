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
      -- Call base Set_Up:
      -- Simple_Package_Tests.Base_Instance (Self).Set_Up;
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Call base Tear_Down:
      -- Simple_Package_Tests.Base_Instance (Self).Tear_Down;
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
      Self.Log ("Performing equals assertion: Added = " & Simple_Package.Add_Two_Numbers (A, B)'Image & " Expected = -8");
      Integer_Assert.Eq (Simple_Package.Add_Two_Numbers (A, B), -8);
      -- Result not equal -9
      Self.Log ("Performing not equal assertion: Added = " & Simple_Package.Add_Two_Numbers (A, B)'Image & " Expected /= -9");
      Integer_Assert.Neq (Simple_Package.Add_Two_Numbers (A, B), -9);
      -- Result should be less than 100
      Self.Log ("Performing less than assertion: Added = " & Simple_Package.Add_Two_Numbers (A, B)'Image & " Expected < 100");
      Integer_Assert.Lt (Simple_Package.Add_Two_Numbers (A, B), 100);
      -- Result should be greater than or equal to -100
      Self.Log ("Performing greater or equal than assertion: Added = " & Simple_Package.Add_Two_Numbers (A, B)'Image & " Expected >= -100");
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
