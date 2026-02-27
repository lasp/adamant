--------------------------------------------------------------------------------
-- Oo_Package Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Oo_Package.Tester;

package body Oo_Package_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Initialize the object oriented package by calling the Init function:
      Self.Oo.Init (N => -15);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Init (Self : in out Instance) is
   begin
      -- This is a white-box test. We need to look inside of the
      -- Instance's private state and make sure that the value of
      -- "n" got set to what we expect, 15.
      Integer_Assert.Eq (Oo_Package.Tester.Get_N (Self.Oo), -15);
   end Test_Init;

   overriding procedure Test_Add (Self : in out Instance) is
   begin
      -- The result should equal -8;
      Integer_Assert.Eq (Self.Oo.Add_N (7), -8);

      -- Make sure the operation above did not alter the
      -- object's internal state. Only the Init function should
      -- be able to do that.
      Integer_Assert.Eq (Oo_Package.Tester.Get_N (Self.Oo), -15);
   end Test_Add;

end Oo_Package_Tests.Implementation;
