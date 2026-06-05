--------------------------------------------------------------------------------
-- Protected_Variables Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Protected_Variables;
with Smart_Assert;

package body Protected_Variables_Tests.Implementation is

   -- Test instantiations. We use small concrete types -- the tests verify the
   -- semantics of each generic's protected operations, not the concurrency
   -- machinery (which is provided by the Ada runtime).

   package Integer_Variable is new Protected_Variables.Generic_Variable (T => Integer);

   type Byte_Mod is mod 2**8;
   package Byte_Counter is new Protected_Variables.Generic_Protected_Counter (T => Byte_Mod);

   type Signed_Range is range -100 .. 100;
   package Signed_Counter is new Protected_Variables.Generic_Protected_Counter_Decrement (T => Signed_Range);

   package Byte_Periodic_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (T => Byte_Mod);

   -- A small record so the Staged_Variable tests exercise the protected
   -- machinery on a composite type, not just a scalar.
   type Pair is record
      A : Integer;
      B : Integer;
   end record;
   package Pair_Staged is new Protected_Variables.Generic_Staged_Variable (T => Pair);

   -- Smart-assert instantiations for the custom types declared above.
   -- Integer_Assert / Boolean_Assert come from Basic_Assertions.
   package Byte_Mod_Assert is new Smart_Assert.Discrete (Byte_Mod, Byte_Mod'Image);
   package Signed_Range_Assert is new Smart_Assert.Discrete (Signed_Range, Signed_Range'Image);
   package Pair_Assert is new Smart_Assert.Basic (Pair, Pair'Image);

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

   overriding procedure Test_Variable (Self : in out Instance) is
      Ignore : Instance renames Self;
      V : Integer_Variable.Variable;
   begin
      V.Set_Var (42);
      Integer_Assert.Eq (V.Get_Var, 42);

      V.Set_Var (-1);
      Integer_Assert.Eq (V.Get_Var, -1);
   end Test_Variable;

   overriding procedure Test_Protected_Counter (Self : in out Instance) is
      Ignore : Instance renames Self;
      C : Byte_Counter.Counter;
      Prev : Byte_Mod;
   begin
      -- Default starts at 0.
      Byte_Mod_Assert.Eq (C.Get_Count, 0);

      -- Set / Get round trip.
      C.Set_Count (10);
      Byte_Mod_Assert.Eq (C.Get_Count, 10);

      -- Default increment is 1.
      C.Increment_Count;
      Byte_Mod_Assert.Eq (C.Get_Count, 11);

      -- Increment by N.
      C.Increment_Count (To_Add => 5);
      Byte_Mod_Assert.Eq (C.Get_Count, 16);

      -- Reset.
      C.Reset_Count;
      Byte_Mod_Assert.Eq (C.Get_Count, 0);

      -- Return-previous semantics.
      C.Set_Count (50);
      C.Increment_Count_And_Return_Previous (Prev_Count => Prev, To_Add => 3);
      Byte_Mod_Assert.Eq (Prev, 50);
      Byte_Mod_Assert.Eq (C.Get_Count, 53);

      -- Modular wrap: 250 + 10 mod 256 = 4.
      C.Set_Count (250);
      C.Increment_Count (To_Add => 10);
      Byte_Mod_Assert.Eq (C.Get_Count, 4);
   end Test_Protected_Counter;

   overriding procedure Test_Protected_Counter_Decrement (Self : in out Instance) is
      Ignore : Instance renames Self;
      C : Signed_Counter.Counter;
      Prev : Signed_Range;
   begin
      -- Default starts at 0.
      Signed_Range_Assert.Eq (C.Get_Count, 0);

      -- Set / Get.
      C.Set_Count (50);
      Signed_Range_Assert.Eq (C.Get_Count, 50);

      -- Default decrement is 1.
      C.Decrement_Count;
      Signed_Range_Assert.Eq (C.Get_Count, 49);

      -- Decrement by N.
      C.Decrement_Count (To_Subtract => 9);
      Signed_Range_Assert.Eq (C.Get_Count, 40);

      -- Reset.
      C.Reset_Count;
      Signed_Range_Assert.Eq (C.Get_Count, 0);

      -- Negative range (counter is signed).
      C.Decrement_Count (To_Subtract => 20);
      Signed_Range_Assert.Eq (C.Get_Count, -20);

      -- Return-previous semantics.
      C.Set_Count (10);
      C.Decrement_Count_And_Return_Previous (Prev_Count => Prev, To_Subtract => 4);
      Signed_Range_Assert.Eq (Prev, 10);
      Signed_Range_Assert.Eq (C.Get_Count, 6);
   end Test_Protected_Counter_Decrement;

   overriding procedure Test_Protected_Periodic_Counter (Self : in out Instance) is
      Ignore : Instance renames Self;
      C : Byte_Periodic_Counter.Counter;
   begin
      -- Default period is 1, count is 0. Count mod 1 = 0, so Is_Count_At_Period is True.
      Byte_Mod_Assert.Eq (C.Get_Period, 1);
      Byte_Mod_Assert.Eq (C.Get_Count, 0);
      Boolean_Assert.Eq (C.Is_Count_At_Period, True);

      -- Set period and reset count.
      C.Set_Period_And_Reset_Count (Value => 5);
      Byte_Mod_Assert.Eq (C.Get_Period, 5);
      Byte_Mod_Assert.Eq (C.Get_Count, 0);
      Boolean_Assert.Eq (C.Is_Count_At_Period, True);

      -- Increment: count wraps mod period.
      C.Increment_Count;
      Byte_Mod_Assert.Eq (C.Get_Count, 1);
      Boolean_Assert.Eq (C.Is_Count_At_Period, False);

      C.Increment_Count (To_Add => 4);
      Byte_Mod_Assert.Eq (C.Get_Count, 0);
      Boolean_Assert.Eq (C.Is_Count_At_Period, True);

      -- Period = 0: Is_Count_At_Period must always return False, and Increment is a no-op.
      C.Set_Period_And_Reset_Count (Value => 0);
      Boolean_Assert.Eq (C.Is_Count_At_Period, False);
      C.Increment_Count;
      Byte_Mod_Assert.Eq (C.Get_Count, 0);

      -- Reset_Count alone (without changing period).
      C.Set_Period_And_Reset_Count (Value => 3);
      C.Increment_Count (To_Add => 2);
      Byte_Mod_Assert.Eq (C.Get_Count, 2);
      C.Reset_Count;
      Byte_Mod_Assert.Eq (C.Get_Count, 0);
      Byte_Mod_Assert.Eq (C.Get_Period, 3);
   end Test_Protected_Periodic_Counter;

   overriding procedure Test_Staged_Variable (Self : in out Instance) is
      Ignore : Instance renames Self;
      S : Pair_Staged.Staged_Variable;
      Out_Val : Pair;
   begin
      -- Default state.
      Boolean_Assert.Eq (S.Is_Staged, False);

      -- Stage a value.
      S.Stage ((A => 1, B => 2));
      Boolean_Assert.Eq (S.Is_Staged, True);

      -- Copy_From_Staged returns the value and clears.
      S.Copy_From_Staged (Out_Val);
      Pair_Assert.Eq (Out_Val, (A => 1, B => 2));
      Boolean_Assert.Eq (S.Is_Staged, False);

      -- Re-stage works after a copy.
      S.Stage ((A => 7, B => 8));
      Boolean_Assert.Eq (S.Is_Staged, True);
      S.Copy_From_Staged (Out_Val);
      Pair_Assert.Eq (Out_Val, (A => 7, B => 8));

      -- Multiple Stage calls before Copy_From_Staged: only the last value persists.
      S.Stage ((A => 10, B => 20));
      S.Stage ((A => 30, B => 40));
      S.Stage ((A => 50, B => 60));
      Boolean_Assert.Eq (S.Is_Staged, True);
      S.Copy_From_Staged (Out_Val);
      Pair_Assert.Eq (Out_Val, (A => 50, B => 60));
      Boolean_Assert.Eq (S.Is_Staged, False);
   end Test_Staged_Variable;

end Protected_Variables_Tests.Implementation;
