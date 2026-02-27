--------------------------------------------------------------------------------
-- History Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with History;

package body History_Tests.Implementation is

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

   overriding procedure Test_History (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      package Natural_History is new History (Natural);
      Hist : Natural_History.Instance;
      Status : Boolean;
      Value : Natural;
      Ignore : Natural;
      Len : Positive;
   begin
      Hist.Init (10);
      Status := Hist.Is_Full;
      Boolean_Assert.Eq (Status, False);
      Status := Hist.Is_Empty;
      Boolean_Assert.Eq (Status, True);
      Len := Hist.Get_Depth;
      Natural_Assert.Eq (Len, 10);

      for Idx in 1 .. Len loop
         Hist.Push (Idx);
         Len := Hist.Get_Count;
         Natural_Assert.Eq (Len, Idx);
      end loop;
      Status := Hist.Is_Full;
      Boolean_Assert.Eq (Status, True);
      Status := Hist.Is_Empty;
      Boolean_Assert.Eq (Status, False);
      -- hist.push(16); -- Should cause assertion, but we can't actually test aunit with aunit :(

      for Idx in 1 .. Len loop
         Value := Hist.Get (Idx);
         Natural_Assert.Eq (Value, Idx);
      end loop;
      -- value := hist.get(11); -- Should cause assertion, but we can't actually test aunit with aunit :(

      Status := Hist.Is_Full;
      Boolean_Assert.Eq (Status, True);
      Status := Hist.Is_Empty;
      Boolean_Assert.Eq (Status, False);
      Hist.Clear;
      Status := Hist.Is_Full;
      Boolean_Assert.Eq (Status, False);
      Status := Hist.Is_Empty;
      Boolean_Assert.Eq (Status, True);
      -- ignore := hist.get(1); -- Should cause assertion, but we can't actually test AUnit with AUnit :(
      Hist.Destroy;
      pragma Unreferenced (Hist);
   end Test_History;

end History_Tests.Implementation;
