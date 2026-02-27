--------------------------------------------------------------------------------
-- Moving_Average Tests Body
--------------------------------------------------------------------------------

with Moving_Average;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;

package body Moving_Average_Tests.Implementation is

   --
   -- Type definitions:
   package My_Moving_Avg is new Moving_Average (Short_Float);

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
   -- Assertion packages:
   -------------------------------------------------------------------------

   package Change_Length_Status_Assert is new Smart_Assert.Basic (My_Moving_Avg.Size_Status, My_Moving_Avg.Size_Status'Image);

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Statistic_Values (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Ma : My_Moving_Avg.Instance;
      Max : Short_Float := 0.0;
      Mean : Short_Float := 0.0;
      Variance : Short_Float := 0.0;
      Ignore_Max : Short_Float := 0.0;
      Ignore_Mean : Short_Float := 0.0;
      Ignore_Variance : Short_Float := 0.0;
   begin
      -- Set the size to the default size of 6
      Ma.Init (Sample_Storage_Size => 6);
      -- Calculate with no data:
      Ma.Calculate_Mean_Variance_Max (Mean, Variance, Max);
      Short_Float_Assert.Eq (Mean, 0.0, Epsilon => 0.000001);
      Short_Float_Assert.Eq (Variance, 0.0, Epsilon => 0.000001);
      Short_Float_Assert.Eq (Max, 0.0, Epsilon => 0.000001);
      -- Test the first time we feed a sample into the calculator
      Ma.Calculate_Mean_Variance_Max (2.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 2.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 0.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 2.0, Epsilon => 0.001);

      -- Now feed until we have a full buffer
      Ma.Calculate_Mean_Variance_Max (4.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (2.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (4.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (2.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (4.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 3.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 1.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 4.0, Epsilon => 0.001);

      -- The buffer should roll after this next call, so we can check this by using a different value to change the max and mean
      Ma.Calculate_Mean_Variance_Max (1.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 17.0 / 6.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 53.0 / 36.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 1.0, Epsilon => 0.001);

      -- Clean up the object when we are done
      Ma.Destroy;
      pragma Unreferenced (Ma);
   end Test_Statistic_Values;

   overriding procedure Test_Reset_Statistics (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Ma : My_Moving_Avg.Instance;
      Max : Short_Float := 0.0;
      Mean : Short_Float := 0.0;
      Variance : Short_Float := 0.0;
      Ignore_Max : Short_Float := 0.0;
      Ignore_Mean : Short_Float := 0.0;
      Ignore_Variance : Short_Float := 0.0;
   begin
      -- implicitly set this one to the default size
      Ma.Init (Sample_Storage_Size => 6, Sample_Calculation_Size => -1);
      -- Start by sending in a few samples, then reset to see if they actually reset the values
      Ma.Calculate_Mean_Variance_Max (5.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (10.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (5.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (10.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 7.5, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 6.25, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 10.0, Epsilon => 0.001);

      -- Call the reset
      Ma.Reset;

      -- Determine if we did reset since old data would affect the outcome if not
      Ma.Calculate_Mean_Variance_Max (2.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 2.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 0.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 2.0, Epsilon => 0.001);

      -- Clean up the object when we are done
      Ma.Destroy;
      pragma Unreferenced (Ma);
   end Test_Reset_Statistics;

   overriding procedure Test_Change_Sample_Length (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Ma : My_Moving_Avg.Instance;
      Status : My_Moving_Avg.Size_Status;
      Max : Short_Float := 0.0;
      Mean : Short_Float := 0.0;
      Variance : Short_Float := 0.0;
      Ignore_Max : Short_Float := 0.0;
      Ignore_Mean : Short_Float := 0.0;
      Ignore_Variance : Short_Float := 0.0;
   begin
      Ma.Init (Sample_Storage_Size => 6, Sample_Calculation_Size => 5);
      -- Start by testing a few samples in to make sure it will reset the values
      Ma.Calculate_Mean_Variance_Max (2.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (2.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (2.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (2.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (2.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 2.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 0.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 2.0, Epsilon => 0.001);

      -- Test a rollover
      Ma.Calculate_Mean_Variance_Max (1.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 1.8, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 0.16, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 1.0, Epsilon => 0.001);

      -- Change the size when the head is further out than the size change
      Status := Ma.Change_Sample_Calculation_Size (4);
      Change_Length_Status_Assert.Eq (Status, My_Moving_Avg.Success);

      -- Now make sure things reset appropriately
      Ma.Calculate_Mean_Variance_Max (5.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 5.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 0.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 5.0, Epsilon => 0.001);

      Ma.Calculate_Mean_Variance_Max (10.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (5.0, Ignore_Mean, Ignore_Variance, Ignore_Max);
      Ma.Calculate_Mean_Variance_Max (10.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 7.5, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 6.25, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 10.0, Epsilon => 0.001);

      -- Now one more to roll over and reset the max
      Ma.Calculate_Mean_Variance_Max (1.0, Mean, Variance, Max);

      Short_Float_Assert.Eq (Mean, 6.5, Epsilon => 0.001);
      Short_Float_Assert.Eq (Variance, 14.25, Epsilon => 0.001);
      Short_Float_Assert.Eq (Max, 1.0, Epsilon => 0.001);

      -- Check that the size change errors if the input is too big
      Status := Ma.Change_Sample_Calculation_Size (20);
      Change_Length_Status_Assert.Eq (Status, My_Moving_Avg.Too_Large);

      -- Clean up the object when we are done
      Ma.Destroy;
      pragma Unreferenced (Ma);
   end Test_Change_Sample_Length;

   overriding procedure Test_Not_Initialized (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Ma : My_Moving_Avg.Instance;
      Status : My_Moving_Avg.Size_Status;
      Ignore_Max : Short_Float := 0.0;
      Ignore_Mean : Short_Float := 0.0;
      Ignore_Variance : Short_Float := 0.0;
   begin
      -- With no initialization, test reset
      Ma.Reset;

      -- With no initialization, test change_sample_length
      Status := Ma.Change_Sample_Calculation_Size (4);
      Change_Length_Status_Assert.Eq (Status, My_Moving_Avg.Too_Large);

      -- Run mean with no data:
      Ma.Calculate_Mean_Variance_Max (Ignore_Mean, Ignore_Variance, Ignore_Max);

      -- Test an invalid initialization
      --ma.init(Sample_Storage_Size => 6, Sample_Calculation_Size => 10);
      pragma Unreferenced (Ma);
   end Test_Not_Initialized;

end Moving_Average_Tests.Implementation;
