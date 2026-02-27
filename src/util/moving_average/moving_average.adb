with Safe_Deallocator;

package body Moving_Average is
   -- Public function bodies:
   procedure Init (
      Self : in out Instance;
      Sample_Storage_Size : in Positive;
      Sample_Calculation_Size : in Integer := -1;
      Calculate_Variance : in Boolean := True;
      Calculate_Max : in Boolean := True
   ) is
   begin
      -- Check if the number of samples to calculate is -1 for an optional parameter. 0 is also an invalid parameter
      if Sample_Calculation_Size <= -1 then
         Self.Items_Length := Sample_Storage_Size;
      else
         -- Assertion to make sure the desired calculation size can't be 0. Must be -1 or a number between 1 and Sample_Storage_Size.
         pragma Assert (Sample_Calculation_Size > 0, "The sample calculation size must be greater than 0");
         pragma Assert (Sample_Calculation_Size <= Sample_Storage_Size, "The sample calculation size must be less than the max sample size");
         Self.Items_Length := Sample_Calculation_Size;
      end if;
      -- Either results needs the array to be defined as the max size so that we can change the length as needed
      Self.Items := new Statistic_Items (Natural'First .. Natural'First + Sample_Storage_Size - 1);
      Self.Calculate_Variance := Calculate_Variance;
      Self.Calculate_Max := Calculate_Max;
   end Init;

   procedure Destroy (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Statistic_Items, Name => Statistic_Items_Access);
   begin
      Free_If_Testing (Self.Items);
   end Destroy;

   procedure Add_New_Sample (Self : in out Instance; New_Sample : in T) is
   begin
      -- Start by adding the sample since we have to do this regardless if we are wrapping yet or not
      Self.Sum := @ + New_Sample;
      if Self.Calculate_Variance then
         Self.Sum_Squared := @ + (New_Sample * New_Sample);
      end if;

      -- Determine if we have a full buffer so that we know if we need to increment the count
      if Self.Count >= Self.Items_Length then
         declare
            Prev_Sample : constant T := Self.Items (Self.Head);
         begin
            Self.Sum := @ - Prev_Sample;
            if Self.Calculate_Variance then
               Self.Sum_Squared := @ - (Prev_Sample * Prev_Sample);
            end if;
         end;
      else
         Self.Count := @ + 1;
      end if;

      -- Determine if we have a new max value to store
      if Self.Calculate_Max then
         if New_Sample > Self.Max_Value then
            Self.Max_Value := New_Sample;
         end if;

         -- If we loop back to the beginning of the samples, then reset the max
         if Self.Head = 0 then
            Self.Max_Value := New_Sample;
         end if;
      end if;

      -- Now update our structures and statistics
      Self.Items (Self.Head) := New_Sample;
      Self.Head := (@ + 1) mod (Self.Items_Length);
   end Add_New_Sample;

   procedure Calculate_Mean_Variance_Max (Self : in out Instance; Mean : out T; Variance : out T; Max : out T) is
   begin
      if Self.Count > 0 then
         -- Calculate the mean and variance and max:
         Mean := Self.Sum / T (Self.Count);
         if Self.Calculate_Variance then
            Variance := ((1.0 / T (Self.Count)) * Self.Sum_Squared) - Mean * Mean;
         else
            Variance := 0.0;
         end if;
         Max := Self.Max_Value;
      else
         Mean := 0.0;
         Variance := 0.0;
         Max := 0.0;
      end if;
   end Calculate_Mean_Variance_Max;

   procedure Calculate_Mean_Variance_Max (Self : in out Instance; New_Sample : in T; Mean : out T; Variance : out T; Max : out T) is
   begin
      -- Start by adding the sample:
      Self.Add_New_Sample (New_Sample);

      -- Now do the calculations:
      Self.Calculate_Mean_Variance_Max (Mean, Variance, Max);
   end Calculate_Mean_Variance_Max;

   function Calculate_Mean (Self : in out Instance) return T is
      Mean : T;
      Ignore_1 : T;
      Ignore_2 : T;
   begin
      Self.Calculate_Mean_Variance_Max (Mean => Mean, Variance => Ignore_1, Max => Ignore_2);
      return Mean;
   end Calculate_Mean;

   function Calculate_Mean (Self : in out Instance; New_Sample : in T) return T is
      Mean : T;
      Ignore_1 : T;
      Ignore_2 : T;
   begin
      Self.Calculate_Mean_Variance_Max (New_Sample => New_Sample, Mean => Mean, Variance => Ignore_1, Max => Ignore_2);
      return Mean;
   end Calculate_Mean;

   procedure Reset (Self : in out Instance) is
   begin
      Self.Head := 0;
      Self.Count := 0;
      Self.Max_Value := 0.0;
      Self.Sum := 0.0;
      Self.Sum_Squared := 0.0;
   end Reset;

   -- Used to change the effective length of the running statistics
   function Change_Sample_Calculation_Size (Self : in out Instance; New_Sample_Length : in Positive) return Size_Status is
   begin
      -- Check that the requested length is not longer than the max length
      if Self.Items = null then
         return Too_Large;
      elsif New_Sample_Length > Self.Items'Length then
         return Too_Large;
      end if;

      -- Otherwise update the length and reset the statistics
      Self.Items_Length := New_Sample_Length;
      Reset (Self);

      return Success;
   end Change_Sample_Calculation_Size;

end Moving_Average;
