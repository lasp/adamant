-- This is a generic, unprotected statistics data structure.
-- The user can instantiate this class with any type that they choose.
generic
   -- The type of data stored on the statistic array.
   type T is digits <>;
package Moving_Average is

   type Instance is tagged private;

   type Size_Status is (Success, Too_Large);

   --
   -- Initialization/destruction functions:
   --
   -- Sample_Storage_Size: Max size that will be allocated for the array that holds the samples. This value is a positive so it must be 1 or greater
   -- Sample_Calculation_Size: Size of the effective array that is used to calculate samples.
   --    Used to change the sample duration that the statistics are calculated over. Is an optional parameter where it will default to the max if negative.
   --    Cannot be 0 as well.
   -- Calculate_Variance: Calculate the variance as well as the mean. This adds a bit of computation time and can be turned off if needed.
   -- Calculate_Max: Calculate the max as well as the mean during the last sample period. This adds a bit of computation time and can be turned off if needed.
   --
   procedure Init (
      Self : in out Instance;
      Sample_Storage_Size : in Positive;
      Sample_Calculation_Size : in Integer := -1;
      Calculate_Variance : in Boolean := True;
      Calculate_Max : in Boolean := True
   );
   procedure Destroy (Self : in out Instance);

   -- Provide a new sample without calculating the mean, variance, and max. This saves some execution time if you just want to
   -- store some samples without calculating the results yet.
   procedure Add_New_Sample (Self : in out Instance; New_Sample : in T);

   -- Procedure to calculate the mean, variance, and max of the last Sample_Calculation_Size samples. There are two procedures, one
   -- where the caller can provide a new sample to include in the calculation, and one where a new sample is not provided.
   procedure Calculate_Mean_Variance_Max (Self : in out Instance; New_Sample : in T; Mean : out T; Variance : out T; Max : out T);
   procedure Calculate_Mean_Variance_Max (Self : in out Instance; Mean : out T; Variance : out T; Max : out T);

   -- Calculate mean without calculating variance and max. If you plan on only using these procedures and not Calculate_Mean_Variance_Max, you
   -- should set Calculate_Variance and Calculate_Max to False in Init.
   function Calculate_Mean (Self : in out Instance; New_Sample : in T) return T;
   function Calculate_Mean (Self : in out Instance) return T;

   -- Reset the statistics
   procedure Reset (Self : in out Instance);

   -- Resets and changes the length of the effective length of calculating the statistics
   function Change_Sample_Calculation_Size (Self : in out Instance; New_Sample_Length : in Positive) return Size_Status;

private
   type Statistic_Items is array (Natural range <>) of T;
   type Statistic_Items_Access is access Statistic_Items;

   type Instance is tagged record
      -- Internal state:
      Head : Natural := Natural'First;
      Count : Natural := Natural'First;
      Sum : T := 0.0;
      Sum_Squared : T := 0.0;
      Max_Value : T := 0.0;
      Items : Statistic_Items_Access;
      Items_Length : Positive := Positive'First;
      -- Booleans to control what is getting calculated:
      Calculate_Variance : Boolean := True;
      Calculate_Max : Boolean := True;
   end record;

end Moving_Average;
