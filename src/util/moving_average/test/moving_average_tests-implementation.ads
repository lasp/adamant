--------------------------------------------------------------------------------
-- Moving_Average Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the moving average utility
package Moving_Average_Tests.Implementation is
   -- Test data and state:
   type Instance is new Moving_Average_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the mathematical values that are calculated from a known set of samples
   overriding procedure Test_Statistic_Values (Self : in out Instance);
   -- This unit test tests resetting the appropriate values in the statistics to give a new start to the moving average
   overriding procedure Test_Reset_Statistics (Self : in out Instance);
   -- This unit test changes the number of samples the moving average is calculated over.
   overriding procedure Test_Change_Sample_Length (Self : in out Instance);
   -- This unit test makes sure that the call to Change_Sample_Length and Reset are safe if not initialized.
   overriding procedure Test_Not_Initialized (Self : in out Instance);

   -- Test data and state:
   type Instance is new Moving_Average_Tests.Base_Instance with record
      null;
   end record;
end Moving_Average_Tests.Implementation;
