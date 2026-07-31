--------------------------------------------------------------------------------
-- Stopwatch_Reporter Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Stopwatch.Reporter utility
package Stopwatch_Reporter_Tests.Implementation is
   -- Test data and state:
   type Instance is new Stopwatch_Reporter_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests starting and stopping the timer pair, including
   -- supplying the wall clock start and stop times externally.
   overriding procedure Test_Start_Stop (Self : in out Instance);
   -- This unit test tests accumulating measurements into the recent-maximum
   -- and maximum values, including the maximum-updated indications.
   overriding procedure Test_Accumulation (Self : in out Instance);
   -- This unit test tests the contents of the accumulated report and the
   -- last-measurement report.
   overriding procedure Test_Reports (Self : in out Instance);
   -- This unit test tests resetting the recent-maximum values and resetting
   -- all values.
   overriding procedure Test_Reset (Self : in out Instance);

   -- Test data and state:
   type Instance is new Stopwatch_Reporter_Tests.Base_Instance with record
      null;
   end record;
end Stopwatch_Reporter_Tests.Implementation;
