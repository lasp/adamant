--------------------------------------------------------------------------------
-- Tick_Divider Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Tick Divider component.
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises tick dividing with a variety of divisors, including disabled divisors. It runs the component through enough iterations to exercise the count rollover, and checks to make sure this happens.
   overriding procedure Nominal (Self : in out Instance);
   -- This unit test configures the component with a variety of bad input parameters and checks to make sure exceptions are thrown as expected.
   overriding procedure Bad_Setup (Self : in out Instance);
   -- This unit test makes sure that when a component has a full queue during scheduling the correct event is thrown.
   overriding procedure Full_Queue (Self : in out Instance);
   -- This unit test exercises the new Tick_Counter mode where the component uses the incoming tick's Count field instead of an internal counter for division logic.
   overriding procedure Tick_Counter_Mode (Self : in out Instance);
   -- This unit test exercises edge cases with Unsigned_32 boundary values including wraparound from max value to zero to ensure proper handling of tick count overflow scenarios.
   overriding procedure Boundary_Tick_Counts (Self : in out Instance);
   -- This unit test validates edge cases with special divider configurations including all-disabled connectors, single divisor of 1, and large divisors with small tick counts.
   overriding procedure Edge_Case_Dividers (Self : in out Instance);
   -- This unit test compares Internal vs Tick_Counter modes with identical tick sequences to verify consistent behavior and validate that the modes produce expected differences in output patterns.
   overriding procedure Mode_Comparison (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
