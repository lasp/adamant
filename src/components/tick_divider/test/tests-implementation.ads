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

   -- This unit test excersizes tick dividing with a variety of divisors, including disabled divisors. It runs the component through enough iterations to excersize the count rollover, and checks to make sure this happens.
   overriding procedure Nominal (Self : in out Instance);
   -- This unit test configures the component with a variety of bad input parameters and checks to make sure exceptions are thrown as expected.
   overriding procedure Bad_Setup (Self : in out Instance);
   -- This unit test makes sure that when a component has a full queue during scheduling the correct event is thrown.
   overriding procedure Full_Queue (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
