--------------------------------------------------------------------------------
-- Tick_Listener Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Tick Listener component. It tests the component in a Linux environment, responding to ticks.
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test sends many ticks to the Tick Listener component and expects it to count them correctly.
   overriding procedure Test_Tick_Handling (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
