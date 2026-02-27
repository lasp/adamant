--------------------------------------------------------------------------------
-- Monitor Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the monitor utility
package Monitor_Tests.Implementation is
   -- Test data and state:
   type Instance is new Monitor_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test makes sure monitor transitions from green to red correctly.
   overriding procedure Test_Monitor_Green_To_Red (Self : in out Instance);
   -- This test makes sure monitor transitions from red to green correctly.
   overriding procedure Test_Monitor_Red_To_Green (Self : in out Instance);
   -- This test makes sure monitor enable and disable function work correctly.
   overriding procedure Test_Monitor_Enable_Disable (Self : in out Instance);

   -- Test data and state:
   type Instance is new Monitor_Tests.Base_Instance with record
      null;
   end record;
end Monitor_Tests.Implementation;
