--------------------------------------------------------------------------------
-- Rate_Group Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Rate Group component.
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test excersizes Rate Group component through a nominal scenario.
   overriding procedure Nominal (Self : in out Instance);
   -- This unit test triggers a cycle slip and makes sure that the Rate Group component reports it.
   overriding procedure Cycle_Slip_Trigger (Self : in out Instance);
   -- This unit test makes sure that the max execution and max cycle time are reported correctly.
   overriding procedure Time_Reporting (Self : in out Instance);
   -- This unit test makes sure that when a component has a full queue during scheduling the correct event is thrown.
   overriding procedure Full_Queue (Self : in out Instance);
   -- This unit test makes sure the proper error is thrown when the rate group component's queue gets full.
   overriding procedure Test_Dropped_Tick (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      null;
   end record;
end Tests.Implementation;
