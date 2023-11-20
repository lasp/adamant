--------------------------------------------------------------------------------
-- Time_Of_Tone_Master Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Time of Tone Master component.
package Time_Of_Tone_Master_Tests.Implementation is
   -- Test data and state:
   type Instance is new Time_Of_Tone_Master_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test ensures that tone messages are sent out appropriately when Ticks are sent to the component.
   overriding procedure Test_Tone_Message (Self : in out Instance);
   -- This test ensures that time messages are sent out appropriately when tone timestamps are sent to the component.
   overriding procedure Test_Time_Message (Self : in out Instance);
   -- This test ensures that enable and disable commands work.
   overriding procedure Test_Enable_Disabled (Self : in out Instance);
   -- This test ensures that the Sync_Once command works.
   overriding procedure Test_Sync_Once (Self : in out Instance);
   -- This test ensures that an invalid command is rejected and reported.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Time_Of_Tone_Master_Tests.Base_Instance with record
      null;
   end record;
end Time_Of_Tone_Master_Tests.Implementation;
