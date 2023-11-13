--------------------------------------------------------------------------------
-- Pid_Controller Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Pid Controller component
package Pid_Controller_Tests.Implementation is
   -- Test data and state:
   type Instance is new Pid_Controller_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises starting the diagnostic packet after being commanded.
   overriding procedure Test_Diagnostic_Packet (Self : in out Instance);
   -- This unit test exercises updating the Data products appropriately.
   overriding procedure Test_Update_Data_Products (Self : in out Instance);
   -- This unit test exercises the data product update period command.
   overriding procedure Test_Database_Update_Period (Self : in out Instance);
   -- This test is a basic test to make sure that the controller
   overriding procedure Test_Pid_Controller (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test exercises that an invalid parameter throws the appropriate event.
   overriding procedure Test_Invalid_Parameter (Self : in out Instance);
   -- This unit test exercises updating a the diagnostic samples by command.
   overriding procedure Test_Start_Diagnostics_Command (Self : in out Instance);
   -- This unit test exercises updating the length of the array used to calculate statistics and thus the duration of the statistic period.
   overriding procedure Test_Set_Controller_Statistic_Duration_Command (Self : in out Instance);
   -- This test makes sure that if the moving_average object is unused, that no statistics come out and nothing breaks.
   overriding procedure Test_Moving_Average_Unused (Self : in out Instance);

   -- Test data and state:
   type Instance is new Pid_Controller_Tests.Base_Instance with record
      null;
   end record;
end Pid_Controller_Tests.Implementation;
