--------------------------------------------------------------------------------
-- Queue_Monitor Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Queue Monitor component.
package Queue_Monitor_Tests.Implementation is
   -- Test data and state:
   type Instance is new Queue_Monitor_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test excersizes checking a component's queue reporting the values in telemetry.
   overriding procedure Test_Queue_Monitoring (Self : in out Instance);
   -- This unit test excersizes the command to change the packet creation rate.
   overriding procedure Test_Packet_Period (Self : in out Instance);
   -- This unit test makes sure an invalid command is reported and ignored.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Queue_Monitor_Tests.Base_Instance with record
      null;
   end record;
end Queue_Monitor_Tests.Implementation;
