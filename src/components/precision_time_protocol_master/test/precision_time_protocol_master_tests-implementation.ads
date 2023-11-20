--------------------------------------------------------------------------------
-- Precision_Time_Protocol_Master Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Precision Time Protocol Master component.
package Precision_Time_Protocol_Master_Tests.Implementation is
   -- Test data and state:
   type Instance is new Precision_Time_Protocol_Master_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test ensures that time syncing messages are sent out appropriately when Ticks are sent to the component.
   overriding procedure Test_Time_Sync (Self : in out Instance);
   -- This test ensures that the Follow_Up message is sent out appropriately.
   overriding procedure Test_Follow_Up (Self : in out Instance);
   -- This test ensures that the Delay_Response message is sent out when a Delay_Request is received.
   overriding procedure Test_Delay_Request (Self : in out Instance);
   -- This test ensures that the unexpected received messages are reported and not processed.
   overriding procedure Test_Unexpected_Message_Received (Self : in out Instance);
   -- This test ensures that the enable/disable commands work as intended.
   overriding procedure Test_Enable_Disable (Self : in out Instance);
   -- This test ensures that the Sync_Once command works.
   overriding procedure Test_Sync_Once (Self : in out Instance);
   -- This test ensures that an invalid command is rejected and reported.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This test ensures that a queue overflow is reported.
   overriding procedure Test_Queue_Overflow (Self : in out Instance);

   -- Test data and state:
   type Instance is new Precision_Time_Protocol_Master_Tests.Base_Instance with record
      null;
   end record;
end Precision_Time_Protocol_Master_Tests.Implementation;
