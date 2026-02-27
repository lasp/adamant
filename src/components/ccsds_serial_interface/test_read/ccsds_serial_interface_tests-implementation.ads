--------------------------------------------------------------------------------
-- Ccsds_Serial_Interface Tests Spec
--------------------------------------------------------------------------------

-- This is the packet send unit test suite for the Serial Interface Component
package Ccsds_Serial_Interface_Tests.Implementation is
   -- Test data and state:
   type Instance is new Ccsds_Serial_Interface_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure that packets received through the serial port are forwarded through the send connector. This test exercises the additional internal task of the Serial Interface Component.
   overriding procedure Test_Packet_Receive (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Serial_Interface_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Serial_Interface_Tests.Implementation;
