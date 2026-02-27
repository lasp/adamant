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

   -- This unit test makes sure that packets sent through the component's queue are forwarded through the serial port.
   overriding procedure Test_Packet_Send (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Serial_Interface_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Serial_Interface_Tests.Implementation;
