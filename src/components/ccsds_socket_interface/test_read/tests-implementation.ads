--------------------------------------------------------------------------------
-- Ccsds_Socket_Interface Tests Spec
--------------------------------------------------------------------------------
with GNAT.Sockets;

-- This is the packet send unit test suite for the Socket Interface Component
package Tests.Implementation is
   -- Test data and state:
   type Instance is new Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test makes sure that packets received through the socket are fowarded through the send connector. This test excersizes the additional internal task of the Socket Interface Component.
   overriding procedure Test_Packet_Receive (Self : in out Instance);

   -- Test data and state:
   type Instance is new Tests.Base_Instance with record
      Port : GNAT.Sockets.Port_Type;
   end record;
end Tests.Implementation;
