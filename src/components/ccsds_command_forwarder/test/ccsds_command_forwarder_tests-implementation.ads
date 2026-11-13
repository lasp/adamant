--------------------------------------------------------------------------------
-- Ccsds_Command_Forwarder Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Command Forwarder component.
package Ccsds_Command_Forwarder_Tests.Implementation is

   -- Test data and state:
   type Instance is new Ccsds_Command_Forwarder_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests that CCSDS packets received by command are forwarded out
   -- of the CCSDS space packet send connector with the expected contents, and that
   -- the appropriate event and data product are produced.
   overriding procedure Test_Nominal_Forwarding (Self : in out Instance);
   -- This unit test tests that a CCSDS packet with the maximum sized data field that
   -- fits within a command argument is forwarded correctly.
   overriding procedure Test_Max_Size_Packet (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Command_Forwarder_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Command_Forwarder_Tests.Implementation;
