--------------------------------------------------------------------------------
-- Ccsds_Packetizer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Packetizer component
package Ccsds_Packetizer_Tests.Implementation is

   -- Test data and state:
   type Instance is new Ccsds_Packetizer_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises the nominal behavior of the CCSDS Packetizer, checking
   -- all emitted CCSDS packets for correctness.
   overriding procedure Test_Nominal_Packetization (Self : in out Instance);
   -- This unit test exercises the packetization of a maximum sized Adamant packet
   -- into a CCSDS packet, which should succeed without issue.
   overriding procedure Test_Max_Size_Packetization (Self : in out Instance);
   -- This unit test exercises the packetization of a minimum sized Adamant packet
   -- into a CCSDS packet, which should succeed without issue.
   overriding procedure Test_Min_Size_Packetization (Self : in out Instance);
   -- This unit test exercises packetization with the CRC disabled and no trailing
   -- bytes, checking that each emitted CCSDS packet ends with the Adamant packet
   -- data and its length field counts no CRC.
   overriding procedure Test_Crc_Disabled (Self : in out Instance);
   -- This unit test exercises packetization with the CRC disabled and trailing bytes
   -- configured, checking that each emitted CCSDS packet ends with the configured
   -- number of zero bytes inside its packet length, at minimum and maximum Adamant
   -- packet sizes.
   overriding procedure Test_Trailing_Bytes (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Packetizer_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Packetizer_Tests.Implementation;
