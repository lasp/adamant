--------------------------------------------------------------------------------
-- Ccsds_Command_Depacketizer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Command Depacketizer component
package Ccsds_Command_Depacketizer_Tests.Implementation is
   -- Test data and state:
   type Instance is new Ccsds_Command_Depacketizer_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test exercises the nominal behavior of the ccsds command depacketizer.
   overriding procedure Test_Nominal_Depacketization (Self : in out Instance);
   -- This unit test makes sure that packets with invalid checksums are reported and dropped.
   overriding procedure Test_Invalid_Packet_Checksum (Self : in out Instance);
   -- This unit test makes sure that packets with invalid packet types are reported and dropped.
   overriding procedure Test_Invalid_Packet_Type (Self : in out Instance);
   -- This unit test makes sure that packets that are too small to hold a valid command are reported and dropped.
   overriding procedure Test_Packet_Too_Small (Self : in out Instance);
   -- This unit test makes sure that packets that are too large to hold a valid command are reported and dropped.
   overriding procedure Test_Packet_Too_Large (Self : in out Instance);
   -- This unit test makes sure that packets that do not include a secondary header are reported and dropped.
   overriding procedure Test_Packet_Without_Secondary_Header (Self : in out Instance);
   -- This unit test makes use of the function code in the secondary header to denote a different number of pad bytes. It makes sure the component responds appropriately.
   overriding procedure Test_Pad_Bytes (Self : in out Instance);
   -- This unit test tests the reset data products command.
   overriding procedure Test_Reset_Counts (Self : in out Instance);
   -- This unit test makes sure the component handles an invalid command appropriately.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Command_Depacketizer_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Command_Depacketizer_Tests.Implementation;
