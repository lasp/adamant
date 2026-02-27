--------------------------------------------------------------------------------
-- Ccsds_Subpacket_Extractor Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Subpacket Extractor.
package Ccsds_Subpacket_Extractor_Tests.Implementation is
   -- Test data and state:
   type Instance is new Ccsds_Subpacket_Extractor_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests the nominal subpacket extraction from a larger CCSDS packet.
   overriding procedure Nominal_Extraction (Self : in out Instance);
   -- This unit test tests that the proper events are sent out when a CCSDS packet with an incorrect size is received by the component.
   overriding procedure Test_Invalid_Length (Self : in out Instance);
   -- This unit test tests that the proper events are sent out when a CCSDS packet with an incorrect size is extracted by the component.
   overriding procedure Test_Invalid_Subpacket_Length (Self : in out Instance);
   -- This unit test tests that the proper events are sent out when a CCSDS packet with left over bytes is encountered.
   overriding procedure Test_Remaining_Bytes (Self : in out Instance);
   -- This unit test tests that the proper events are sent out when a packet is dropped due to an overflowed queue.
   overriding procedure Test_Dropped_Packet (Self : in out Instance);
   -- This unit test tests the component with nonzero start and stop offsets for CCSDS extraction.
   overriding procedure Test_Offsets (Self : in out Instance);
   -- This unit test tests the component with a zero and positive Max_Subpackets_To_Extract configuration and looks for appropriate behavior.
   overriding procedure Test_Max_Subpackets_To_Extract (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Subpacket_Extractor_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Subpacket_Extractor_Tests.Implementation;
