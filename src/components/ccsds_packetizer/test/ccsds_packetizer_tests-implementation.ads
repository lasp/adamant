--------------------------------------------------------------------------------
-- Ccsds_Packetizer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Packetizer component
package Ccsds_Packetizer_Tests.Implementation is
   -- Test data and state:
   type Instance is new Ccsds_Packetizer_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test excersizes the nominal behavior of the ccsds packetizer.
   overriding procedure Test_Nominal_Packetization (Self : in out Instance);
   -- This unit test excersizes the packetization of a maximum sized adamant packet into a ccsds packet.
   overriding procedure Test_Max_Size_Packetization (Self : in out Instance);
   -- This unit test excersizes the packetization of a minimum sized Adamant packet into a CCSDS packet, which should succeed without issue.
   overriding procedure Test_Min_Size_Packetization (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Packetizer_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Packetizer_Tests.Implementation;
