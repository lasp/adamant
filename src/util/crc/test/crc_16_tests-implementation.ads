--------------------------------------------------------------------------------
-- Crc_16 Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the crc 16 algorithm
package Crc_16_Tests.Implementation is
   -- Test data and state:
   type Instance is new Crc_16_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test sends in a bit pattern and validates that the correct CRC is returned.
   overriding procedure Test_Crc (Self : in out Instance);
   -- This test sends in a bit scattered pattern and validates that the correct CRC is returned.
   overriding procedure Test_Crc_Seeded (Self : in out Instance);

   -- Test data and state:
   type Instance is new Crc_16_Tests.Base_Instance with record
      null;
   end record;
end Crc_16_Tests.Implementation;
