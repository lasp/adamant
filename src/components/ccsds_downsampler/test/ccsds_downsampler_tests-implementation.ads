--------------------------------------------------------------------------------
-- Ccsds_Downsampler Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the CCSDS Downsampler component
package Ccsds_Downsampler_Tests.Implementation is
   -- Test data and state:
   type Instance is new Ccsds_Downsampler_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test is to test the cases when the component will downsample an APID and permutations related to the filter factor.
   overriding procedure Test_Downsample_Packet (Self : in out Instance);
   -- This unit test sends the command to modify a filter factor for both valid ids and invalid ids.
   overriding procedure Test_Modify_Filter_Factor (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Downsampler_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Downsampler_Tests.Implementation;
