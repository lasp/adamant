--------------------------------------------------------------------------------
-- Apid_Tree Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the apid tree object for help with the ccsds_downsampler
package Apid_Tree_Tests.Implementation is
   -- Test data and state:
   type Instance is new Apid_Tree_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test is intended to test the different permutations of the init of the apid and filter factor type array.
   overriding procedure Test_Init_List (Self : in out Instance);
   -- This unit test tests changing the value of the filter factor for a given apid.
   overriding procedure Test_Set_Filter_Factor (Self : in out Instance);
   -- This unit test makes sure the counters for the number of packets that should be passed or filtered are correct.
   overriding procedure Test_Get_Counters (Self : in out Instance);

   -- Test data and state:
   type Instance is new Apid_Tree_Tests.Base_Instance with record
      null;
   end record;
end Apid_Tree_Tests.Implementation;
