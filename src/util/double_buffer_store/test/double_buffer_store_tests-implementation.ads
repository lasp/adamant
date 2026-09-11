--------------------------------------------------------------------------------
-- Double_Buffer_Store Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the double buffer store.
package Double_Buffer_Store_Tests.Implementation is
   -- Test data and state:
   type Instance is new Double_Buffer_Store_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This test fills both regions with garbage, zeros, and ones and checks that Init
   -- and Restore report No_Valid_Copy with a zero value, that Init leaves the regions
   -- alone, and that the first save then goes to copy A.
   overriding procedure Test_No_Valid_Copy_On_First_Boot (Self : in out Instance);
   -- This test performs a sequence of saves and checks that the writes alternate
   -- between copy A and copy B, that the save time is stored, that the CRC covers the
   -- save time and data, that the previous copy is left intact, and that Restore
   -- returns the newest value after each save.
   overriding procedure Test_Save_Alternates (Self : in out Instance);
   -- This test fills both regions with valid saves and checks that Init returns the
   -- one with the later save time, invalidates the other by complementing its CRC, and
   -- targets the invalidated copy for the next save. It also checks the tie and the
   -- single valid copy cases, and that Restore does not modify memory.
   overriding procedure Test_Init_Picks_Newest_And_Invalidates_Other (Self : in out Instance);
   -- This test emulates a reboot at each point during a save and checks that Init
   -- returns the newest value that was completely written.
   overriding procedure Test_Interrupted_Save (Self : in out Instance);
   -- This test corrupts a copy in each of its fields and checks that Restore falls back
   -- to the other copy or reports No_Valid_Copy, and that the next save recovers the
   -- store.
   overriding procedure Test_Corrupt_Copy (Self : in out Instance);
   -- This test binds two instances to two pairs of regions and checks that each keeps
   -- its own regions and target.
   overriding procedure Test_Two_Instances (Self : in out Instance);
   -- This test instantiates the store on a larger packed record and checks that save
   -- and Restore round trip every field, that the CRC covers all of them, and that
   -- Init picks the newer copy, invalidates the other, and falls back to the remaining
   -- valid copy.
   overriding procedure Test_Larger_Record (Self : in out Instance);
   -- This test hands Init the same region for both copies, then two regions that
   -- partly overlap, and checks that both are rejected.
   overriding procedure Test_Overlapping_Regions_Rejected (Self : in out Instance);

   -- Test data and state:
   type Instance is new Double_Buffer_Store_Tests.Base_Instance with record
      null;
   end record;
end Double_Buffer_Store_Tests.Implementation;
