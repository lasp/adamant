--------------------------------------------------------------------------------
-- Apid_Tree Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Interfaces; use Interfaces;
with Smart_Assert;
with Apid_Tree; use Apid_Tree;
with Ccsds_Downsampler_Types; use Ccsds_Downsampler_Types;

package body Apid_Tree_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      null;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -- Custom asserts
   package Filter_Action_Status_Assert is new Smart_Assert.Basic (Filter_Action_Status, Filter_Action_Status'Image);
   package Filter_Factor_Set_Status_Assert is new Smart_Assert.Basic (Filter_Factor_Set_Status, Filter_Factor_Set_Status'Image);

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Init_List (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Tree : Apid_Tree.Instance;
      Status : Filter_Action_Status;
      Ignore : Unsigned_16;
      First_Index : Positive;
      Last_Index : Natural;
      Tree_Entry : Ccsds_Downsampler_Tree_Entry;
      Apid_Start_List : aliased Ccsds_Downsample_Packet_List := [(Apid => 2, Filter_Factor => 0), (Apid => 3, Filter_Factor => 3), (Apid => 4, Filter_Factor => 1)];
   begin
      Tree.Init (Apid_Start_List'Unchecked_Access);
      -- "Filter" each Id and make sure we get the correct values and expected filter pairs
      Status := Tree.Filter_Packet (2, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (2, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (2, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);

      Status := Tree.Filter_Packet (4, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (4, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (4, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (4, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (4, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (4, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);

      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Invalid_Id);

      Status := Tree.Filter_Packet (5, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Invalid_Id);

      Status := Tree.Filter_Packet (3, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (3, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (3, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (3, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (3, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (3, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (3, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);

      -- Also make sure that the ranges make sense
      First_Index := Tree.Get_Tree_First_Index;
      Natural_Assert.Eq (First_Index, 1);
      Last_Index := Tree.Get_Tree_Last_Index;
      Natural_Assert.Eq (Last_Index, 3);

      -- And Lastly make sure each element was filled in correctly
      Tree_Entry := Tree.Get_Tree_Entry (1);
      Natural_Assert.Eq (Natural (Tree_Entry.Apid), 2);
      Unsigned_16_Assert.Eq (Tree_Entry.Filter_Factor, 0);
      Unsigned_16_Assert.Eq (Tree_Entry.Filter_Count, 3);

      Tree_Entry := Tree.Get_Tree_Entry (2);
      Natural_Assert.Eq (Natural (Tree_Entry.Apid), 3);
      Unsigned_16_Assert.Eq (Tree_Entry.Filter_Factor, 3);
      Unsigned_16_Assert.Eq (Tree_Entry.Filter_Count, 7);

      Tree_Entry := Tree.Get_Tree_Entry (3);
      Natural_Assert.Eq (Natural (Tree_Entry.Apid), 4);
      Unsigned_16_Assert.Eq (Tree_Entry.Filter_Factor, 1);
      Unsigned_16_Assert.Eq (Tree_Entry.Filter_Count, 6);

   end Test_Init_List;

   overriding procedure Test_Set_Filter_Factor (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Tree : Apid_Tree.Instance;
      Status : Filter_Action_Status;
      Set_Status : Filter_Factor_Set_Status;
      Ignore : Unsigned_16;
      Tree_Index : Positive;
      Apid_Start_List : aliased Ccsds_Downsample_Packet_List := [(Apid => 1, Filter_Factor => 0), (Apid => 3, Filter_Factor => 1), (Apid => 5, Filter_Factor => 7)];
   begin
      Tree.Init (Apid_Start_List'Unchecked_Access);

      -- First make sure the first apid is set to all filtered
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);

      -- Set the filter factor to all pass
      Set_Status := Tree.Set_Filter_Factor (1, 1, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Success);
      Natural_Assert.Eq (Natural (Tree_Index), 1);

      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);

      -- Set to something other than 1 and 0
      Set_Status := Tree.Set_Filter_Factor (1, 5, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Success);
      Natural_Assert.Eq (Natural (Tree_Index), 1);

      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass); -- Counter continues from 0 here
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Pass);

      -- Go back to fully filtered
      Set_Status := Tree.Set_Filter_Factor (1, 0, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Success);
      Natural_Assert.Eq (Natural (Tree_Index), 1);

      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Status := Tree.Filter_Packet (1, Ignore);
      Filter_Action_Status_Assert.Eq (Status, Filter);

      -- Set the filter factor to anything else for a different apid to test the tree index changes
      Set_Status := Tree.Set_Filter_Factor (5, 2, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Success);
      Natural_Assert.Eq (Natural (Tree_Index), 3);

      -- Invalid Id test
      Set_Status := Tree.Set_Filter_Factor (0, 0, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Invalid_Id);
      Natural_Assert.Eq (Natural (Tree_Index), 1);

      Set_Status := Tree.Set_Filter_Factor (2, 1, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Invalid_Id);
      Natural_Assert.Eq (Natural (Tree_Index), 1);

      Set_Status := Tree.Set_Filter_Factor (4, 2, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Invalid_Id);
      Natural_Assert.Eq (Natural (Tree_Index), 1);

      Set_Status := Tree.Set_Filter_Factor (6, 3, Tree_Index);
      Filter_Factor_Set_Status_Assert.Eq (Set_Status, Invalid_Id);
      Natural_Assert.Eq (Natural (Tree_Index), 1);
      pragma Unreferenced (Tree);
   end Test_Set_Filter_Factor;

   overriding procedure Test_Get_Counters (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Tree : Apid_Tree.Instance;
      Status : Filter_Action_Status;
      Cnt : Unsigned_16;
      Apid_Start_List : aliased Ccsds_Downsample_Packet_List := [(Apid => 0, Filter_Factor => 1), (Apid => 1, Filter_Factor => 0), (Apid => 2, Filter_Factor => 3), (Apid => 5, Filter_Factor => 2)];
   begin
      Tree.Init (Apid_Start_List'Unchecked_Access);

      -- First filtered and passed packets
      Status := Tree.Filter_Packet (1, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Unsigned_16_Assert.Eq (Cnt, 1);

      Status := Tree.Filter_Packet (0, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Unsigned_16_Assert.Eq (Cnt, 1);

      -- A few more of different ids to make sure the Cnt works
      Status := Tree.Filter_Packet (5, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Unsigned_16_Assert.Eq (Cnt, 2);

      Status := Tree.Filter_Packet (5, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Unsigned_16_Assert.Eq (Cnt, 2);

      Status := Tree.Filter_Packet (2, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Unsigned_16_Assert.Eq (Cnt, 3);
      Status := Tree.Filter_Packet (2, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Unsigned_16_Assert.Eq (Cnt, 3);
      Status := Tree.Filter_Packet (2, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Unsigned_16_Assert.Eq (Cnt, 4);

      Status := Tree.Filter_Packet (1, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Unsigned_16_Assert.Eq (Cnt, 5);
      Status := Tree.Filter_Packet (1, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Unsigned_16_Assert.Eq (Cnt, 6);
      Status := Tree.Filter_Packet (1, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Filter);
      Unsigned_16_Assert.Eq (Cnt, 7);

      Status := Tree.Filter_Packet (0, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Unsigned_16_Assert.Eq (Cnt, 4);
      Status := Tree.Filter_Packet (0, Cnt);
      Filter_Action_Status_Assert.Eq (Status, Pass);
      Unsigned_16_Assert.Eq (Cnt, 5);
      pragma Unreferenced (Tree);
   end Test_Get_Counters;

end Apid_Tree_Tests.Implementation;
