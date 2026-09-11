--------------------------------------------------------------------------------
-- Double_Buffer_Store Tests Body
--------------------------------------------------------------------------------

with Basic_Types; use Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Crc_16;
with Double_Buffer_Store;
with Interfaces; use Interfaces;
with Packed_U32;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Smart_Assert;
with Sys_Time;
with Sys_Time.Assertion; use Sys_Time.Assertion;
with System; use System;
with System.Assertions;

package body Double_Buffer_Store_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Stores under test. The regions stand in for persistent memory and the
   -- instances for the bookkeeping a component would keep.
   -------------------------------------------------------------------------

   package Layout is new Double_Buffer_Store (Packed_U32.T);
   use Layout;
   Region_A : aliased Layout.Persistent_Copy;
   Region_B : aliased Layout.Persistent_Copy;
   Store : Layout.Instance;

   -- A second pair of regions and instance, for the two instance test:
   Other_A : aliased Layout.Persistent_Copy;
   Other_B : aliased Layout.Persistent_Copy;
   Other_Store : Layout.Instance;

   -- Two copies laid over one buffer so that the second starts inside the first, for
   -- the overlap test. They are imported so that nothing initializes the buffer twice.
   Overlap_Buffer : aliased Byte_Array (0 .. 2 * Layout.Persistent_Copy'Max_Size_In_Storage_Elements) := [others => 0];
   Overlap_A : aliased Layout.Persistent_Copy with Import, Address => Overlap_Buffer (0)'Address;
   Overlap_B : aliased Layout.Persistent_Copy with Import, Address => Overlap_Buffer (4)'Address;

   -- A second store over a larger record, Sys_Time.T, which is two unsigned words:
   package Time_Layout is new Double_Buffer_Store (Sys_Time.T);
   Time_Region_A : aliased Time_Layout.Persistent_Copy;
   Time_Region_B : aliased Time_Layout.Persistent_Copy;
   Time_Store : Time_Layout.Instance;

   -------------------------------------------------------------------------
   -- Assertion packages:
   -------------------------------------------------------------------------

   package Restore_Status_Assert is new Smart_Assert.Basic (Restore_Status, Restore_Status'Image);

   -------------------------------------------------------------------------
   -- Test helpers:
   -------------------------------------------------------------------------

   -- Save times, in increasing order:
   Time_1 : constant Sys_Time.T := (Seconds => 1_234, Subseconds => 5_678);
   Time_2 : constant Sys_Time.T := (Seconds => 1_234, Subseconds => 5_679);
   Time_3 : constant Sys_Time.T := (Seconds => 1_235, Subseconds => 0);
   Time_4 : constant Sys_Time.T := (Seconds => 2_000, Subseconds => 0);
   Zero_Time : constant Sys_Time.T := (Seconds => 0, Subseconds => 0);

   -- Copy contents that are not a valid store:
   Garbage : constant Persistent_Copy := (Crc => [16#DE#, 16#AD#], Payload => (Save_Time => (Seconds => 16#BEEF#, Subseconds => 7), Data => (Value => 16#0BAD_F00D#)));
   Zeros : constant Persistent_Copy := (Crc => [0, 0], Payload => (Save_Time => Zero_Time, Data => (Value => 0)));
   Ones : constant Persistent_Copy := (Crc => [16#FF#, 16#FF#], Payload => (Save_Time => (Seconds => 16#FFFF_FFFF#, Subseconds => Sys_Time.Subseconds_Type'Last), Data => (Value => 16#FFFF_FFFF#)));

   -- The CRC the store writes: the CRC-16 over the serialized save time followed by
   -- the serialized data.
   function Expected_Crc (Value : in Unsigned_32; Save_Time : in Sys_Time.T) return Crc_16.Crc_16_Type is
      (Crc_16.Compute_Crc_16 (Sys_Time.Serialization.To_Byte_Array (Save_Time) & Packed_U32.Serialization.To_Byte_Array ((Value => Value))));

   -- The contents a save of Value at Save_Time writes, for loading regions and for
   -- emulating interrupted saves:
   function Encode (Value : in Unsigned_32; Save_Time : in Sys_Time.T) return Persistent_Copy is
      (Crc => Expected_Crc (Value, Save_Time), Payload => (Save_Time => Save_Time, Data => (Value => Value)));

   -- Read a region out of memory for inspection. The result is a non volatile copy,
   -- so it can be compared and passed to assertions.
   type Snapshot_Type is record
      Crc : Crc_16.Crc_16_Type;
      Payload : Payload_Type;
   end record;
   function Snapshot (Region : in Persistent_Copy) return Snapshot_Type is
      (Crc => Region.Crc, Payload => Region.Payload);
   function Snapshot (Contents : in Persistent_Copy; Same_As : in Persistent_Copy) return Boolean is
      (Snapshot (Contents) = Snapshot (Same_As));

   -- Check that Init of the main store returns the expected status and value:
   procedure Check_Init (Expected_Status : in Restore_Status; Expected_Value : in Unsigned_32) is
      Value : Packed_U32.T;
      Status : Restore_Status;
   begin
      Store.Init (Region_A'Access, Region_B'Access, Value, Status);
      Restore_Status_Assert.Eq (Status, Expected_Status);
      Packed_U32_Assert.Eq (Value, (Value => Expected_Value));
   end Check_Init;

   -- Check that Restore of the main store returns the expected status and value:
   procedure Check_Restore (Expected_Status : in Restore_Status; Expected_Value : in Unsigned_32) is
      Value : Packed_U32.T;
      Status : Restore_Status;
   begin
      Store.Restore (Value, Status);
      Restore_Status_Assert.Eq (Status, Expected_Status);
      Packed_U32_Assert.Eq (Value, (Value => Expected_Value));
   end Check_Restore;

   -- Check that a region holds exactly the contents a save would write:
   procedure Check_Region (Region : in Persistent_Copy; Value : in Unsigned_32; Save_Time : in Sys_Time.T) is
      S : constant Snapshot_Type := Snapshot (Region);
   begin
      Sys_Time_Assert.Eq (S.Payload.Save_Time, Save_Time);
      Packed_U32_Assert.Eq (S.Payload.Data, (Value => Value));
      Byte_Array_Assert.Eq (S.Crc, Expected_Crc (Value, Save_Time));
   end Check_Region;

   -- Check that a region holds a save that Init has invalidated: the payload is intact
   -- and the CRC is the complement of the one the save wrote.
   procedure Check_Invalidated (Region : in Persistent_Copy; Value : in Unsigned_32; Save_Time : in Sys_Time.T) is
      S : constant Snapshot_Type := Snapshot (Region);
      Crc : constant Crc_16.Crc_16_Type := Expected_Crc (Value, Save_Time);
   begin
      Sys_Time_Assert.Eq (S.Payload.Save_Time, Save_Time);
      Packed_U32_Assert.Eq (S.Payload.Data, (Value => Value));
      Byte_Array_Assert.Eq (S.Crc, [not Crc (0), not Crc (1)]);
   end Check_Invalidated;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
      Ignore : Instance renames Self;
   begin
      -- Every test starts with regions holding no valid save:
      Region_A := Garbage;
      Region_B := Garbage;
      Other_A := Garbage;
      Other_B := Garbage;
      Time_Region_A := (Crc => [16#DE#, 16#AD#], Payload => (Save_Time => Garbage.Payload.Save_Time, Data => Garbage.Payload.Save_Time));
      Time_Region_B := (Crc => [16#DE#, 16#AD#], Payload => (Save_Time => Garbage.Payload.Save_Time, Data => Garbage.Payload.Save_Time));
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_No_Valid_Copy_On_First_Boot (Self : in out Instance) is
      Ignore : Instance renames Self;
   begin
      -- Garbage in both regions:
      Check_Init (No_Valid_Copy, 0);
      Check_Restore (No_Valid_Copy, 0);
      Boolean_Assert.Eq (Snapshot (Region_A, Same_As => Garbage), True);
      Boolean_Assert.Eq (Snapshot (Region_B, Same_As => Garbage), True);

      -- All zeros in both regions is also not a valid store, since the CRC of zeroed
      -- contents is not zero:
      Region_A := Zeros;
      Region_B := Zeros;
      Check_Restore (No_Valid_Copy, 0);
      Check_Init (No_Valid_Copy, 0);

      -- All ones, the erased state of many nonvolatile memories, is not valid either:
      Region_A := Ones;
      Region_B := Ones;
      Check_Restore (No_Valid_Copy, 0);
      Check_Init (No_Valid_Copy, 0);
      Boolean_Assert.Eq (Snapshot (Region_A, Same_As => Ones), True);
      Boolean_Assert.Eq (Snapshot (Region_B, Same_As => Ones), True);

      -- The first save goes to copy A and leaves copy B alone:
      Store.Save ((Value => 1), Time_1);
      Check_Region (Region_A, 1, Time_1);
      Boolean_Assert.Eq (Snapshot (Region_B, Same_As => Ones), True);
      Check_Restore (Restored, 1);
   end Test_No_Valid_Copy_On_First_Boot;

   overriding procedure Test_Save_Alternates (Self : in out Instance) is
      Ignore : Instance renames Self;
   begin
      Check_Init (No_Valid_Copy, 0);

      -- First save goes to copy A:
      Store.Save ((Value => 1), Time_1);
      Check_Region (Region_A, 1, Time_1);
      Boolean_Assert.Eq (Snapshot (Region_B, Same_As => Garbage), True);
      Check_Restore (Restored, 1);

      -- Second save goes to copy B. Copy A is left intact.
      Store.Save ((Value => 2), Time_2);
      Check_Region (Region_A, 1, Time_1);
      Check_Region (Region_B, 2, Time_2);
      Check_Restore (Restored, 2);

      -- Third save goes back to copy A:
      Store.Save ((Value => 3), Time_3);
      Check_Region (Region_A, 3, Time_3);
      Check_Region (Region_B, 2, Time_2);
      Check_Restore (Restored, 3);

      -- Fourth save goes to copy B:
      Store.Save ((Value => 16#1234_5678#), Time_4);
      Check_Region (Region_A, 3, Time_3);
      Check_Region (Region_B, 16#1234_5678#, Time_4);
      Check_Restore (Restored, 16#1234_5678#);

      -- A reboot finds the newest save in copy B and invalidates A. The next save
      -- rewrites A.
      Check_Init (Restored, 16#1234_5678#);
      Check_Invalidated (Region_A, 3, Time_3);
      Check_Region (Region_B, 16#1234_5678#, Time_4);
      Store.Save ((Value => 5), Time_4);
      Check_Region (Region_A, 5, Time_4);
      Check_Region (Region_B, 16#1234_5678#, Time_4);
   end Test_Save_Alternates;

   overriding procedure Test_Init_Picks_Newest_And_Invalidates_Other (Self : in out Instance) is
      Ignore : Instance renames Self;
   begin
      -- Bind the store first, on regions holding nothing valid, so that Restore can be
      -- checked before each Init below:
      Check_Init (No_Valid_Copy, 0);

      -- Both valid, B newer. Restore returns B and touches nothing. Init returns B,
      -- complements A's CRC and leaves the rest of A, and the next save rewrites A.
      Region_A := Encode (1, Time_1);
      Region_B := Encode (2, Time_2);
      Check_Restore (Restored, 2);
      Check_Region (Region_A, 1, Time_1);
      Check_Region (Region_B, 2, Time_2);
      Check_Init (Restored, 2);
      Check_Invalidated (Region_A, 1, Time_1);
      Check_Region (Region_B, 2, Time_2);
      Check_Restore (Restored, 2);
      Store.Save ((Value => 3), Time_3);
      Check_Region (Region_A, 3, Time_3);
      Check_Region (Region_B, 2, Time_2);

      -- Both valid, A newer:
      Region_A := Encode (2, Time_2);
      Region_B := Encode (1, Time_1);
      Check_Restore (Restored, 2);
      Check_Init (Restored, 2);
      Check_Region (Region_A, 2, Time_2);
      Check_Invalidated (Region_B, 1, Time_1);
      Store.Save ((Value => 3), Time_3);
      Check_Region (Region_A, 2, Time_2);
      Check_Region (Region_B, 3, Time_3);

      -- Equal save times fall back to copy A:
      Region_A := Encode (1, Time_1);
      Region_B := Encode (2, Time_1);
      Check_Restore (Restored, 1);
      Check_Init (Restored, 1);
      Check_Region (Region_A, 1, Time_1);
      Check_Invalidated (Region_B, 2, Time_1);

      -- A copy saved at time zero is invalidated like any other, since the CRC and not
      -- the save time is what changes:
      Region_A := Encode (1, Time_1);
      Region_B := Encode (2, Zero_Time);
      Check_Init (Restored, 1);
      Check_Invalidated (Region_B, 2, Zero_Time);
      Check_Restore (Restored, 1);

      -- Only B valid, even though A carries a later time. A's CRC is complemented and
      -- the rest of it is left alone.
      Region_A := Garbage;
      Region_B := Encode (2, Time_1);
      Check_Restore (Restored, 2);
      Check_Init (Restored, 2);
      Byte_Array_Assert.Eq (Snapshot (Region_A).Crc, [not Snapshot (Garbage).Crc (0), not Snapshot (Garbage).Crc (1)]);
      Sys_Time_Assert.Eq (Snapshot (Region_A).Payload.Save_Time, Snapshot (Garbage).Payload.Save_Time);
      Packed_U32_Assert.Eq (Snapshot (Region_A).Payload.Data, Snapshot (Garbage).Payload.Data);
      Store.Save ((Value => 3), Time_3);
      Check_Region (Region_A, 3, Time_3);
      Check_Region (Region_B, 2, Time_1);

      -- Only A valid:
      Region_A := Encode (1, Time_4);
      Region_B := Garbage;
      Check_Restore (Restored, 1);
      Check_Init (Restored, 1);
      Check_Region (Region_A, 1, Time_4);
      Store.Save ((Value => 3), Time_3);
      Check_Region (Region_A, 1, Time_4);
      Check_Region (Region_B, 3, Time_3);
   end Test_Init_Picks_Newest_And_Invalidates_Other;

   overriding procedure Test_Interrupted_Save (Self : in out Instance) is
      Ignore : Instance renames Self;
      Next : constant Persistent_Copy := Encode (2, Time_2);
   begin
      -- Start from a store holding one save in copy A. The next save goes to copy B
      -- and the reboot can land at each point of that save.
      Region_A := Encode (1, Time_1);

      -- Reboot after the payload reaches copy B but before its CRC. Copy B is not
      -- valid and Init returns copy A.
      Region_B.Payload := Next.Payload;
      Check_Init (Restored, 1);
      Check_Restore (Restored, 1);

      -- Reboot after the CRC reaches copy B. Both are valid and the later save time
      -- wins.
      Region_A := Encode (1, Time_1);
      Region_B := Next;
      Check_Restore (Restored, 2);
      Check_Init (Restored, 2);
      Check_Invalidated (Region_A, 1, Time_1);

      -- Reboot during the very first save, before its CRC. Nothing is valid.
      Region_A := Garbage;
      Region_B := Garbage;
      Region_A.Payload := Next.Payload;
      Check_Restore (No_Valid_Copy, 0);
      Check_Init (No_Valid_Copy, 0);
   end Test_Interrupted_Save;

   overriding procedure Test_Corrupt_Copy (Self : in out Instance) is
      Ignore : Instance renames Self;
   begin
      Region_A := Encode (1, Time_1);
      Region_B := Encode (2, Time_2);
      Check_Init (Restored, 2);
      Check_Region (Region_B, 2, Time_2);

      -- Flip a data bit in the newest copy. Copy A was invalidated by Init, so nothing
      -- is valid.
      Region_B.Payload.Data := (Value => 2 xor 16#0000_0100#);
      Check_Restore (No_Valid_Copy, 0);

      -- Repair copy A's CRC and the restore falls back to it:
      Region_A := Encode (1, Time_1);
      Check_Restore (Restored, 1);

      -- Flip a CRC bit in copy A too. Nothing is valid, and after Init the next save
      -- starts over in copy A.
      Region_A.Crc := [Region_A.Crc (0) xor 16#10#, Region_A.Crc (1)];
      Check_Restore (No_Valid_Copy, 0);
      Check_Init (No_Valid_Copy, 0);
      Store.Save ((Value => 3), Time_3);
      Check_Region (Region_A, 3, Time_3);
      Check_Restore (Restored, 3);

      -- A flipped save time bit invalidates a copy as well:
      Region_A.Payload.Save_Time := (Seconds => Time_3.Seconds xor 16#8000_0000#, Subseconds => Time_3.Subseconds);
      Check_Restore (No_Valid_Copy, 0);
   end Test_Corrupt_Copy;

   overriding procedure Test_Two_Instances (Self : in out Instance) is
      Ignore : Instance renames Self;
      Ignore_Value : Packed_U32.T;
      Value : Packed_U32.T;
      Status : Restore_Status;
   begin
      -- The main store restores from B and so targets A. The other store starts fresh
      -- and targets A too, then each alternates over its own regions.
      Region_A := Encode (1, Time_1);
      Region_B := Encode (2, Time_2);
      Check_Init (Restored, 2);
      Other_Store.Init (Other_A'Access, Other_B'Access, Ignore_Value, Status);
      Restore_Status_Assert.Eq (Status, No_Valid_Copy);

      Store.Save ((Value => 3), Time_3);
      Other_Store.Save ((Value => 10), Time_3);
      Other_Store.Save ((Value => 11), Time_4);
      Store.Save ((Value => 4), Time_4);

      Check_Region (Region_A, 3, Time_3);
      Check_Region (Region_B, 4, Time_4);
      Check_Region (Other_A, 10, Time_3);
      Check_Region (Other_B, 11, Time_4);
      Check_Restore (Restored, 4);
      Other_Store.Restore (Value, Status);
      Restore_Status_Assert.Eq (Status, Restored);
      Packed_U32_Assert.Eq (Value, (Value => 11));
   end Test_Two_Instances;

   overriding procedure Test_Larger_Record (Self : in out Instance) is
      Ignore : Instance renames Self;
      package Time_Status_Assert is new Smart_Assert.Basic (Time_Layout.Restore_Status, Time_Layout.Restore_Status'Image);
      Stored : constant Sys_Time.T := (Seconds => 16#A5A5_5A5A#, Subseconds => 16#0F0F#);
      Value : Sys_Time.T;
      Status : Time_Layout.Restore_Status;
   begin
      -- Nothing valid at first, and the zero value comes back:
      Time_Store.Init (Time_Region_A'Access, Time_Region_B'Access, Value, Status);
      Time_Status_Assert.Eq (Status, Time_Layout.No_Valid_Copy);
      Sys_Time_Assert.Eq (Value, Zero_Time);

      -- Save and Restore round trip every field:
      Time_Store.Save (Stored, Time_1);
      Time_Store.Restore (Value, Status);
      Time_Status_Assert.Eq (Status, Time_Layout.Restored);
      Sys_Time_Assert.Eq (Value, Stored);

      -- The CRC covers all eight data bytes, and flipping one invalidates the copy:
      declare
         Crc : constant Crc_16.Crc_16_Type := Time_Region_A.Crc;
         Data : constant Sys_Time.T := Time_Region_A.Payload.Data;
      begin
         Byte_Array_Assert.Eq (Crc, Crc_16.Compute_Crc_16 (Sys_Time.Serialization.To_Byte_Array (Time_1) & Sys_Time.Serialization.To_Byte_Array (Stored)));
         Sys_Time_Assert.Eq (Data, Stored);
      end;
      Time_Region_A.Payload.Data := (Seconds => Stored.Seconds, Subseconds => Stored.Subseconds xor 1);
      Time_Store.Restore (Value, Status);
      Time_Status_Assert.Eq (Status, Time_Layout.No_Valid_Copy);

      -- Two saves later, Init picks the newer copy A and invalidates copy B, whose
      -- payload stays in place:
      Time_Store.Save (Stored, Time_2);
      Time_Store.Save (Zero_Time, Time_3);
      Time_Store.Init (Time_Region_A'Access, Time_Region_B'Access, Value, Status);
      Time_Status_Assert.Eq (Status, Time_Layout.Restored);
      Sys_Time_Assert.Eq (Value, Zero_Time);
      declare
         Data_B : constant Sys_Time.T := Time_Region_B.Payload.Data;
      begin
         Sys_Time_Assert.Eq (Data_B, Stored);
      end;

      -- A save then lands in copy B, and with copy A corrupted a reboot restores from
      -- copy B alone:
      Time_Store.Save (Stored, Time_4);
      Time_Region_A.Payload.Data := (Seconds => 0, Subseconds => 1);
      Time_Store.Init (Time_Region_A'Access, Time_Region_B'Access, Value, Status);
      Time_Status_Assert.Eq (Status, Time_Layout.Restored);
      Sys_Time_Assert.Eq (Value, Stored);
   end Test_Larger_Record;

   overriding procedure Test_Overlapping_Regions_Rejected (Self : in out Instance) is
      Ignore : Instance renames Self;

      -- True when Init refuses the two regions:
      function Rejects (First : in Persistent_Copy_Access; Second : in Persistent_Copy_Access) return Boolean is
         Rejected_Store : Layout.Instance;
         Value : Packed_U32.T;
         Status : Restore_Status;
      begin
         Rejected_Store.Init (First, Second, Value, Status);
         return False;
      exception
         when System.Assertions.Assert_Failure =>
            return True;
      end Rejects;
   begin
      -- Overlapping copies would corrupt each other on every save, so Init refuses
      -- them, whether they are the same object or only share some bytes:
      Boolean_Assert.Eq (Rejects (Region_A'Access, Region_A'Access), True);
      Boolean_Assert.Eq (Rejects (Overlap_A'Access, Overlap_B'Access), True);
      Boolean_Assert.Eq (Rejects (Overlap_B'Access, Overlap_A'Access), True);
      -- Distinct regions are fine, in either order:
      Boolean_Assert.Eq (Rejects (Region_A'Access, Region_B'Access), False);
      Boolean_Assert.Eq (Rejects (Region_B'Access, Region_A'Access), False);
   end Test_Overlapping_Regions_Rejected;

end Double_Buffer_Store_Tests.Implementation;
