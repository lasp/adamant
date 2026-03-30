--------------------------------------------------------------------------------
-- Parameter_Table_Buffer Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;
with Parameter_Table_Buffer; use Parameter_Table_Buffer;
with Ccsds_Enums; use Ccsds_Enums.Ccsds_Sequence_Flag;
with Basic_Types;
with Memory_Region;
with Parameter_Types;
with System; use System;

package body Parameter_Table_Buffer_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Assertion packages:
   -------------------------------------------------------------------------
   package Append_Status_Assert is new Smart_Assert.Basic (Append_Status, Append_Status'Image);
   package Table_Id_Assert is new Smart_Assert.Basic (Parameter_Types.Parameter_Table_Id, Parameter_Types.Parameter_Table_Id'Image);

   -- Default buffer size used by most tests:
   Default_Buffer_Size : constant Positive := 64;

   -- Helper: Check table region has expected length and nonzero address.
   procedure Check_Table_Region (Buf : in Parameter_Table_Buffer.Instance; Expected_Length : in Natural) is
      Region : constant Memory_Region.T := Buf.Get_Table_Region;
   begin
      Natural_Assert.Eq (Region.Length, Expected_Length);
      pragma Assert (Region.Address /= Null_Address);
   end Check_Table_Region;

   -- Helper: Check full buffer region has expected length and nonzero address.
   procedure Check_Full_Buffer_Region (Buf : in Parameter_Table_Buffer.Instance; Expected_Length : in Natural) is
      Region : constant Memory_Region.T := Buf.Get_Full_Buffer_Region;
   begin
      Natural_Assert.Eq (Region.Length, Expected_Length);
      pragma Assert (Region.Address /= Null_Address);
   end Check_Full_Buffer_Region;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------
   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      Self.Buf.Create (Buffer_Size => Default_Buffer_Size);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      Self.Buf.Destroy;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Create_Destroy (Self : in out Instance) is
   begin
      -- Buffer was created in fixture. Verify initial state:
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 0);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 0);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 0);
      Check_Table_Region (Self.Buf, 0);
      Check_Full_Buffer_Region (Self.Buf, Default_Buffer_Size);

      -- Destroy and recreate to test lifecycle:
      Self.Buf.Destroy;
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 0);
      Self.Buf.Create (Buffer_Size => 32);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 0);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 0);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 0);
      Check_Table_Region (Self.Buf, 0);
      Check_Full_Buffer_Region (Self.Buf, 32);
   end Test_Create_Destroy;

   overriding procedure Test_Unsegmented_Complete_Table (Self : in out Instance) is
      Status : Append_Status;
      -- Table ID = 0x000A, payload = [0x11, 0x22, 0x33]:
      Data : constant Basic_Types.Byte_Array := [16#00#, 16#0A#, 16#11#, 16#22#, 16#33#];
   begin
      Status := Self.Buf.Append (Data => Data, Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 10);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 3);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);
      Check_Table_Region (Self.Buf, 3);

      -- Verify data contents:
      declare
         Region : constant Memory_Region.T := Self.Buf.Get_Table_Region;
         Result : Basic_Types.Byte_Array (0 .. 2);
         for Result'Address use Region.Address;
         pragma Import (Ada, Result);
      begin
         Byte_Assert.Eq (Result (0), 16#11#);
         Byte_Assert.Eq (Result (1), 16#22#);
         Byte_Assert.Eq (Result (2), 16#33#);
      end;

      -- Unsegmented with only Table ID (no payload) should also work:
      Status := Self.Buf.Append (Data => [16#00#, 16#FF#], Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 16#00FF#);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 0);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);
   end Test_Unsegmented_Complete_Table;

   overriding procedure Test_Unsegmented_Too_Small (Self : in out Instance) is
      Status : Append_Status;
   begin
      -- Empty data:
      Status := Self.Buf.Append (Data => [1 .. 0 => 0], Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Too_Small_Table);

      -- 1 byte:
      Status := Self.Buf.Append (Data => [16#01#], Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Too_Small_Table);
   end Test_Unsegmented_Too_Small;

   overriding procedure Test_Unsegmented_During_Receive (Self : in out Instance) is
      Status : Append_Status;
   begin
      -- Start a segmented table (ID = 1):
      Status := Self.Buf.Append (Data => [16#00#, 16#01#, 16#AA#, 16#BB#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 1);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 2);

      -- Unsegmented packet arrives (ID = 2) — replaces in-progress table:
      Status := Self.Buf.Append (Data => [16#00#, 16#02#, 16#CC#, 16#DD#], Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 2);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 2);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);

      -- Continuation should be ignored since we're back to Idle:
      Status := Self.Buf.Append (Data => [16#EE#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Packet_Ignored);
   end Test_Unsegmented_During_Receive;

   overriding procedure Test_Nominal_Segmented_Flow (Self : in out Instance) is
      Status : Append_Status;
      -- Table ID = 0x0005 followed by 4 bytes of payload:
      First_Data : constant Basic_Types.Byte_Array := [16#00#, 16#05#, 16#11#, 16#22#, 16#33#, 16#44#];
      Cont_Data : constant Basic_Types.Byte_Array := [16#55#, 16#66#];
      Last_Data : constant Basic_Types.Byte_Array := [16#77#, 16#88#];
   begin
      -- FirstSegment:
      Status := Self.Buf.Append (Data => First_Data, Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 5);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 4);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);

      -- ContinuationSegment:
      Status := Self.Buf.Append (Data => Cont_Data, Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffering_Table);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 6);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 2);

      -- LastSegment:
      Status := Self.Buf.Append (Data => Last_Data, Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 8);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 3);

      -- Verify table region:
      Check_Table_Region (Self.Buf, 8);
      Check_Full_Buffer_Region (Self.Buf, Default_Buffer_Size);
   end Test_Nominal_Segmented_Flow;

   overriding procedure Test_First_Segment_Extracts_Table_Id (Self : in out Instance) is
      Status : Append_Status;
   begin
      -- Table ID = 0x1234 (big-endian: 0x12, 0x34):
      Status := Self.Buf.Append (Data => [16#12#, 16#34#, 16#AA#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 16#1234#);

      -- Table ID = 0x0001:
      Status := Self.Buf.Append (Data => [16#00#, 16#01#, 16#BB#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 1);

      -- Table ID = 0xFF00:
      Status := Self.Buf.Append (Data => [16#FF#, 16#00#, 16#CC#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 16#FF00#);
   end Test_First_Segment_Extracts_Table_Id;

   overriding procedure Test_First_Segment_Too_Small (Self : in out Instance) is
      Status : Append_Status;
   begin
      -- Empty data:
      Status := Self.Buf.Append (Data => [1 .. 0 => 0], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, Too_Small_Table);

      -- 1 byte (still too small for 2-byte Table ID):
      Status := Self.Buf.Append (Data => [16#01#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, Too_Small_Table);

      -- Verify we're back to Idle (continuation should be ignored):
      Status := Self.Buf.Append (Data => [16#AA#, 16#BB#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Packet_Ignored);
   end Test_First_Segment_Too_Small;

   overriding procedure Test_Continuation_Without_First (Self : in out Instance) is
      Status : Append_Status;
      Data : constant Basic_Types.Byte_Array := [16#AA#, 16#BB#, 16#CC#];
   begin
      -- ContinuationSegment from Idle:
      Status := Self.Buf.Append (Data => Data, Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Packet_Ignored);

      -- LastSegment from Idle:
      Status := Self.Buf.Append (Data => Data, Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Packet_Ignored);
   end Test_Continuation_Without_First;

   overriding procedure Test_First_Segment_Resets_Buffer (Self : in out Instance) is
      Status : Append_Status;
   begin
      -- Start first table (ID = 1):
      Status := Self.Buf.Append (Data => [16#00#, 16#01#, 16#AA#, 16#BB#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 1);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 2);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);

      -- Add continuation:
      Status := Self.Buf.Append (Data => [16#CC#, 16#DD#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffering_Table);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 4);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 2);

      -- New FirstSegment interrupts (ID = 2):
      Status := Self.Buf.Append (Data => [16#00#, 16#02#, 16#EE#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 2);
      -- Buffer should only contain data from the new table:
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 1);
      -- Packet count reset for new table:
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);
   end Test_First_Segment_Resets_Buffer;

   overriding procedure Test_Buffer_Overflow_First_Segment (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Status : Append_Status;
      -- Use a small buffer for overflow testing:
      Small_Buf : Parameter_Table_Buffer.Instance;
      -- Buffer is 4 bytes, FirstSegment has 2-byte ID + 5 bytes payload = 7 total.
      -- Payload (5 bytes) exceeds buffer capacity (4 bytes):
      Big_Data : constant Basic_Types.Byte_Array := [16#00#, 16#01#, 16#AA#, 16#BB#, 16#CC#, 16#DD#, 16#EE#];
   begin
      Small_Buf.Create (Buffer_Size => 4);

      Status := Small_Buf.Append (Data => Big_Data, Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      -- Should be back to Idle:
      Status := Small_Buf.Append (Data => [16#AA#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Packet_Ignored);

      Small_Buf.Destroy;
      pragma Unreferenced (Small_Buf);
   end Test_Buffer_Overflow_First_Segment;

   overriding procedure Test_Buffer_Overflow_Continuation (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Status : Append_Status;
      Small_Buf : Parameter_Table_Buffer.Instance;
   begin
      Small_Buf.Create (Buffer_Size => 4);

      -- FirstSegment with ID + 2 bytes payload (2 bytes stored in buffer):
      Status := Small_Buf.Append (Data => [16#00#, 16#01#, 16#AA#, 16#BB#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Natural_Assert.Eq (Small_Buf.Get_Table_Length, 2);

      -- Continuation that would overflow (2 + 3 = 5 > 4):
      Status := Small_Buf.Append (Data => [16#CC#, 16#DD#, 16#EE#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      -- Buffer stays in Receiving_Table. A packet that still exceeds capacity also overflows:
      Status := Small_Buf.Append (Data => [16#FF#, 16#EE#, 16#DD#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      -- A new FirstSegment recovers:
      Status := Small_Buf.Append (Data => [16#00#, 16#02#, 16#11#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Small_Buf.Get_Table_Id, 2);

      Small_Buf.Destroy;
      pragma Unreferenced (Small_Buf);
   end Test_Buffer_Overflow_Continuation;

   overriding procedure Test_Buffer_Overflow_Last_Segment (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Status : Append_Status;
      Small_Buf : Parameter_Table_Buffer.Instance;
   begin
      Small_Buf.Create (Buffer_Size => 4);

      -- FirstSegment with ID + 2 bytes payload:
      Status := Small_Buf.Append (Data => [16#00#, 16#01#, 16#AA#, 16#BB#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);

      -- LastSegment that would overflow:
      Status := Small_Buf.Append (Data => [16#CC#, 16#DD#, 16#EE#], Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      -- Buffer should still be in Receiving_Table (not Idle), since overflow doesn't transition.
      -- A continuation that still exceeds capacity should return Buffer_Overflow (not Packet_Ignored):
      Status := Small_Buf.Append (Data => [16#FF#, 16#EE#, 16#DD#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      Small_Buf.Destroy;
      pragma Unreferenced (Small_Buf);
   end Test_Buffer_Overflow_Last_Segment;

   overriding procedure Test_Get_Table_Region (Self : in out Instance) is
      Status : Append_Status;
      -- Table ID = 0x0003, payload = [0x11, 0x22, 0x33, 0x44]:
      First_Data : constant Basic_Types.Byte_Array := [16#00#, 16#03#, 16#11#, 16#22#];
      Last_Data : constant Basic_Types.Byte_Array := [16#33#, 16#44#];
   begin
      Status := Self.Buf.Append (Data => First_Data, Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Status := Self.Buf.Append (Data => Last_Data, Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Complete_Table);

      -- Verify table region:
      declare
         Region : constant Memory_Region.T := Self.Buf.Get_Table_Region;
         Full_Region : constant Memory_Region.T := Self.Buf.Get_Full_Buffer_Region;
         -- Read back the bytes from the region address:
         Result : Basic_Types.Byte_Array (0 .. 3);
         for Result'Address use Region.Address;
         pragma Import (Ada, Result);
      begin
         -- Table region length should be 4 (payload only, no Table ID):
         Natural_Assert.Eq (Region.Length, 4);
         pragma Assert (Region.Address /= Null_Address);
         -- Full buffer region should be the full buffer size:
         Natural_Assert.Eq (Full_Region.Length, Default_Buffer_Size);
         pragma Assert (Full_Region.Address /= Null_Address);
         -- Both regions should start at the same address:
         pragma Assert (Region.Address = Full_Region.Address);
         -- Verify data contents:
         Byte_Assert.Eq (Result (0), 16#11#);
         Byte_Assert.Eq (Result (1), 16#22#);
         Byte_Assert.Eq (Result (2), 16#33#);
         Byte_Assert.Eq (Result (3), 16#44#);
      end;

      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 2);
   end Test_Get_Table_Region;

   overriding procedure Test_Multiple_Tables (Self : in out Instance) is
      Status : Append_Status;
   begin
      -- First table (ID = 10):
      Status := Self.Buf.Append (Data => [16#00#, 16#0A#, 16#AA#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Status := Self.Buf.Append (Data => [16#BB#], Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 10);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 2);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 2);

      -- Second table (ID = 20) — packet count resets:
      Status := Self.Buf.Append (Data => [16#00#, 16#14#, 16#CC#, 16#DD#, 16#EE#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);
      Status := Self.Buf.Append (Data => [16#FF#], Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 20);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 4);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 2);
   end Test_Multiple_Tables;

   overriding procedure Test_First_Segment_Only_Table_Id (Self : in out Instance) is
      Status : Append_Status;
   begin
      -- FirstSegment with exactly 2 bytes (Table ID only, no payload):
      Status := Self.Buf.Append (Data => [16#00#, 16#07#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Self.Buf.Get_Table_Id, 7);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 0);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 1);

      -- Add continuation with some data:
      Status := Self.Buf.Append (Data => [16#AA#, 16#BB#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffering_Table);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 2);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 2);

      -- Complete with a LastSegment:
      Status := Self.Buf.Append (Data => [16#11#, 16#22#], Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 4);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 3);

      Check_Table_Region (Self.Buf, 4);
      Check_Full_Buffer_Region (Self.Buf, Default_Buffer_Size);
   end Test_First_Segment_Only_Table_Id;

   overriding procedure Test_Buffer_Overflow_Unsegmented (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Status : Append_Status;
      Small_Buf : Parameter_Table_Buffer.Instance;
      -- Buffer is 4 bytes, payload (5 bytes) exceeds capacity:
      Big_Data : constant Basic_Types.Byte_Array := [16#00#, 16#01#, 16#AA#, 16#BB#, 16#CC#, 16#DD#, 16#EE#];
   begin
      Small_Buf.Create (Buffer_Size => 4);

      Status := Small_Buf.Append (Data => Big_Data, Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      -- Should be in Idle after overflow on Unsegmented:
      Status := Small_Buf.Append (Data => [16#AA#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Packet_Ignored);

      -- A valid Unsegmented should still work after overflow:
      Status := Small_Buf.Append (Data => [16#00#, 16#02#, 16#11#], Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Complete_Table);
      Table_Id_Assert.Eq (Small_Buf.Get_Table_Id, 2);

      Small_Buf.Destroy;
      pragma Unreferenced (Small_Buf);
   end Test_Buffer_Overflow_Unsegmented;

   overriding procedure Test_Data_Integrity_Multi_Segment (Self : in out Instance) is
      Status : Append_Status;
      -- Table ID = 0x0001, followed by payload split across 4 segments:
      First_Data : constant Basic_Types.Byte_Array := [16#00#, 16#01#, 16#AA#, 16#BB#];
      Cont1_Data : constant Basic_Types.Byte_Array := [16#CC#, 16#DD#];
      Cont2_Data : constant Basic_Types.Byte_Array := [16#EE#];
      Last_Data : constant Basic_Types.Byte_Array := [16#FF#, 16#11#, 16#22#];
   begin
      Status := Self.Buf.Append (Data => First_Data, Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Status := Self.Buf.Append (Data => Cont1_Data, Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffering_Table);
      Status := Self.Buf.Append (Data => Cont2_Data, Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffering_Table);
      Status := Self.Buf.Append (Data => Last_Data, Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Complete_Table);

      -- Total payload: AA BB CC DD EE FF 11 22 = 8 bytes
      Natural_Assert.Eq (Self.Buf.Get_Table_Length, 8);
      Natural_Assert.Eq (Self.Buf.Get_Packet_Count, 4);

      -- Verify every byte:
      declare
         Region : constant Memory_Region.T := Self.Buf.Get_Table_Region;
         Result : Basic_Types.Byte_Array (0 .. 7);
         for Result'Address use Region.Address;
         pragma Import (Ada, Result);
      begin
         Natural_Assert.Eq (Region.Length, 8);
         Byte_Assert.Eq (Result (0), 16#AA#);
         Byte_Assert.Eq (Result (1), 16#BB#);
         Byte_Assert.Eq (Result (2), 16#CC#);
         Byte_Assert.Eq (Result (3), 16#DD#);
         Byte_Assert.Eq (Result (4), 16#EE#);
         Byte_Assert.Eq (Result (5), 16#FF#);
         Byte_Assert.Eq (Result (6), 16#11#);
         Byte_Assert.Eq (Result (7), 16#22#);
      end;
   end Test_Data_Integrity_Multi_Segment;

   overriding procedure Test_State_After_Errors (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Status : Append_Status;
      Small_Buf : Parameter_Table_Buffer.Instance;
   begin
      Small_Buf.Create (Buffer_Size => 4);

      -- Too_Small_Table on FirstSegment:
      Status := Small_Buf.Append (Data => [16#01#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, Too_Small_Table);
      -- Table ID should be unchanged (still default 0):
      Table_Id_Assert.Eq (Small_Buf.Get_Table_Id, 0);
      Natural_Assert.Eq (Small_Buf.Get_Table_Length, 0);
      Natural_Assert.Eq (Small_Buf.Get_Packet_Count, 0);

      -- Too_Small_Table on Unsegmented:
      Status := Small_Buf.Append (Data => [1 .. 0 => 0], Sequence_Flag => Unsegmented);
      Append_Status_Assert.Eq (Status, Too_Small_Table);
      Table_Id_Assert.Eq (Small_Buf.Get_Table_Id, 0);

      -- Buffer_Overflow on FirstSegment — Table ID IS extracted before overflow:
      Status := Small_Buf.Append (Data => [16#00#, 16#05#, 16#AA#, 16#BB#, 16#CC#, 16#DD#, 16#EE#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);
      -- Table ID was extracted before the overflow check:
      Table_Id_Assert.Eq (Small_Buf.Get_Table_Id, 5);
      Natural_Assert.Eq (Small_Buf.Get_Table_Length, 0);
      Natural_Assert.Eq (Small_Buf.Get_Packet_Count, 0);

      -- Successful FirstSegment resets everything:
      Status := Small_Buf.Append (Data => [16#00#, 16#0A#, 16#11#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Table_Id_Assert.Eq (Small_Buf.Get_Table_Id, 10);
      Natural_Assert.Eq (Small_Buf.Get_Table_Length, 1);
      Natural_Assert.Eq (Small_Buf.Get_Packet_Count, 1);

      Small_Buf.Destroy;
      pragma Unreferenced (Small_Buf);
   end Test_State_After_Errors;

   overriding procedure Test_Overflow_Preserves_Existing_Data (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Status : Append_Status;
      Small_Buf : Parameter_Table_Buffer.Instance;
   begin
      Small_Buf.Create (Buffer_Size => 4);

      -- FirstSegment with ID + 2 bytes payload (fills buffer to index 2):
      Status := Small_Buf.Append (Data => [16#00#, 16#01#, 16#AA#, 16#BB#], Sequence_Flag => Firstsegment);
      Append_Status_Assert.Eq (Status, New_Table);
      Natural_Assert.Eq (Small_Buf.Get_Table_Length, 2);

      -- Continuation that would overflow — existing data should be preserved:
      Status := Small_Buf.Append (Data => [16#CC#, 16#DD#, 16#EE#], Sequence_Flag => Continuationsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      -- Verify existing data is still intact:
      declare
         Region : constant Memory_Region.T := Small_Buf.Get_Table_Region;
         Result : Basic_Types.Byte_Array (0 .. 1);
         for Result'Address use Region.Address;
         pragma Import (Ada, Result);
      begin
         Natural_Assert.Eq (Region.Length, 2);
         Byte_Assert.Eq (Result (0), 16#AA#);
         Byte_Assert.Eq (Result (1), 16#BB#);
      end;

      -- LastSegment that would overflow — same preservation:
      Status := Small_Buf.Append (Data => [16#FF#, 16#EE#, 16#DD#], Sequence_Flag => Lastsegment);
      Append_Status_Assert.Eq (Status, Buffer_Overflow);

      -- Data still intact:
      declare
         Region : constant Memory_Region.T := Small_Buf.Get_Table_Region;
         Result : Basic_Types.Byte_Array (0 .. 1);
         for Result'Address use Region.Address;
         pragma Import (Ada, Result);
      begin
         Natural_Assert.Eq (Region.Length, 2);
         Byte_Assert.Eq (Result (0), 16#AA#);
         Byte_Assert.Eq (Result (1), 16#BB#);
      end;

      Small_Buf.Destroy;
      pragma Unreferenced (Small_Buf);
   end Test_Overflow_Preserves_Existing_Data;

end Parameter_Table_Buffer_Tests.Implementation;
