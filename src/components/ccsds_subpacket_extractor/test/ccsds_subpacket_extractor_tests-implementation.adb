--------------------------------------------------------------------------------
-- Ccsds_Subpacket_Extractor Tests Body
--------------------------------------------------------------------------------

with Ccsds_Space_Packet.Assertion; use Ccsds_Space_Packet.Assertion;
with Serializer_Types;
with Ccsds_Primary_Header.Assertion; use Ccsds_Primary_Header;
use Ccsds_Primary_Header.Assertion;
with Invalid_Packet_Length.Assertion; use Invalid_Packet_Length.Assertion;
with Interfaces; use Interfaces;
with Basic_Assertions; use Basic_Assertions;
with Packed_Byte.Assertion; use Packed_Byte.Assertion;
with Ccsds_Enums; use Ccsds_Enums;

package body Ccsds_Subpacket_Extractor_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   type Ccsds_Array is array (Natural range <>) of Ccsds_Space_Packet.T;

   function Create_Packet (Packet_Array : in Ccsds_Array; Start_Offset : in Natural := 0; Stop_Offset : in Natural := 0) return Ccsds_Space_Packet.T is
      use Serializer_Types;
      To_Return : Ccsds_Space_Packet.T;
      Idx : Natural := Start_Offset;
      Last_Index : constant Natural := To_Return.Data'Last - Stop_Offset;
      Stat : Serialization_Status;
      Num_Bytes_Serialized : Natural;
   begin
      for Packet of Packet_Array loop
         Stat := Ccsds_Space_Packet.Serialization.To_Byte_Array (To_Return.Data (Idx .. Last_Index), Packet, Num_Bytes_Serialized);
         pragma Assert (Stat = Success, "Failed to serialize CCSDS inside of another CCSDS");
         Idx := Idx + Num_Bytes_Serialized;
      end loop;
      To_Return.Header.Packet_Length := Unsigned_16 (Idx) + Unsigned_16 (Stop_Offset) - 1;
      return To_Return;
   end Create_Packet;

   -- Normal small packet:
   Packet_1 : constant Ccsds_Space_Packet.T :=
      (Header =>
          (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (17), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
            Sequence_Count => Ccsds_Sequence_Count_Type (53), Packet_Length => 6),
       Data => [1, 2, 3, 4, 5, 6, 7, others => 0]);
   -- Max size packet:
   Packet_2 : constant Ccsds_Space_Packet.T :=
      (Header =>
          (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (22), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
            Sequence_Count => Ccsds_Sequence_Count_Type (12), Packet_Length => Unsigned_16 (Ccsds_Space_Packet.Ccsds_Data_Type'Length) - Unsigned_16 (Ccsds_Primary_Header.Size_In_Bytes) - 1),
       Data => [others => 99]);
   -- Another normal small packet:
   Packet_3 : constant Ccsds_Space_Packet.T :=
      (Header =>
          (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (13), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
            Sequence_Count => Ccsds_Sequence_Count_Type (1), Packet_Length => 17),
       Data => [1, 2, 3, 4, 5, 6, 7, others => 255]);

   overriding procedure Nominal_Extraction (Self : in out Instance) is
      T : Component.Ccsds_Subpacket_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send over the sync connector first:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_1]));

      -- Expect no events to be thrown and one packet to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packet:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_1);

      -- Now try the async connector:
      T.Ccsds_Space_Packet_T_Send_2 (Create_Packet ([0 => Packet_1]));

      -- Expect no change, since we have not drained the queue:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);

      -- Expect another packet extracted:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packet:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), Packet_1);

      -- Send over the sync connector first:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_2]));

      -- Expect no events to be thrown and one packet to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packet:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (3), Packet_2);

      -- Now try the async connector:
      T.Ccsds_Space_Packet_T_Send_2 (Create_Packet ([0 => Packet_2]));

      -- Expect no change, since we have not drained the queue:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);

      -- Expect another packet extracted:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packet:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (4), Packet_2);

      -- Send over the sync connector first:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_1, 1 => Packet_3, 2 => Packet_1, 3 => Packet_3]));

      -- Expect no events to be thrown and one packet to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packets:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (5), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (6), Packet_3);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (7), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (8), Packet_3);

      -- Now try the async connector:
      T.Ccsds_Space_Packet_T_Send_2 (Create_Packet ([0 => Packet_3, 1 => Packet_1, 2 => Packet_3, 3 => Packet_1]));

      -- Expect no events to be thrown and one packet to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Drain the queue:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);

      -- Expect another packet extracted:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packets:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (9), Packet_3);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (10), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (11), Packet_3);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (12), Packet_1);
   end Nominal_Extraction;

   overriding procedure Test_Invalid_Length (Self : in out Instance) is
      T : Component.Ccsds_Subpacket_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : Ccsds_Space_Packet.T;
   begin
      --
      -- Too small test:
      --

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_1]);
      -- Override packet header to make it too small:
      Packet.Header.Packet_Length := Unsigned_16 (Ccsds_Primary_Header.Size_In_Bytes) - 2;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 1);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get (1), (Packet.Header, Ccsds_Primary_Header.Size_In_Bytes - 1, Ccsds_Primary_Header.Size_In_Bytes));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_2]);
      -- Override packet header to make it too small:
      Packet.Header.Packet_Length := 0;
      T.Ccsds_Space_Packet_T_Send_2 (Packet);

      -- Drain the queue:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 2);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get (2), (Packet.Header, 1, Ccsds_Primary_Header.Size_In_Bytes));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);

      --
      -- Too large test:
      --

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_2]);
      -- Override packet header to make it too large:
      Packet.Header.Packet_Length := Packet.Data'Length * 8;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 3);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get (3), (Packet.Header, Packet.Data'Length * 8 + 1, Packet.Data'Length));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (3).Header, Packet.Header);

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_2]);
      -- Override packet header to make it too large:
      Packet.Header.Packet_Length := Packet.Data'Length;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 4);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get (4), (Packet.Header, Packet.Data'Length + 1, Packet.Data'Length));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 4);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (4).Header, Packet.Header);

      -- Send over the async connector:
      Packet := Create_Packet ([0 => Packet_3]);
      -- Override packet header to make it too large:
      Packet.Header.Packet_Length := Packet.Data'Length;

      -- On the async connector, this shouldn't even be allowed to get queued up. Make sure
      -- it gets dropped.
      T.Expect_Ccsds_Space_Packet_T_Send_2_Dropped := True;
      T.Ccsds_Space_Packet_T_Send_2 (Packet);

      -- Drain the queue, but expect empty:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 0);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dropped_Packet_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Dropped_Packet_History.Get (1), Packet.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 5);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (5).Header, Packet.Header);
   end Test_Invalid_Length;

   overriding procedure Test_Invalid_Subpacket_Length (Self : in out Instance) is
      T : Component.Ccsds_Subpacket_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : Ccsds_Space_Packet.T;
   begin
      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_1, 1 => Packet_3]);
      -- Override packet header to make it too small:
      Packet.Header.Packet_Length := Packet.Header.Packet_Length - 1;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect event to be thrown and the last packet not extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Extracted_Packet_Length_History.Get_Count, 1);
      declare
         use Serializer_Types;
         Len : Natural;
         Ignore : Serialization_Status;
      begin
         Ignore := Ccsds_Space_Packet.Serialized_Length (Packet_3, Len);
         Invalid_Packet_Length_Assert.Eq (T.Invalid_Extracted_Packet_Length_History.Get (1), (Packet_3.Header, Natural (Packet_3.Header.Packet_Length), Len - 1));
      end;
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_2]);
      -- Override packet header to make it way too small:
      Packet.Header.Packet_Length := Packet.Header.Packet_Length - Unsigned_16 (Ccsds_Primary_Header.Size_In_Bytes);
      T.Ccsds_Space_Packet_T_Send_2 (Packet);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Extracted_Packet_Length_History.Get_Count, 2);
      declare
         use Serializer_Types;
         Len : Natural;
         Ignore : Serialization_Status;
      begin
         Ignore := Ccsds_Space_Packet.Serialized_Length (Packet_2, Len);
         Invalid_Packet_Length_Assert.Eq (T.Invalid_Extracted_Packet_Length_History.Get (2), (Packet_2.Header, Natural (Packet_2.Header.Packet_Length), Len - Ccsds_Primary_Header.Size_In_Bytes));
      end;
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 2);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (2).Header, Packet.Header);
   end Test_Invalid_Subpacket_Length;

   overriding procedure Test_Remaining_Bytes (Self : in out Instance) is
      T : Component.Ccsds_Subpacket_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : Ccsds_Space_Packet.T;
   begin
      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_1, 1 => Packet_3]);
      -- Override packet header to make it too large by a little:
      Packet.Header.Packet_Length := Packet.Header.Packet_Length + 1;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect 2 packets to be extracted correctly:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), Packet_3);

      -- Expect event to be thrown and the last packet not extracted.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Trailing_Bytes_History.Get_Count, 1);
      Packed_Byte_Assert.Eq (T.Dropped_Trailing_Bytes_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);

      -- Send over the async connector:
      Packet := Create_Packet ([0 => Packet_3]);
      -- Override packet header to make it too large by a lot:
      Packet.Data (Natural (Packet.Header.Packet_Length) + 1 .. Packet.Data'Last) := [others => 99];
      Packet.Header.Packet_Length := Packet.Header.Packet_Length + Unsigned_16 (Ccsds_Primary_Header.Size_In_Bytes);
      T.Ccsds_Space_Packet_T_Send_2 (Packet);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);

      -- Expect 1 packet to be extracted correctly:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (3), Packet_3);

      -- Expect event to be thrown and the last packet not extracted.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dropped_Trailing_Bytes_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Extracted_Packet_Length_History.Get_Count, 1);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Extracted_Packet_Length_History.Get (1), (Ccsds_Primary_Header.Serialization.From_Byte_Array ([others => 16#63#]), 16#6363#, Ccsds_Primary_Header.Size_In_Bytes));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);
   end Test_Remaining_Bytes;

   overriding procedure Test_Dropped_Packet (Self : in out Instance) is
      T : Component.Ccsds_Subpacket_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : Ccsds_Space_Packet.T;
   begin
      -- Send packets out of the async connector:
      T.Ccsds_Space_Packet_T_Send_2 (Create_Packet ([0 => Packet_2]));
      T.Ccsds_Space_Packet_T_Send_2 (Create_Packet ([0 => Packet_2]));
      T.Ccsds_Space_Packet_T_Send_2 (Create_Packet ([0 => Packet_2]));
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_3]));
      -- We should overflow here:
      T.Expect_Ccsds_Space_Packet_T_Send_2_Dropped := True;
      Packet := Create_Packet ([0 => Packet_1]);
      T.Ccsds_Space_Packet_T_Send_2 (Packet);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Packet_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Dropped_Packet_History.Get (1), Packet.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);

      -- Drain the queue:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 3);

      -- Expect packets extracted:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), Packet_2);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (3), Packet_2);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (4), Packet_2);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
   end Test_Dropped_Packet;

   overriding procedure Test_Offsets (Self : in out Instance) is
      T : Component.Ccsds_Subpacket_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : Ccsds_Space_Packet.T;
   begin
      -- Initialize component with some offsets:
      T.Component_Instance.Init (Start_Offset => 4, Stop_Offset => 6);

      -- Send over the sync connector first:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_1], Start_Offset => 4, Stop_Offset => 6));

      -- Expect no events to be thrown and one packet to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packet:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_1);

      -- Send over the sync connector first:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_1, 1 => Packet_3, 2 => Packet_1, 3 => Packet_3], Start_Offset => 4, Stop_Offset => 6));

      -- Expect no events to be thrown and one packet to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packets:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (3), Packet_3);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (4), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (5), Packet_3);

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_1, 1 => Packet_3], Start_Offset => 4, Stop_Offset => 6);
      -- Override packet header to make it too large by a little:
      Packet.Header.Packet_Length := Packet.Header.Packet_Length + 1;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect 2 packets to be extracted correctly:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (6), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (7), Packet_3);

      -- Expect event to be thrown and the last packet not extracted.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Trailing_Bytes_History.Get_Count, 1);
      Packed_Byte_Assert.Eq (T.Dropped_Trailing_Bytes_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_1], Start_Offset => 4, Stop_Offset => 6);
      -- Override packet header to make it too small:
      Packet.Header.Packet_Length := Unsigned_16 (Ccsds_Primary_Header.Size_In_Bytes) - 2;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 1);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get (1), (Packet.Header, Ccsds_Primary_Header.Size_In_Bytes - 1 - 4 - 6, Ccsds_Primary_Header.Size_In_Bytes));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);

      -- Send over the sync connector first:
      Packet := Create_Packet ([0 => Packet_1], Start_Offset => 4, Stop_Offset => 6);
      -- Override packet header to make it too large:
      Packet.Header.Packet_Length := Packet.Data'Length * 8;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 2);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get (2), (Packet.Header, Packet.Data'Length * 8 + 1, Packet.Data'Length));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (3).Header, Packet.Header);

      -- Send over the sync connector first, make offsets so big nothing can be extracted:
      Packet := Create_Packet ([0 => Packet_1]);
      -- Override packet header to make it too small:
      Packet.Header.Packet_Length := 1;
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect event to be thrown and no packet extracted.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get_Count, 3);
      Invalid_Packet_Length_Assert.Eq (T.Invalid_Received_Packet_Length_History.Get (3), (Packet.Header, 2 - 6 - 4, Ccsds_Primary_Header.Size_In_Bytes));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 4);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (4).Header, Packet.Header);

   end Test_Offsets;

   overriding procedure Test_Max_Subpackets_To_Extract (Self : in out Instance) is
      T : Component.Ccsds_Subpacket_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Initialize component with max subpackets to extract being zero:
      T.Component_Instance.Init (Max_Subpackets_To_Extract => 0);

      -- Send large packet:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_1, 1 => Packet_3, 2 => Packet_1, 3 => Packet_3]));

      -- Expect no events to be thrown and no packets to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Initialize component with max subpackets to extract being 2:
      T.Component_Instance.Init (Max_Subpackets_To_Extract => 2);

      -- Send large packet containing 4 subpackets:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_1, 1 => Packet_3, 2 => Packet_1, 3 => Packet_3]));

      -- Expect no events to be thrown and two packets to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packets:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), Packet_3);

      -- Initialize component with max subpackets to extract being 1:
      T.Component_Instance.Init (Max_Subpackets_To_Extract => 1);

      -- Send large packet containing 4 subpackets:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_3, 1 => Packet_1, 2 => Packet_3]));

      -- Expect no events to be thrown and two packets to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packets:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (3), Packet_3);

      -- Initialize component with max subpackets to extract being 3:
      T.Component_Instance.Init (Max_Subpackets_To_Extract => 3);

      -- Send large packet containing 3 subpackets:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_3, 1 => Packet_1, 2 => Packet_3]));

      -- Expect no events to be thrown and two packets to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packets:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (4), Packet_3);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (5), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (6), Packet_3);

      -- Initialize component with max subpackets to extract being 18:
      T.Component_Instance.Init (Max_Subpackets_To_Extract => 18);

      -- Send large packet containing 3 subpackets:
      T.Ccsds_Space_Packet_T_Send (Create_Packet ([0 => Packet_3, 1 => Packet_1, 2 => Packet_3]));

      -- Expect no events to be thrown and two packets to be received:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check the packets:
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (7), Packet_3);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (8), Packet_1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (9), Packet_3);
   end Test_Max_Subpackets_To_Extract;

end Ccsds_Subpacket_Extractor_Tests.Implementation;
