--------------------------------------------------------------------------------
-- Ccsds_Packetizer Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Packet;
with Basic_Types.Representation;
with Crc_16;
with Interfaces; use Interfaces;
with Basic_Assertions; use Basic_Assertions;
with Ccsds_Primary_Header.Assertion; use Ccsds_Primary_Header.Assertion;
with Ccsds_Space_Packet.Representation;
with Sys_Time.Assertion; use Sys_Time.Assertion;
with Packet_Types;
with Ada.Text_IO; use Ada.Text_IO;
with Ccsds_Enums; use Ccsds_Enums;

package body Ccsds_Packetizer_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helper functions:
   -------------------------------------------------------------------------

   procedure Check_Packet (P : in Packet.T; Ccsds_Packet : in Ccsds_Space_Packet.T) is
      -- Use clauses:
      use Basic_Types;
      use Crc_16;
      use Ccsds_Primary_Header;

      -- Define CCSDS header:
      Header : constant Ccsds_Primary_Header.T :=
         (Version => 0, Packet_Type => Ccsds_Packet_Type.Telemetry, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Present, Apid => Ccsds_Apid_Type (P.Header.Id), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
          Sequence_Count => Ccsds_Sequence_Count_Type (P.Header.Sequence_Count), Packet_Length => Unsigned_16 (Sys_Time.Serialization.Serialized_Length + P.Header.Buffer_Length + Crc_16_Type'Length - 1));

      -- Timestamp:
      Time_Deserialized : Sys_Time.T;
   begin
      -- Check CCSDS header:
      Ccsds_Primary_Header_Assert.Eq (Ccsds_Packet.Header, Header);

      -- Check timestamp:
      Time_Deserialized := Sys_Time.Serialization.From_Byte_Array (Ccsds_Packet.Data (0 .. Sys_Time.Serialization.Serialized_Length - 1));
      Sys_Time_Assert.Eq (Time_Deserialized, P.Header.Time);

      -- Check data:
      Assert (Ccsds_Packet.Data (Sys_Time.Serialization.Serialized_Length .. Natural (Ccsds_Packet.Header.Packet_Length) - Crc_16_Type'Length) = P.Buffer (P.Buffer'First .. P.Buffer'First + P.Header.Buffer_Length - 1), "Comparing buffers failed.");

      -- Check checksum:
      declare
         Crc : constant Crc_16_Type := Ccsds_Packet.Data (Natural (Ccsds_Packet.Header.Packet_Length) - Crc_16_Type'Length + 1 .. Natural (Ccsds_Packet.Header.Packet_Length));
         Overlay : Basic_Types.Byte_Array (0 .. Natural (Ccsds_Packet.Header.Packet_Length) + Ccsds_Primary_Header.Serialization.Serialized_Length - Crc_16_Type'Length) with
            Import,
            Convention => Ada,
            Address => Ccsds_Packet'Address;
      begin
         Put_Line (Ccsds_Space_Packet.Representation.Image (Ccsds_Packet));
         Put_Line (Basic_Types.Representation.Image (Crc));
         Assert (Crc = Compute_Crc_16 (Overlay), "Comparing checksums failed.");
      end;
   end Check_Packet;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Packetization (Self : in out Instance) is
      use Packet_Types;
      T : Component.Ccsds_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      P : Packet.T := (Header => (Time => (10, 55), Id => 77, Sequence_Count => 99, Buffer_Length => 5), Buffer => [1, 2, 3, 4, 5, others => 0]);
   begin
      -- Send a few packets:
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);

      -- Check packet contents:
      P.Header.Sequence_Count := 99;
      for Idx in 1 .. 5 loop
         P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
         Check_Packet (P, T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (Idx));
      end loop;
   end Test_Nominal_Packetization;

   overriding procedure Test_Max_Size_Packetization (Self : in out Instance) is
      use Packet_Types;
      T : Component.Ccsds_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      P : Packet.T :=
         (Header => (Time => (10, 55), Id => 77, Sequence_Count => 13, Buffer_Length => Packet_Types.Packet_Buffer_Type'Length),
          Buffer => [0 => 1, 1 => 2, 2 => 3, 3 => 4, 4 => 5, 5 .. Packet_Types.Packet_Buffer_Type'Last - 1 => 22, Packet_Types.Packet_Buffer_Type'Last => 9]);
   begin
      -- Send a few packets:
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);

      -- Check packet contents:
      P.Header.Sequence_Count := 13;
      for Idx in 1 .. 5 loop
         P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
         Check_Packet (P, T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (Idx));
      end loop;
   end Test_Max_Size_Packetization;

   overriding procedure Test_Min_Size_Packetization (Self : in out Instance) is
      use Packet_Types;
      T : Component.Ccsds_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      P : Packet.T := (Header => (Time => (10, 55), Id => 13, Sequence_Count => 77, Buffer_Length => 0), Buffer => [0 => 1, 1 => 2, 2 => 3, 3 => 4, 4 => 5, 5 .. Packet_Types.Packet_Buffer_Type'Last - 1 => 22, Packet_Types.Packet_Buffer_Type'Last => 9]);
   begin
      -- Send a few packets:
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
      T.Packet_T_Send (P);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);

      -- Check packet contents:
      P.Header.Sequence_Count := 77;
      for Idx in 1 .. 5 loop
         P.Header.Sequence_Count := P.Header.Sequence_Count + 1;
         Check_Packet (P, T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (Idx));
      end loop;
   end Test_Min_Size_Packetization;

end Ccsds_Packetizer_Tests.Implementation;
