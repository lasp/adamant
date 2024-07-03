--------------------------------------------------------------------------------
-- Ccsds_Command_Depacketizer Tests Body
--------------------------------------------------------------------------------

with Ccsds_Space_Packet.Assertion; use Ccsds_Space_Packet.Assertion;
with Ccsds_Primary_Header.Assertion; use Ccsds_Primary_Header.Assertion;
use Ccsds_Primary_Header;
with Ccsds_Command_Secondary_Header;
with Interfaces; use Interfaces;
with Basic_Types; use Basic_Types;
with Xor_8;
with Command_Types;
with Basic_Assertions; use Basic_Assertions;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Command.Assertion; use Command.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Invalid_Packet_Xor8_Info.Assertion; use Invalid_Packet_Xor8_Info.Assertion;
with Invalid_Packet_Length.Assertion; use Invalid_Packet_Length.Assertion;
with Command_Id;
with Ccsds_Enums; use Ccsds_Enums;
with Configuration;
with Command;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;

package body Ccsds_Command_Depacketizer_Tests.Implementation is

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
   -- Private Functions:
   -------------------------------------------------------------------------

   -- Construct a CCSDS packet with the given variable parameters:
   function Construct_Packet (Data : Byte_Array; Packet_Type : Ccsds_Packet_Type.E := Ccsds_Packet_Type.Telecommand; Secondary_Header : Ccsds_Secondary_Header_Indicator.E := Ccsds_Secondary_Header_Indicator.Secondary_Header_Present; Apid : Ccsds_Apid_Type := 0; Sequence_Count : Ccsds_Sequence_Count_Type := 0) return Ccsds_Space_Packet.T is
      -- Define packet:
      Packet : Ccsds_Space_Packet.T := (Header => (Version => 0, Packet_Type => Packet_Type, Secondary_Header => Secondary_Header, Apid => Apid, Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented, Sequence_Count => Sequence_Count, Packet_Length => Data'Length - 1), Data => [others => 0]);
   begin
      -- Set data:
      Packet.Data (Packet.Data'First .. Packet.Data'First + Data'Length - 1) := Data;
      return Packet;
   end Construct_Packet;

   -- Construct a CCSDS command packet with the given variable parameters:
   function Construct_Command_Packet (Cmd_Id : Command_Types.Command_Id; Data : Byte_Array := [1 .. 0 => 0]; Packet_Type : Ccsds_Packet_Type.E := Ccsds_Packet_Type.Telecommand; Secondary_Header : Ccsds_Secondary_Header_Indicator.E := Ccsds_Secondary_Header_Indicator.Secondary_Header_Present; Apid : Ccsds_Apid_Type := 0; Sequence_Count : Ccsds_Sequence_Count_Type := 0; Checksum_Seed : Xor_8.Xor_8_Type := 255; Function_Code : Ccsds_Command_Secondary_Header.Function_Code_Type := 0; Packet_Length_Adjustment : Integer := 0) return Ccsds_Space_Packet.T is
      -- Define secondary header:
      The_Secondary_Header : Ccsds_Command_Secondary_Header.T := (Reserved => 0, Function_Code => Function_Code, Checksum => 0);
      -- Construct packet:
      Packet : Ccsds_Space_Packet.T := Construct_Packet (Ccsds_Command_Secondary_Header.Serialization.To_Byte_Array (The_Secondary_Header) & Command_Id.Serialization.To_Byte_Array ((Id => Cmd_Id)) & Data & (1 .. Natural (Function_Code) => 0), Packet_Type, Secondary_Header, Apid, Sequence_Count);
   begin
      -- Adjust packet length if requested:
      Packet.Header.Packet_Length := Unsigned_16 (Natural (Packet.Header.Packet_Length) + Packet_Length_Adjustment);
      declare
         -- Get packet header bytes:
         Header_Bytes : constant Byte_Array := Ccsds_Primary_Header.Serialization.To_Byte_Array (Packet.Header);
         -- Construct packet checksum:
         Checksum : constant Xor_8.Xor_8_Type := Xor_8.Compute_Xor_8 (Header_Bytes & Packet.Data, Checksum_Seed);
      begin
         -- Set the secondary header packet checksum:
         The_Secondary_Header.Checksum := Checksum;
         -- Recopy the secondary header into the packet:
         Packet.Data (Packet.Data'First .. Packet.Data'First + Ccsds_Command_Secondary_Header.Serialization.Serialized_Length - 1) := Ccsds_Command_Secondary_Header.Serialization.To_Byte_Array (The_Secondary_Header);
      end;

      -- Return the packet:
      return Packet;
   end Construct_Command_Packet;

   function Construct_Command (Id : Command_Types.Command_Id; Arg_Buffer : Byte_Array) return Command.T is
      -- Construct the command:
      Cmd : Command.T := (Header => (Source_Id => 0, Id => Id, Arg_Buffer_Length => Arg_Buffer'Length), Arg_Buffer => [others => 0]);
   begin
      -- Set the argument buffer:
      Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. Cmd.Arg_Buffer'First + Cmd.Header.Arg_Buffer_Length - 1) := Arg_Buffer;
      return Cmd;
   end Construct_Command;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Depacketization (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : constant Ccsds_Space_Packet.T := Construct_Command_Packet (17, [0 => 16]);
      Packet2 : constant Ccsds_Space_Packet.T := Construct_Command_Packet (22, Sequence_Count => 1);
      Cmd : constant Command.T := Construct_Command (Id => 17, Arg_Buffer => [Command_Types.Command_Arg_Buffer_Type'First => 16]);
      Cmd2 : constant Command.T := Construct_Command (Id => 22, Arg_Buffer => [1 .. 0 => 0]);
   begin
      -- Send the packet:
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect one command to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Cmd);

      -- Expect no events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Expect a single data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Accepted_Packet_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Accepted_Packet_Count_History.Get (1), (Value => 1));

      -- Send the packet:
      T.Ccsds_Space_Packet_T_Send (Packet2);

      -- Expect one command to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      -- Only check the command header, since the data might be bogus, and that is ok.
      Command_Header_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2).Header, Cmd2.Header);

      -- Expect no events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Expect a single data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Accepted_Packet_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Accepted_Packet_Count_History.Get (2), (Value => 2));
   end Test_Nominal_Depacketization;

   overriding procedure Test_Invalid_Packet_Checksum (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : constant Ccsds_Space_Packet.T := Construct_Command_Packet (17, [0 => 16], Checksum_Seed => 0);
      Invalid_Checksum_Info : constant Invalid_Packet_Xor8_Info.T := (Ccsds_Header => (Packet.Header, (0, 0, 221)), Computed_Checksum => 255, Expected_Checksum => 221);
   begin
      -- Send the packet many times:
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect no commands to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect 3 events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Packet_Checksum_History.Get_Count, 3);
      Invalid_Packet_Xor8_Info_Assert.Eq (T.Invalid_Packet_Checksum_History.Get (3), Invalid_Checksum_Info);

      -- Expect 3 error packets:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet);

      -- Expect 3 data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (1), (Value => 1));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (2), (Value => 2));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (3), (Value => 3));
   end Test_Invalid_Packet_Checksum;

   overriding procedure Test_Invalid_Packet_Type (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : constant Ccsds_Space_Packet.T := Construct_Command_Packet (17, [0 => 16], Packet_Type => Ccsds_Packet_Type.Telemetry);
   begin
      -- Send the packet many times:
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect no commands to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect 3 events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Packet_Type_History.Get_Count, 3);
      Ccsds_Primary_Header_Assert.Eq (T.Invalid_Packet_Type_History.Get (3), Packet.Header);

      -- Expect 3 error packets:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet);

      -- Expect 3 data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (1), (Value => 1));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (2), (Value => 2));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (3), (Value => 3));
   end Test_Invalid_Packet_Type;

   overriding procedure Test_Packet_Too_Small (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : constant Ccsds_Space_Packet.T := Construct_Command_Packet (16#0500#, Packet_Length_Adjustment => -1);
      Packet_2 : Ccsds_Space_Packet.T := Construct_Command_Packet (16#0500#, Function_Code => 127, Packet_Length_Adjustment => -1);
      Ccsds_Command_Secondary_Header_Length : constant Natural := Ccsds_Command_Secondary_Header.Serialization.Serialized_Length;
      Command_Id_Length : constant Natural := Command_Id.Serialization.Serialized_Length;
      Meta_Length : constant Natural := Command_Id_Length + Ccsds_Command_Secondary_Header_Length;
   begin
      -- Send the packet many times:
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect no commands to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect 3 events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Packet_Too_Small_History.Get_Count, 3);
      Invalid_Packet_Length_Assert.Eq (T.Packet_Too_Small_History.Get (3), (Ccsds_Header => Packet.Header, Length => Meta_Length - 1, Length_Bound => Meta_Length));

      -- Expect 3 error packets:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet);

      -- Expect 3 data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (1), (Value => 1));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (2), (Value => 2));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (3), (Value => 3));

      -- Set packet length of packet 2 to be smaller than function code. This makes sure that the FSW can handle a negative length:
      Packet_2.Header.Packet_Length := 15;
      Packet_2.Data (1) := 82; -- set the checksum so it gets through

      -- Send the packet many times:
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      T.Ccsds_Space_Packet_T_Send (Packet_2);

      -- Expect no commands to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect 3 events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Packet_Too_Small_History.Get_Count, 6);
      Invalid_Packet_Length_Assert.Eq (T.Packet_Too_Small_History.Get (6), (Ccsds_Header => Packet_2.Header, Length => -111, Length_Bound => Meta_Length));

      -- Expect 3 error packets:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 6);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (4), Packet_2);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (5), Packet_2);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (6), Packet_2);

      -- Expect 3 data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 6);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (4), (Value => 4));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (5), (Value => 5));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (6), (Value => 6));
   end Test_Packet_Too_Small;

   overriding procedure Test_Packet_Too_Large (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Bytes : constant Byte_Array := [0 .. 997 => 1];
      Packet : Ccsds_Space_Packet.T := Construct_Command_Packet (16#0101#, Bytes);
   begin
      -- Send the packet many times:
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect no commands to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect 3 events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Packet_Too_Large_History.Get_Count, 3);
      Invalid_Packet_Length_Assert.Eq (T.Packet_Too_Large_History.Get (3), (Ccsds_Header => Packet.Header, Length => Bytes'Length, Length_Bound => Configuration.Command_Buffer_Size));

      -- Expect 3 error packets:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet);

      -- Expect 3 data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (1), (Value => 1));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (2), (Value => 2));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (3), (Value => 3));

      -- Adjust the packet length so that it is larger than a CCSDS packet buffer, this
      -- should cause a slightly different error:
      Packet.Header.Packet_Length := 5_000;

      -- Send the packet many times:
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect no commands to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect 3 events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Packet_Too_Large_History.Get_Count, 6);
      Invalid_Packet_Length_Assert.Eq (T.Packet_Too_Large_History.Get (6), (Ccsds_Header => Packet.Header, Length => 5_001, Length_Bound => Packet.Data'Length));

      -- Expect 3 error packets:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 6);
      -- Note, we cannot check the entire CCSDS packet in this case, because it is too large to fit
      -- into a Packet.T type, so it gets truncated. Let's just make sure the header is good.
      -- Ccsds_Space_Packet_Assert.eq(t.Error_Packet_History.get(4), packet);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (4).Header, Packet.Header);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (5).Header, Packet.Header);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (6).Header, Packet.Header);

      -- Expect 3 data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 6);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (4), (Value => 4));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (5), (Value => 5));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (6), (Value => 6));
   end Test_Packet_Too_Large;

   overriding procedure Test_Packet_Without_Secondary_Header (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet : constant Ccsds_Space_Packet.T := Construct_Command_Packet (17, [0 => 16], Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present);
   begin
      -- Send the packet many times:
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);
      T.Ccsds_Space_Packet_T_Send (Packet);

      -- Expect no commands to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect 3 events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.No_Secondary_Header_History.Get_Count, 3);
      Ccsds_Primary_Header_Assert.Eq (T.No_Secondary_Header_History.Get (3), Packet.Header);

      -- Expect 3 error packets:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet);

      -- Expect 3 data product to be sent:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (1), (Value => 1));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (2), (Value => 2));
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (3), (Value => 3));
   end Test_Packet_Without_Secondary_Header;

   overriding procedure Test_Pad_Bytes (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet_1 : constant Ccsds_Space_Packet.T := Construct_Command_Packet (17, [0 => 16], Sequence_Count => 0, Function_Code => 5);
      Packet_2 : constant Ccsds_Space_Packet.T := Construct_Command_Packet (17, [0 => 16], Sequence_Count => 1, Function_Code => 127);
      Packet_3 : constant Ccsds_Space_Packet.T := Construct_Command_Packet (17, [0 => 16], Sequence_Count => 2, Function_Code => 20);
      Cmd : constant Command.T := Construct_Command (Id => 17, Arg_Buffer => [Command_Types.Command_Arg_Buffer_Type'First => 16]);
   begin
      -- Send the packet:
      T.Ccsds_Space_Packet_T_Send (Packet_1);

      -- Expect one command to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Cmd);

      -- Expect no events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send the packet:
      T.Ccsds_Space_Packet_T_Send (Packet_2);

      -- Expect one command to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Cmd);

      -- Expect no events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send the packet:
      T.Ccsds_Space_Packet_T_Send (Packet_3);

      -- Expect one command to have been sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Cmd);

      -- Expect no events to be thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
   end Test_Pad_Bytes;

   overriding procedure Test_Reset_Counts (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Seed the component by running the nominal test.
      Self.Test_Nominal_Depacketization;
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Event_T_Recv_Sync_History.Clear;
      T.Accepted_Packet_Count_History.Clear;
      T.Rejected_Packet_Count_History.Clear;

      -- OK now send command to reset data products
      T.Command_T_Send (T.Commands.Reset_Counts);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Reset_Counts_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Accepted_Packet_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Accepted_Packet_Count_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Rejected_Packet_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Rejected_Packet_Count_History.Get (1), (Value => 0));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Counts_Reset_History.Get_Count, 1);
   end Test_Reset_Counts;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Ccsds_Command_Depacketizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Reset_Counts;
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 5;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Reset_Counts_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Reset_Counts_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 5]));
   end Test_Invalid_Command;

end Ccsds_Command_Depacketizer_Tests.Implementation;
