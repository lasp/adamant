--------------------------------------------------------------------------------
-- Ccsds_Command_Forwarder Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Ccsds_Command_Space_Packet;
with Ccsds_Enums;
with Ccsds_Primary_Header;
with Ccsds_Space_Packet.Assertion; use Ccsds_Space_Packet.Assertion;
with Ccsds_Primary_Header.Assertion; use Ccsds_Primary_Header.Assertion;
with Command;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Interfaces; use Interfaces;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Serializer_Types;

package body Ccsds_Command_Forwarder_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helpers:
   -------------------------------------------------------------------------

   -- Construct a CCSDS primary header for a test packet:
   function Make_Header (Apid : in Ccsds_Primary_Header.Ccsds_Apid_Type; Sequence_Count : in Ccsds_Primary_Header.Ccsds_Sequence_Count_Type; Packet_Length : in Unsigned_16) return Ccsds_Primary_Header.T is
      ((
         Version => 0,
         Packet_Type => Ccsds_Enums.Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Enums.Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Apid,
         Sequence_Flag => Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Sequence_Count,
         Packet_Length => Packet_Length
      ));

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   -- This unit test tests that CCSDS packets received by command are forwarded out
   -- of the CCSDS space packet send connector with the expected contents, and that
   -- the appropriate event and data product are produced.
   overriding procedure Test_Nominal_Forwarding (Self : in out Instance) is
      use Serializer_Types;
      T : Component.Ccsds_Command_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      -- A packet with 3 bytes of data (Packet_Length is data length minus one):
      Packet_1 : constant Ccsds_Command_Space_Packet.T := (
         Header => Make_Header (Apid => 42, Sequence_Count => 15, Packet_Length => 2),
         Data => [0 => 1, 1 => 2, 2 => 3, others => 0]
      );
      -- A packet with 1 byte of data, the minimum size CCSDS packet:
      Packet_2 : constant Ccsds_Command_Space_Packet.T := (
         Header => Make_Header (Apid => 19, Sequence_Count => 16, Packet_Length => 0),
         Data => [0 => 255, others => 0]
      );
   begin
      -- Make sure the initial value of the data product was sent at Set_Up:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packets_Forwarded_Count_History.Get_Count, 1);
      Packed_U32_Assert.Eq (T.Packets_Forwarded_Count_History.Get (1), (Value => 0));

      -- Send the command to forward the first packet:
      pragma Assert (T.Commands.Forward_Packet (Packet_1, Cmd) = Success);
      T.Command_T_Send (Cmd);

      -- Make sure the packet was forwarded with the expected contents:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), (
         Header => Packet_1.Header,
         Data => [0 => 1, 1 => 2, 2 => 3, others => 0]
      ));

      -- Make sure the command was responded to with success:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Forward_Packet_Id, Status => Success));

      -- Make sure the packet forwarded event was sent with the packet's header:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Forwarded_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Packet_Forwarded_History.Get (1), Packet_1.Header);

      -- Make sure the data product was updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packets_Forwarded_Count_History.Get_Count, 2);
      Packed_U32_Assert.Eq (T.Packets_Forwarded_Count_History.Get (2), (Value => 1));

      -- Send the command to forward the second packet:
      pragma Assert (T.Commands.Forward_Packet (Packet_2, Cmd) = Success);
      T.Command_T_Send (Cmd);

      -- Make sure the packet was forwarded with the expected contents:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), (
         Header => Packet_2.Header,
         Data => [0 => 255, others => 0]
      ));

      -- Make sure the command was responded to with success:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Forward_Packet_Id, Status => Success));

      -- Make sure the packet forwarded event was sent with the packet's header:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Forwarded_History.Get_Count, 2);
      Ccsds_Primary_Header_Assert.Eq (T.Packet_Forwarded_History.Get (2), Packet_2.Header);

      -- Make sure the data product was updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Packets_Forwarded_Count_History.Get_Count, 3);
      Packed_U32_Assert.Eq (T.Packets_Forwarded_Count_History.Get (3), (Value => 2));

      -- Make sure no invalid command events were thrown:
      Boolean_Assert.Eq (T.Invalid_Command_Received_History.Is_Empty, True);
   end Test_Nominal_Forwarding;

   -- This unit test tests that a CCSDS packet with the maximum sized data field that
   -- fits within a command argument is forwarded correctly.
   overriding procedure Test_Max_Size_Packet (Self : in out Instance) is
      use Serializer_Types;
      T : Component.Ccsds_Command_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      -- The maximum number of data bytes that fits within the command argument:
      Max_Data_Length : constant Natural := Ccsds_Command_Space_Packet.Ccsds_Command_Data_Type'Length;
      -- A packet with the maximum sized data field:
      Max_Packet : constant Ccsds_Command_Space_Packet.T := (
         Header => Make_Header (Apid => 100, Sequence_Count => 0, Packet_Length => Unsigned_16 (Max_Data_Length - 1)),
         Data => [others => 16#AB#]
      );
      -- The expected forwarded packet, which has the same data but padded out with
      -- zeros to the size of the full CCSDS space packet:
      Expected_Packet : Ccsds_Space_Packet.T := (
         Header => Max_Packet.Header,
         Data => [others => 0]
      );
   begin
      -- Fill in the expected packet data:
      Expected_Packet.Data (0 .. Max_Data_Length - 1) := [others => 16#AB#];

      -- Send the command to forward the packet:
      pragma Assert (T.Commands.Forward_Packet (Max_Packet, Cmd) = Success);
      T.Command_T_Send (Cmd);

      -- Make sure the packet was forwarded with the expected contents:
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Expected_Packet);

      -- Make sure the command was responded to with success:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Forward_Packet_Id, Status => Success));

      -- Make sure the packet forwarded event was sent with the packet's header:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Forwarded_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Packet_Forwarded_History.Get (1), Max_Packet.Header);

      -- Make sure the data product was updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packets_Forwarded_Count_History.Get_Count, 2);
      Packed_U32_Assert.Eq (T.Packets_Forwarded_Count_History.Get (2), (Value => 1));
   end Test_Max_Size_Packet;

   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      use Serializer_Types;
      T : Component.Ccsds_Command_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      -- A valid command which we will corrupt the length of:
      Cmd : Command.T;
   begin
      -- Construct a valid command:
      pragma Assert (T.Commands.Forward_Packet ((
         Header => Make_Header (Apid => 42, Sequence_Count => 0, Packet_Length => 0),
         Data => [others => 0]
      ), Cmd) = Success);

      -- Make the command invalid by modifying its length:
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Forward_Packet_Id, Status => Length_Error));

      -- Make sure the invalid command event was thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Forward_Packet_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));

      -- Make sure no packet was forwarded and the data product was not updated:
      Boolean_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Packet_Forwarded_History.Is_Empty, True);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packets_Forwarded_Count_History.Get_Count, 1);
      Packed_U32_Assert.Eq (T.Packets_Forwarded_Count_History.Get (1), (Value => 0));
   end Test_Invalid_Command;

end Ccsds_Command_Forwarder_Tests.Implementation;
