--------------------------------------------------------------------------------
-- Ccsds_Downsampler Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Ccsds_Space_Packet;
with Command;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Interfaces; use Interfaces;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Test_Downsample_List;
with Data_Product.Assertion; use Data_Product.Assertion;
with Data_Product_Types;
with Packed_U16;

package body Ccsds_Downsampler_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Downsample_List => Test_Downsample_List.Downsample_List_Access);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- Setup the time
      Self.Tester.System_Time := (0, 0);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -- Helper function to create data product definitions
   function Test_Dp_Received (Id : in Data_Product_Types.Data_Product_Id; Product_Value : in Unsigned_16) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Id => Id, Time => (0, 0), Buffer_Length => Packed_U16.Serialization.Serialized_Length), Buffer => [others => 0]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_U16.Serialization.Serialized_Length - 1) := Packed_U16.Serialization.To_Byte_Array ((Value => Product_Value));
      return Dp;
   end Test_Dp_Received;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Downsample_Packet (Self : in out Instance) is
      T : Component.Ccsds_Downsampler.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Packet : Ccsds_Space_Packet.T;
      Packed_Passed_Dp_Id : constant Data_Product_Types.Data_Product_Id := 1;
      Packed_Filtered_Dp_Id : constant Data_Product_Types.Data_Product_Id := 0;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Downsampled Packets:");
      Put_Line ("----------------------------------");

      -- Send in a bunch of different ids a bunch and make sure we see the correct behavior.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (1), Test_Dp_Received (Packed_Filtered_Dp_Id, 0));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Test_Dp_Received (Packed_Passed_Dp_Id, 0));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Test_Dp_Received (2, 0));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Test_Dp_Received (3, 3));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Test_Dp_Received (4, 1));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (6), Test_Dp_Received (5, 4));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (7), Test_Dp_Received (6, 2));
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);

      -- All packets Filtered, check the count of the data products as well.
      Incoming_Packet.Header.Apid := 100;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (8), Test_Dp_Received (Packed_Filtered_Dp_Id, 1));

      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (9), Test_Dp_Received (Packed_Filtered_Dp_Id, 2));

      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 10);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (10), Test_Dp_Received (Packed_Filtered_Dp_Id, 3));

      -- All packets passed
      Incoming_Packet.Header.Apid := 300;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      -- Data Products
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 11);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (11), Test_Dp_Received (Packed_Passed_Dp_Id, 1));

      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 12);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (12), Test_Dp_Received (Packed_Passed_Dp_Id, 2));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 13);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (13), Test_Dp_Received (Packed_Passed_Dp_Id, 3));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 14);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (14), Test_Dp_Received (Packed_Passed_Dp_Id, 4));

      -- Every 3rd packet passed
      Incoming_Packet.Header.Apid := 200;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 15);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (15), Test_Dp_Received (Packed_Passed_Dp_Id, 5));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (16), Test_Dp_Received (Packed_Filtered_Dp_Id, 4));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 17);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (17), Test_Dp_Received (Packed_Filtered_Dp_Id, 5));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 18);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (18), Test_Dp_Received (Packed_Passed_Dp_Id, 6));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 19);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (19), Test_Dp_Received (Packed_Filtered_Dp_Id, 6));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 20);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (20), Test_Dp_Received (Packed_Filtered_Dp_Id, 7));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 21);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (21), Test_Dp_Received (Packed_Passed_Dp_Id, 7));

      -- Apids that are not found should be passed along
      Incoming_Packet.Header.Apid := 101;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 22);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (22), Test_Dp_Received (Packed_Passed_Dp_Id, 8));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 23);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (23), Test_Dp_Received (Packed_Passed_Dp_Id, 9));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 24);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (24), Test_Dp_Received (Packed_Passed_Dp_Id, 10));

      Incoming_Packet.Header.Apid := 501;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 25);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (25), Test_Dp_Received (Packed_Passed_Dp_Id, 11));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 26);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (26), Test_Dp_Received (Packed_Passed_Dp_Id, 12));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 27);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (27), Test_Dp_Received (Packed_Passed_Dp_Id, 13));

   end Test_Downsample_Packet;

   overriding procedure Test_Modify_Filter_Factor (Self : in out Instance) is
      T : Component.Ccsds_Downsampler.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Packet : Ccsds_Space_Packet.T;
      Packed_Passed_Dp_Id : constant Data_Product_Types.Data_Product_Id := 1;
      Packed_Filtered_Dp_Id : constant Data_Product_Types.Data_Product_Id := 0;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Modify Filter Factor:");
      Put_Line ("----------------------------------");

      -- Make sure nothing has been called.
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (1), Test_Dp_Received (Packed_Filtered_Dp_Id, 0));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Test_Dp_Received (Packed_Passed_Dp_Id, 0));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Test_Dp_Received (2, 0));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Test_Dp_Received (3, 3));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Test_Dp_Received (4, 1));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (6), Test_Dp_Received (5, 4));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (7), Test_Dp_Received (6, 2));

      -- Start with the one that is filtered Check that it is filtered first.
      Incoming_Packet.Header.Apid := 100;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (8), Test_Dp_Received (Packed_Filtered_Dp_Id, 1));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (9), Test_Dp_Received (Packed_Filtered_Dp_Id, 2));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 10);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (10), Test_Dp_Received (Packed_Filtered_Dp_Id, 3));

      -- Change to unfiltered for all packets
      T.Command_T_Send (T.Commands.Modify_Filter_Factor ((Filter_Factor => 1, Apid => 100)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Modify_Filter_Factor_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Modified_Factor_Filter_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 11);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (11), Test_Dp_Received (2, 1));

      -- Now check that we get messages forwarded
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 12);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (12), Test_Dp_Received (Packed_Passed_Dp_Id, 1));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 13);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (13), Test_Dp_Received (Packed_Passed_Dp_Id, 2));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 14);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (14), Test_Dp_Received (Packed_Passed_Dp_Id, 3));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 15);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (15), Test_Dp_Received (Packed_Passed_Dp_Id, 4));

      -- Change to something that is not 1 or 0 for a filter factor
      T.Command_T_Send (T.Commands.Modify_Filter_Factor ((Filter_Factor => 5, Apid => 100)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Modify_Filter_Factor_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Modified_Factor_Filter_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (16), Test_Dp_Received (2, 5));

      -- Check that the appropriate number come out
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 17);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (17), Test_Dp_Received (Packed_Passed_Dp_Id, 5));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 18);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (18), Test_Dp_Received (Packed_Filtered_Dp_Id, 4));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 19);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (19), Test_Dp_Received (Packed_Filtered_Dp_Id, 5));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 20);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (20), Test_Dp_Received (Packed_Filtered_Dp_Id, 6));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 21);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (21), Test_Dp_Received (Packed_Filtered_Dp_Id, 7));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 22);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (22), Test_Dp_Received (Packed_Passed_Dp_Id, 6));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 23);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (23), Test_Dp_Received (Packed_Filtered_Dp_Id, 8));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 24);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (24), Test_Dp_Received (Packed_Filtered_Dp_Id, 9));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 25);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (25), Test_Dp_Received (Packed_Filtered_Dp_Id, 10));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 26);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (26), Test_Dp_Received (Packed_Filtered_Dp_Id, 11));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 27);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (27), Test_Dp_Received (Packed_Passed_Dp_Id, 7));

      -- Back to all filtered
      T.Command_T_Send (T.Commands.Modify_Filter_Factor ((Filter_Factor => 0, Apid => 100)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Modify_Filter_Factor_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Modified_Factor_Filter_History.Get_Count, 3);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 28);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (28), Test_Dp_Received (2, 0));

      -- Check that it was successful
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 29);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (29), Test_Dp_Received (Packed_Filtered_Dp_Id, 12));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 30);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (30), Test_Dp_Received (Packed_Filtered_Dp_Id, 13));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 31);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (31), Test_Dp_Received (Packed_Filtered_Dp_Id, 14));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 32);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (32), Test_Dp_Received (Packed_Filtered_Dp_Id, 15));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 33);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (33), Test_Dp_Received (Packed_Filtered_Dp_Id, 16));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 34);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (34), Test_Dp_Received (Packed_Filtered_Dp_Id, 17));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 35);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (35), Test_Dp_Received (Packed_Filtered_Dp_Id, 18));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 36);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (36), Test_Dp_Received (Packed_Filtered_Dp_Id, 19));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 37);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (37), Test_Dp_Received (Packed_Filtered_Dp_Id, 20));

      -- Now check that we fail for IDs that are not in the list.
      T.Command_T_Send (T.Commands.Modify_Filter_Factor ((Filter_Factor => 0, Apid => 101)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Modify_Filter_Factor_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Factor_Filter_Change_Failed_Invalid_Apid_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 37);

      T.Command_T_Send (T.Commands.Modify_Filter_Factor ((Filter_Factor => 0, Apid => 501)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Modify_Filter_Factor_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Factor_Filter_Change_Failed_Invalid_Apid_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 37);

   end Test_Modify_Filter_Factor;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Ccsds_Downsampler.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Modify_Filter_Factor ((Filter_Factor => 0, Apid => 1));
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Invalid Command:");
      Put_Line ("----------------------------------");
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Modify_Filter_Factor_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Modify_Filter_Factor_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Ccsds_Downsampler_Tests.Implementation;
