--------------------------------------------------------------------------------
-- Ccsds_Product_Extractor Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Ccsds_Space_Packet;
with Test_Products;
with Data_Product.Assertion; use Data_Product.Assertion;
with Data_Product_Types;
with Packed_U32;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Packed_U16;
with Packed_Byte;
with Packed_Natural;
with Interfaces; use Interfaces;
with Sys_Time;

package body Ccsds_Product_Extractor_Tests.Implementation is

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
      Self.Tester.Component_Instance.Init (Data_Product_Extraction_List => Test_Products.Data_Product_Extraction_List'Access);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -- Helper function to create data product definitions
   function Test_Dp_Received_Natural (Id : in Data_Product_Types.Data_Product_Id; Product_Value : in Natural; Timestamp : in Sys_Time.T) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Id => Id, Time => Timestamp, Buffer_Length => Packed_Natural.Serialization.Serialized_Length), Buffer => [others => 16#FF#]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_Natural.Serialization.Serialized_Length - 1) := Packed_Natural.Serialization.To_Byte_Array ((Value => Product_Value));
      return Dp;
   end Test_Dp_Received_Natural;

   function Test_Dp_Received_Natural_Le (Id : in Data_Product_Types.Data_Product_Id; Product_Value : in Natural; Timestamp : in Sys_Time.T) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Id => Id, Time => Timestamp, Buffer_Length => Packed_Natural.Serialization_Le.Serialized_Length), Buffer => [others => 16#FF#]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_Natural.Serialization_Le.Serialized_Length - 1) := Packed_Natural.Serialization_Le.To_Byte_Array ((Value => Product_Value));
      return Dp;
   end Test_Dp_Received_Natural_Le;

   function Test_Dp_Received_U32 (Id : in Data_Product_Types.Data_Product_Id; Product_Value : in Unsigned_32; Timestamp : in Sys_Time.T) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Id => Id, Time => Timestamp, Buffer_Length => Packed_U32.Serialization.Serialized_Length), Buffer => [others => 16#FF#]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_U32.Serialization.Serialized_Length - 1) := Packed_U32.Serialization.To_Byte_Array ((Value => Product_Value));
      return Dp;
   end Test_Dp_Received_U32;

   function Test_Dp_Received_U16 (Id : in Data_Product_Types.Data_Product_Id; Product_Value : in Unsigned_16; Timestamp : in Sys_Time.T) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Id => Id, Time => Timestamp, Buffer_Length => Packed_U16.Serialization.Serialized_Length), Buffer => [others => 16#FF#]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_U16.Serialization.Serialized_Length - 1) := Packed_U16.Serialization.To_Byte_Array ((Value => Product_Value));
      return Dp;
   end Test_Dp_Received_U16;

   function Test_Dp_Received_U8 (Id : in Data_Product_Types.Data_Product_Id; Product_Value : in Unsigned_8; Timestamp : in Sys_Time.T) return Data_Product.T is
      Dp : Data_Product.T := (Header => (Id => Id, Time => Timestamp, Buffer_Length => Packed_Byte.Serialization.Serialized_Length), Buffer => [others => 16#FF#]);
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Packed_Byte.Serialization.Serialized_Length - 1) := Packed_Byte.Serialization.To_Byte_Array ((Value => Product_Value));
      return Dp;
   end Test_Dp_Received_U8;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Received_Data_Product_Packet (Self : in out Instance) is
      T : Component.Ccsds_Product_Extractor.Implementation.Tester.Instance_Access renames Self.Tester;
      Incoming_Packet_Apid_100 : Ccsds_Space_Packet.T;
      Incoming_Packet_Apid_200 : Ccsds_Space_Packet.T;
      Incoming_Packet_Apid_300 : Ccsds_Space_Packet.T;
      Incoming_Packet_Apid_400 : Ccsds_Space_Packet.T;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Product Extractor Packets:");
      Put_Line ("----------------------------------");

      -- Check that we don't have any events or data products sent yet
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Setup some basic packet info. Start with 0 values just to make sure things are working first.
      Incoming_Packet_Apid_100.Header.Apid := 100;
      Incoming_Packet_Apid_100.Header.Packet_Length := 50;
      Incoming_Packet_Apid_100.Data := [others => 0];

      Incoming_Packet_Apid_200.Header.Apid := 200;
      Incoming_Packet_Apid_200.Header.Packet_Length := 30;
      Incoming_Packet_Apid_200.Data := [others => 0];

      Incoming_Packet_Apid_300.Header.Apid := 300;
      Incoming_Packet_Apid_300.Header.Packet_Length := 20;
      Incoming_Packet_Apid_300.Data := [others => 0];

      Incoming_Packet_Apid_400.Header.Apid := 400;
      Incoming_Packet_Apid_400.Header.Packet_Length := 5;
      Incoming_Packet_Apid_400.Data := [others => 0];

      -- First time through, make sure the data products are 0
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_100);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (1), Test_Dp_Received_U16 (0, 0, (0, 0)));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (2), Test_Dp_Received_U8 (1, 0, (0, 0)));

      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_200);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (3), Test_Dp_Received_U32 (2, 0, (0, 0)));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (4), Test_Dp_Received_Natural (3, 0, (0, 0)));

      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_300);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (5), Test_Dp_Received_Natural_Le (4, 0, (0, 0)));

      -- Should not get anything here
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_400);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);

      -- Now test with some data
      -- Apid 100 test
      Incoming_Packet_Apid_100.Data (11) := 20;
      Incoming_Packet_Apid_100.Data (16) := 50;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_100);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (6), Test_Dp_Received_U16 (0, 20, (0, 0)));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (7), Test_Dp_Received_U8 (1, 50, (0, 0)));

      -- Apid 200 test
      Incoming_Packet_Apid_200.Data (11) := 90;
      Incoming_Packet_Apid_200.Data (23) := 10;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_200);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (8), Test_Dp_Received_U32 (2, 90, (0, 0)));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (9), Test_Dp_Received_Natural (3, 10, (0, 0)));

      -- Apid 300 test
      Incoming_Packet_Apid_300.Data (15) := 16#67#;
      Incoming_Packet_Apid_300.Data (16) := 16#45#;
      Incoming_Packet_Apid_300.Data (17) := 16#23#;
      Incoming_Packet_Apid_300.Data (18) := 16#01#;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_300);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 10);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (10), Test_Dp_Received_Natural_Le (4, 16#0123_4567#, (0, 0)));

      -- Now change the length of the packet and check for failures
      Incoming_Packet_Apid_100.Data (11) := 30;
      Incoming_Packet_Apid_100.Header.Packet_Length := 8;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_100);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Extracted_Product_Length_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (1).Apid), 100);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (1).Id), 0);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (1).Length), 8);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (2).Apid), 100);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (2).Id), 1);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (2).Length), 8);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 10);

      -- Now try to induce a data validation error
      Incoming_Packet_Apid_200.Data (11) := 60;
      Incoming_Packet_Apid_200.Data (20 .. 23) := Packed_U32.Serialization.To_Byte_Array ((Value => Unsigned_32 (Natural'Last) + 1));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_200);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Extracted_Product_Data_History.Get_Count, 1);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Data_History.Get (1).Id), 3);
      Unsigned_32_Assert.Eq (T.Invalid_Extracted_Product_Data_History.Get (1).Errant_Field_Number, 1);
      Packed_U32_Assert.Eq (Packed_U32.Serialization.From_Byte_Array (T.Invalid_Extracted_Product_Data_History.Get (1).Errant_Field (4 .. 7)), ((Value => Unsigned_32 (Natural'Last) + 1)));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 11);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (11), Test_Dp_Received_U32 (2, 60, (0, 0)));

      -- Same for the little endian case
      Incoming_Packet_Apid_300.Data (15 .. 18) := Packed_U32.Serialization_Le.To_Byte_Array ((Value => Unsigned_32 (Natural'Last) + 1));
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_300);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Extracted_Product_Data_History.Get_Count, 2);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Data_History.Get (2).Id), 4);
      Unsigned_32_Assert.Eq (T.Invalid_Extracted_Product_Data_History.Get (2).Errant_Field_Number, 1);
      Packed_U32_Le_Assert.Eq (Packed_U32.Serialization_Le.From_Byte_Array (T.Invalid_Extracted_Product_Data_History.Get (2).Errant_Field (4 .. 7)), ((Value => Unsigned_32 (Natural'Last) + 1)));

      -- Check that we can set the time in the packet and receive it through the correct product
      Incoming_Packet_Apid_100.Header.Packet_Length := 16;
      Incoming_Packet_Apid_100.Data (0 .. Sys_Time.Size_In_Bytes - 1) := Sys_Time.Serialization.To_Byte_Array ((50, 100));
      Incoming_Packet_Apid_100.Data (11) := 30;
      Incoming_Packet_Apid_100.Data (16) := 40;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_100);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 13);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (12), Test_Dp_Received_U16 (0, 30, (50, 100)));
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (13), Test_Dp_Received_U8 (1, 40, (0, 0)));

      -- Lastly, check the very end of a packet offset
      Incoming_Packet_Apid_100.Header.Packet_Length := 15;
      Incoming_Packet_Apid_100.Data (0 .. Sys_Time.Size_In_Bytes - 1) := Sys_Time.Serialization.To_Byte_Array ((50, 100));
      Incoming_Packet_Apid_100.Data (11) := 25;
      Incoming_Packet_Apid_100.Data (15) := 10;
      T.Ccsds_Space_Packet_T_Send (Incoming_Packet_Apid_100);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Invalid_Extracted_Product_Length_History.Get_Count, 3);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (3).Apid), 100);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (3).Id), 1);
      Natural_Assert.Eq (Natural (T.Invalid_Extracted_Product_Length_History.Get (3).Length), 15);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 14);
      Data_Product_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get (14), Test_Dp_Received_U16 (0, 25, (50, 100)));

   end Test_Received_Data_Product_Packet;

end Ccsds_Product_Extractor_Tests.Implementation;
