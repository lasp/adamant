--------------------------------------------------------------------------------
-- Sequence_Store Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Basic_Assertions; use Basic_Assertions;
with Sequence_Store_Slot_Header.Assertion; use Sequence_Store_Slot_Header.Assertion;
with Sequence_Store_Slot_Metadata;
with Basic_Types;
with Memory_Region;
with Packet_Types;
with Packed_Slot_Summary;
with Packet.Assertion; use Packet.Assertion;
with System.Storage_Elements; use System.Storage_Elements;
with Sequence_Store_Enums;
with Crc_16;
with Sequence_Header;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Command;
with Packed_Slot_Number.Assertion; use Packed_Slot_Number.Assertion;
with Packed_Sequence_Id.Assertion; use Packed_Sequence_Id.Assertion;
with Packed_Slot_Validity.Assertion; use Packed_Slot_Validity.Assertion;
with Sequence_Store_Memory_Region_Release.Assertion; use Sequence_Store_Memory_Region_Release.Assertion;
with Sequence_Store_Memory_Region_Store.Assertion; use Sequence_Store_Memory_Region_Store.Assertion;
with Slot_Written_Summary.Assertion; use Slot_Written_Summary.Assertion;
with Invalid_Sequence_Crc_Info.Assertion; use Invalid_Sequence_Crc_Info.Assertion;
with Invalid_Sequence_Length_Info.Assertion; use Invalid_Sequence_Length_Info.Assertion;
with Sequence_Store_Memory_Region_Fetch.Assertion; use Sequence_Store_Memory_Region_Fetch.Assertion;
with Interfaces; use Interfaces;
with Test_Slots; use Test_Slots; -- Slot_N_Memory variables declared here.
with Test_Sequence_Store;

package body Sequence_Store_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals:
   -------------------------------------------------------------------------
   -- Slot headers:
   Slot_0_Header : Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Slot_0_Memory'Address;
   Slot_1_Header : Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Slot_1_Memory'Address;
   Slot_2_Header : Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Slot_2_Memory'Address;
   Slot_3_Header : Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Slot_3_Memory'Address;
   Slot_5_Header : Sequence_Store_Slot_Header.T with Import, Convention => Ada, Address => Slot_5_Memory'Address;

      -- Slot memory regions:
   Slot_0_Region : constant Memory_Region.T := (Address => Slot_0_Memory'Address, Length => Slot_0_Memory'Length);
   Slot_1_Region : constant Memory_Region.T := (Address => Slot_1_Memory'Address, Length => Slot_1_Memory'Length);
   Slot_2_Region : constant Memory_Region.T := (Address => Slot_2_Memory'Address, Length => Slot_2_Memory'Length);
   Slot_3_Region : constant Memory_Region.T := (Address => Slot_3_Memory'Address, Length => Slot_3_Memory'Length);
   Slot_Too_Small : constant Memory_Region.T := (Address => Slot_5_Memory'Address, Length => 5);
   Slot_Overlap_1 : constant Memory_Region.T := (Address => Slot_0_Memory'Address - Storage_Offset (5), Length => 80);
   Slot_Overlap_2 : constant Memory_Region.T := (Address => Slot_3_Memory'Address + Storage_Offset (6), Length => 90);

   -- For testing bad initialization conditions:
   Slots_Empty : aliased Component.Sequence_Store.Sequence_Slot_Array := [1 .. 0 => Slot_0_Region];
   Slots_Weird_Index : aliased Component.Sequence_Store.Sequence_Slot_Array := [1 => Slot_0_Region, 2 => Slot_1_Region, 3 => Slot_2_Region, 4 => Slot_3_Region];
   Slots_Too_Small : aliased Component.Sequence_Store.Sequence_Slot_Array := [Slot_0_Region, Slot_Too_Small, Slot_2_Region, Slot_3_Region];
   Slots_Overlap_1 : aliased Component.Sequence_Store.Sequence_Slot_Array := [Slot_0_Region, Slot_1_Region, Slot_Overlap_1, Slot_3_Region];
   Slots_Overlap_2 : aliased Component.Sequence_Store.Sequence_Slot_Array := [Slot_0_Region, Slot_1_Region, Slot_3_Region, Slot_Overlap_2];

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Sequence_Slots => Test_Sequence_Store.Slots_Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Clean up the component:
      Self.Tester.Component_Instance.Final;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helper functions:
   -------------------------------------------------------------------------

   function Create_Summary_Packet (Self : in out Instance; Sequence_Count : in Packet_Types.Sequence_Count_Mod_Type) return Packet.T is
      -- Initialize packet:
      Pkt : Packet.T := (
         Header => (
            Time => Self.Tester.System_Time,
            Id => Self.Tester.Packets.Get_Slot_Summaries_Id,
            Sequence_Count => Sequence_Count,
            Buffer_Length => Packed_Slot_Summary.Size_In_Bytes * Test_Sequence_Store.Slots'Length
         ),
         Buffer => [others => 0]
      );
      Idx : Natural := Pkt.Buffer'First;
   begin
      -- Serialize slot 1 data:
      Pkt.Buffer (Idx .. Idx + Packed_Slot_Summary.Size_In_Bytes - 1) :=
         Packed_Slot_Summary.Serialization.To_Byte_Array ((
            Slot_Info => Slot_0_Header.Slot_Info,
            Id => Slot_0_Header.Seq_Header.Id,
            Sequence_Length => Slot_0_Header.Seq_Header.Length
         ));
      Idx := @ + Packed_Slot_Summary.Size_In_Bytes;

      -- Serialize slot 2 data:
      Pkt.Buffer (Idx .. Idx + Packed_Slot_Summary.Size_In_Bytes - 1) :=
         Packed_Slot_Summary.Serialization.To_Byte_Array ((
            Slot_Info => Slot_1_Header.Slot_Info,
            Id => Slot_1_Header.Seq_Header.Id,
            Sequence_Length => Slot_1_Header.Seq_Header.Length
         ));
      Idx := @ + Packed_Slot_Summary.Size_In_Bytes;

      -- Serialize slot 3 data:
      Pkt.Buffer (Idx .. Idx + Packed_Slot_Summary.Size_In_Bytes - 1) :=
         Packed_Slot_Summary.Serialization.To_Byte_Array ((
            Slot_Info => Slot_2_Header.Slot_Info,
            Id => Slot_2_Header.Seq_Header.Id,
            Sequence_Length => Slot_2_Header.Seq_Header.Length
         ));
      Idx := @ + Packed_Slot_Summary.Size_In_Bytes;

      -- Serialize slot 4 data:
      Pkt.Buffer (Idx .. Idx + Packed_Slot_Summary.Size_In_Bytes - 1) :=
         Packed_Slot_Summary.Serialization.To_Byte_Array ((
            Slot_Info => Slot_3_Header.Slot_Info,
            Id => Slot_3_Header.Seq_Header.Id,
            Sequence_Length => Slot_3_Header.Seq_Header.Length
         ));
      Idx := @ + Packed_Slot_Summary.Size_In_Bytes;

      return Pkt;
   end Create_Summary_Packet;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Initialization (Self : in out Instance) is
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal is
      begin
         -- Call component init here.
         Self.Tester.Component_Instance.Init (Sequence_Slots => Test_Sequence_Store.Slots_Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init failed!");
      end Init_Nominal;

      procedure Init_None is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Sequence_Slots => Slots_Empty'Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
         -- Should never get here:
         Assert (False, "Init none did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_None;

      procedure Init_Weird_Index is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Sequence_Slots => Slots_Weird_Index'Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
         -- Should never get here:
         Assert (False, "Init weird index did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Weird_Index;

      procedure Init_Slot_Too_Small is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Sequence_Slots => Slots_Too_Small'Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
         -- Should never get here:
         Assert (False, "Init slot too small did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Slot_Too_Small;

      procedure Init_Overlap_1 is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Sequence_Slots => Slots_Overlap_1'Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
         -- Should never get here:
         Assert (False, "Init overlap 1 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Overlap_1;

      procedure Init_Overlap_2 is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Sequence_Slots => Slots_Overlap_2'Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
         -- Should never get here:
         Assert (False, "Init overlap 2 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Overlap_2;

      procedure Init_Check_Slots_At_Startup is
         use Sequence_Store_Enums.Slot_State_Type;
         use Sequence_Store_Enums.Slot_Valid_Type;
      begin
         -- Initialize slot headers:
         -- Garbage
         Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 18));
         -- Garbage
         Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 44, Length => 18));
         -- Good
         Slot_2_Header :=
            (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid),
             Seq_Header =>
                (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 18 - 1)), Version => 0,
                  Category => 0, Id => 55, Length => 18));
         -- Set 2 again with proper CRC over initialized header.
         Slot_2_Header :=
            (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid),
             Seq_Header =>
                (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 18 - 1)), Version => 0,
                  Category => 0, Id => 55, Length => 18));
         -- Too long:
         Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 66, Length => 100));
         -- Call init:
         Self.Tester.Component_Instance.Init (Sequence_Slots => Test_Sequence_Store.Slots_Access, Check_Slots_At_Startup => True, Dump_Slot_Summary_At_Startup => True);
         -- Check headers now:
         Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 18)));
         -- Check headers now:
         Sequence_Store_Slot_Header_Assert.Eq (Slot_1_Header, (Slot_Info => (Reserved => 11, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 44, Length => 18)));
         Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Active, Validity => Valid), Seq_Header => (
            Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 18 - 1)), Version => 0,
            Category => 0, Id => 55, Length => 18
         )));
         Sequence_Store_Slot_Header_Assert.Eq (Slot_3_Header, (Slot_Info => (Reserved => 12, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 66, Length => 100)));
      exception
         -- Should never get here:
         when others =>
            Assert (False, "Init check slots produced exception!");
      end Init_Check_Slots_At_Startup;

      procedure Init_Duplicate_Id is
         use Sequence_Store_Enums.Slot_State_Type;
         use Sequence_Store_Enums.Slot_Valid_Type;
      begin
         -- Initialize slot headers:
         Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
         -- Activated
         Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
         -- Should be deactivated
         Slot_2_Header := (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
         Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 23, Length => 100));
         -- Call init:
         Self.Tester.Component_Instance.Init (Sequence_Slots => Test_Sequence_Store.Slots_Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);
         -- Check headers now:
         Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));
         Sequence_Store_Slot_Header_Assert.Eq (Slot_1_Header, (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));
         Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));
         Sequence_Store_Slot_Header_Assert.Eq (Slot_3_Header, (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 23, Length => 100)));
      exception
         -- Should never get here:
         when others =>
            Assert (False, "Init check slots produced exception!");
      end Init_Duplicate_Id;
   begin
      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- Check packet:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      -- Test different start-up scenarios:
      T.Component_Instance.Final;
      Init_Nominal;
      T.Component_Instance.Final;
      Init_None;
      T.Component_Instance.Final;
      Init_Weird_Index;
      T.Component_Instance.Final;
      Init_Slot_Too_Small;
      T.Component_Instance.Final;
      Init_Overlap_1;
      T.Component_Instance.Final;
      Init_Overlap_2;
      T.Component_Instance.Final;
      Init_Check_Slots_At_Startup;
      T.Component_Instance.Final;
      Init_Duplicate_Id;
   end Test_Initialization;

   overriding procedure Test_Dump_Summary (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send command to dump summary:
      T.Command_T_Send (T.Commands.Dump_Summary);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Summary_Id, Status => Success));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      -- Expect two events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);

      -- Set the slot metadata to something different:
      Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
      Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 15, Length => 10));
      Slot_2_Header := (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10));
      Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 23, Length => 100));

      -- Send command to dump summary:
      T.Command_T_Send (T.Commands.Dump_Summary);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Summary_Id, Status => Success));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 2);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (2), Self.Create_Summary_Packet (1));

      -- Expect two events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 2);
   end Test_Dump_Summary;

   overriding procedure Test_Nominal_Activate_Deactivate_Slot (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Set the slot metadata to something:
      Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
      Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 15, Length => 10));
      Slot_2_Header := (Slot_Info => (Reserved => 9, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10));
      Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 23, Length => 100));

      -- Re-initialize so we start fresh:
      Self.Tester.Component_Instance.Final;
      Self.Tester.Component_Instance.Init (Sequence_Slots => Test_Sequence_Store.Slots_Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);

      -- Send command to activate first slot:
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Success));

      -- Expect slot_0 to now be active:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Active, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      -- Expect three events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Activated_Slot_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Activated_Slot_History.Get (1), (Slot => 0));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);

      -- Test activating same slot again.
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Success));

      -- Expect slot_0 to now be active:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Active, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- Do NOT expect a packet to be produced, since nothing changed:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Expect event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Activated_Slot_History.Get_Count, 2);
      Packed_Slot_Number_Assert.Eq (T.Activated_Slot_History.Get (2), (Slot => 0));

      -- Test activating another slot:

      -- First make sure slot 2 unchanged.
      Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10)));
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Success));

      -- Expect slot_2 to now be active:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10)));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 2);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (2), Self.Create_Summary_Packet (1));

      -- Expect three events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Activated_Slot_History.Get_Count, 3);
      Packed_Slot_Number_Assert.Eq (T.Activated_Slot_History.Get (3), (Slot => 2));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 2);

      -- Test deactivating slot
      T.Command_T_Send (T.Commands.Deactivate_Slot ((Slot => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Deactivate_Slot_Id, Status => Success));

      -- Expect slot_2 to now be inactive:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10)));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 3);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (3), Self.Create_Summary_Packet (2));

      -- Expect three events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Deactivated_Slot_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Deactivated_Slot_History.Get (1), (Slot => 2));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 3);

      --
      -- Test deactivating another slot

      -- Expect slot_0 to still be active:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Active, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      T.Command_T_Send (T.Commands.Deactivate_Slot ((Slot => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Deactivate_Slot_Id, Status => Success));

      -- Expect slot_0 to now be inactive:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 4);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (4), Self.Create_Summary_Packet (3));

      -- Expect three events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Deactivated_Slot_History.Get_Count, 2);
      Packed_Slot_Number_Assert.Eq (T.Deactivated_Slot_History.Get (2), (Slot => 0));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 4);

      --
      -- Test deactivating again
      T.Command_T_Send (T.Commands.Deactivate_Slot ((Slot => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Deactivate_Slot_Id, Status => Success));

      -- Expect slot_0 to now be inactive:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- Do not expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Expect one event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 14);
      Natural_Assert.Eq (T.Deactivated_Slot_History.Get_Count, 3);
      Packed_Slot_Number_Assert.Eq (T.Deactivated_Slot_History.Get (3), (Slot => 0));
   end Test_Nominal_Activate_Deactivate_Slot;

   overriding procedure Test_Activate_Deactivate_Slot_Fail (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Set the slot metadata to something:
      Slot_0_Header := (Slot_Info => (Reserved => 22, State => Active, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
      Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 15, Length => 10));
      Slot_2_Header := (Slot_Info => (Reserved => 9, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
      Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 23, Length => 100));

      -- Re-initialize so we start fresh:
      Self.Tester.Component_Instance.Final;
      Self.Tester.Component_Instance.Init (Sequence_Slots => Test_Sequence_Store.Slots_Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);

      -- Let's try to activate a slot that does not exist:
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 4)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Failure));

      -- Make sure appropriate event thrown:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Slot_Number_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Invalid_Slot_Number_History.Get (1), (Slot => 4));

      -- Let's try to deactivate a slot that does not exist:
      T.Command_T_Send (T.Commands.Deactivate_Slot ((Slot => 1_001)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Deactivate_Slot_Id, Status => Failure));

      -- Make sure appropriate event thrown:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Slot_Number_History.Get_Count, 2);
      Packed_Slot_Number_Assert.Eq (T.Invalid_Slot_Number_History.Get (2), (Slot => 1_001));

      -- OK now let's try to activate a duplicate ID in the store:
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Failure));

      -- Make sure appropriate event thrown:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Cannot_Activate_Duplicate_Sequence_Id_History.Get_Count, 1);
      Packed_Sequence_Id_Assert.Eq (T.Cannot_Activate_Duplicate_Sequence_Id_History.Get (1), (Id => 22));

      -- Make sure slot 2 still inactive:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- OK now let's deactivate the first duplicate and try again.
      T.Command_T_Send (T.Commands.Deactivate_Slot ((Slot => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Deactivate_Slot_Id, Status => Success));

      -- Make sure appropriate event thrown:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Deactivated_Slot_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Deactivated_Slot_History.Get (1), (Slot => 0));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);

      -- Make sure slot 0 still inactive:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- OK now we should be able to activate slot 2:
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Success));

      -- Make sure appropriate event thrown:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Packed_Slot_Number_Assert.Eq (T.Activated_Slot_History.Get (1), (Slot => 2));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 2);

      -- Make sure slot 2 active:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- OK now we should fail to activate slot 0.
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Failure));

      -- Make sure appropriate event thrown:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Cannot_Activate_Duplicate_Sequence_Id_History.Get_Count, 2);
      Packed_Sequence_Id_Assert.Eq (T.Cannot_Activate_Duplicate_Sequence_Id_History.Get (2), (Id => 22));

      -- Make sure slot 0 inactive:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));
   end Test_Activate_Deactivate_Slot_Fail;

   overriding procedure Test_Check_Slot (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Initialize slot headers:
      -- Garbage
      Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10));
      -- Garbage
      Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 44, Length => 10));
      -- Good
      Slot_2_Header :=
         (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid),
          Seq_Header =>
             (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + Sequence_Header.Size_In_Bytes + 10 - 1)), Version => 0,
               Category => 0, Id => 55, Length => 10 + Sequence_Header.Size_In_Bytes));
      -- Set 3 again with proper CRC over initialized header.
      Slot_2_Header :=
         (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid),
          Seq_Header =>
             (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + Sequence_Header.Size_In_Bytes + 10 - 1)), Version => 0,
               Category => 0, Id => 55, Length => 10 + Sequence_Header.Size_In_Bytes));
      -- Too long:
      Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 66, Length => 100));

      -- Send command to check first slot:
      T.Command_T_Send (T.Commands.Check_Slot ((Slot => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Check_Slot_Id, Status => Success));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10)));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Checked_Slot_Validity_History.Get_Count, 1);
      Packed_Slot_Validity_Assert.Eq (T.Checked_Slot_Validity_History.Get (1), (Slot => 0, Validity => Invalid));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);

      -- Make sure second slot unchanged:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_1_Header, (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 44, Length => 10)));

      -- Send command to check second slot:
      T.Command_T_Send (T.Commands.Check_Slot ((Slot => 1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Check_Slot_Id, Status => Success));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_1_Header, (Slot_Info => (Reserved => 11, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 44, Length => 10)));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 2);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (2), Self.Create_Summary_Packet (1));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Checked_Slot_Validity_History.Get_Count, 2);
      Packed_Slot_Validity_Assert.Eq (T.Checked_Slot_Validity_History.Get (2), (Slot => 1, Validity => Invalid));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 2);

      -- Send command to check third slot:
      T.Command_T_Send (T.Commands.Check_Slot ((Slot => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Check_Slot_Id, Status => Success));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_2_Header,
          (Slot_Info => (Reserved => 9, State => Active, Validity => Valid),
            Seq_Header =>
               (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + Sequence_Header.Size_In_Bytes + 10 - 1)), Version => 0,
                Category => 0, Id => 55, Length => 10 + Sequence_Header.Size_In_Bytes)));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 3);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (3), Self.Create_Summary_Packet (2));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Checked_Slot_Validity_History.Get_Count, 3);
      Packed_Slot_Validity_Assert.Eq (T.Checked_Slot_Validity_History.Get (3), (Slot => 2, Validity => Valid));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 3);

      -- Send command to check fourth slot:
      T.Command_T_Send (T.Commands.Check_Slot ((Slot => 3)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Check_Slot_Id, Status => Success));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_3_Header, (Slot_Info => (Reserved => 12, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 66, Length => 100)));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 4);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (4), Self.Create_Summary_Packet (3));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Checked_Slot_Validity_History.Get_Count, 4);
      Packed_Slot_Validity_Assert.Eq (T.Checked_Slot_Validity_History.Get (4), (Slot => 3, Validity => Invalid));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 4);

      -- Send command to check nonexistent slot:
      T.Command_T_Send (T.Commands.Check_Slot ((Slot => 4)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Check_Slot_Id, Status => Failure));

      -- No packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Invalid_Slot_Number_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Invalid_Slot_Number_History.Get (1), (Slot => 4));
   end Test_Check_Slot;

   overriding procedure Test_Check_All_Slots (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Initialize slot headers:
      -- Garbage
      Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10));
      -- Garbage
      Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 44, Length => 10));
      -- Good
      Slot_2_Header :=
         (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid),
          Seq_Header =>
             (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + Sequence_Header.Size_In_Bytes + 10 - 1)), Version => 0,
               Category => 0, Id => 55, Length => 10 + Sequence_Header.Size_In_Bytes));
      -- Set 3 again with proper CRC over initialized header.
      Slot_2_Header :=
         (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid),
          Seq_Header =>
             (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + Sequence_Header.Size_In_Bytes + 10 - 1)), Version => 0,
               Category => 0, Id => 55, Length => 10 + Sequence_Header.Size_In_Bytes));
      -- Too long:
      Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 66, Length => 100));

      -- Send command to check all slots:
      T.Command_T_Send (T.Commands.Check_All_Slots);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Check_All_Slots_Id, Status => Success));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10)));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_1_Header, (Slot_Info => (Reserved => 11, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 44, Length => 10)));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_2_Header,
          (Slot_Info => (Reserved => 9, State => Active, Validity => Valid),
            Seq_Header =>
               (Crc => Crc_16.Compute_Crc_16 (Slot_2_Memory (Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + 2 .. Slot_2_Memory'First + Sequence_Store_Slot_Metadata.Size_In_Bytes + Sequence_Header.Size_In_Bytes + 10 - 1)), Version => 0,
                Category => 0, Id => 55, Length => 10 + Sequence_Header.Size_In_Bytes)));

      -- Check slot:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_3_Header, (Slot_Info => (Reserved => 12, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 66, Length => 100)));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Checking_All_Slot_Validity_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);
   end Test_Check_All_Slots;

   overriding procedure Test_Nominal_Write_Sequence (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      use Sequence_Store_Enums.Sequence_Store_Status;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;

      -- Create a sequence to send:
      Sequence_Source_Memory : Basic_Types.Byte_Array (0 .. 999) := [others => 88];
      -- Sequence header:
      Seq_Header : Sequence_Header.T with
         Import,
         Convention => Ada,
         Address => Sequence_Source_Memory'Address;
         -- Sequence memory region:
      Seq_Region : Memory_Region.T := (Address => Sequence_Source_Memory'Address, Length => 0);
   begin
      -- Initialize slot headers:
      Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10 + Sequence_Header.Size_In_Bytes));

      -- OK let's set the sequence header such that it looks like a valid
      -- sequence:
      Seq_Header := (Crc => [0, 0], Version => 0, Category => 0, Id => 77, Length => 10 + Sequence_Header.Size_In_Bytes);
      Seq_Header.Crc := Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + Sequence_Header.Size_In_Bytes + 10 - 1));

      -- Set the slot region length correctly:
      Seq_Region.Length := 10 + Sequence_Header.Size_In_Bytes;

      -- OK let's send the region to the component to store on slot 0:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 0, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Sequence_Region => Seq_Region, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 1);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (1), (Slot => 0, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Wrote_Sequence_To_Slot_History.Get_Count, 1);
      Slot_Written_Summary_Assert.Eq (T.Wrote_Sequence_To_Slot_History.Get (1), (Store => (Slot => 0, Sequence_Region => Seq_Region), Header => (Slot_Info => (Reserved => 22, State => Inactive, Validity => Valid), Seq_Header => Seq_Header)));

      -- Check slot header
      Sequence_Store_Slot_Header_Assert.Eq (Slot_0_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Valid), Seq_Header => Seq_Header));

      -- Check slot data:
      Byte_Array_Assert.Eq
         (Slot_0_Memory (Sequence_Store_Slot_Metadata.Size_In_Bytes .. Sequence_Store_Slot_Metadata.Size_In_Bytes + Seq_Header.Length - 1), Sequence_Source_Memory (0 .. Seq_Header.Length - 1));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      --
      -- OK let's do this one more time
      --

      -- Initialize slot headers:
      Slot_3_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 70 - Sequence_Store_Slot_Header.Size_In_Bytes + Sequence_Header.Size_In_Bytes));

      -- OK let's set the sequence header such that it looks like a valid
      -- sequence:
      Seq_Header :=
         (Crc =>
             [0, 0], Version => 14, Category => 3, Id => 1_008, Length => 70 - Sequence_Store_Slot_Header.Size_In_Bytes -- maximum size that should fit
      );
      Seq_Header.Crc := Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + (70 - Sequence_Store_Slot_Header.Size_In_Bytes) - 1));

      -- Set the slot region length correctly:
      Seq_Region.Length := (70 - Sequence_Store_Slot_Header.Size_In_Bytes);

      -- OK let's send the region to the component to store on slot 0:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 3, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (2), (Sequence_Region => Seq_Region, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 2);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 2);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (2), (Slot => 3, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Wrote_Sequence_To_Slot_History.Get_Count, 2);
      Slot_Written_Summary_Assert.Eq (T.Wrote_Sequence_To_Slot_History.Get (2), (Store => (Slot => 3, Sequence_Region => Seq_Region), Header => (Slot_Info => (Reserved => 22, State => Inactive, Validity => Valid), Seq_Header => Seq_Header)));

      -- Check slot header
      Sequence_Store_Slot_Header_Assert.Eq (Slot_3_Header, (Slot_Info => (Reserved => 22, State => Inactive, Validity => Valid), Seq_Header => Seq_Header));

      -- Check slot data:
      Byte_Array_Assert.Eq
         (Slot_3_Memory (Sequence_Store_Slot_Metadata.Size_In_Bytes .. Sequence_Store_Slot_Metadata.Size_In_Bytes + Seq_Header.Length - 1), Sequence_Source_Memory (0 .. Seq_Header.Length - 1));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 2);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (2), Self.Create_Summary_Packet (1));
   end Test_Nominal_Write_Sequence;

   overriding procedure Test_Write_Sequence_Fail (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      use Sequence_Store_Enums.Sequence_Store_Status;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;

      -- Create a sequence to send:
      Sequence_Source_Memory : Basic_Types.Byte_Array (0 .. 999) := [others => 88];
      -- Sequence header:
      Seq_Header : Sequence_Header.T with
         Import,
         Convention => Ada,
         Address => Sequence_Source_Memory'Address;
         -- Sequence memory region:
      Seq_Region : Memory_Region.T := (Address => Sequence_Source_Memory'Address, Length => 0);
   begin
      -- OK let's set the sequence header such that it looks like a valid
      -- sequence:
      Seq_Header := (Crc => [0, 0], Version => 0, Category => 0, Id => 77, Length => 10 + Sequence_Header.Size_In_Bytes);
      Seq_Header.Crc := Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + Sequence_Header.Size_In_Bytes + 10 - 1));

      -- Set the slot region length correctly:
      Seq_Region.Length := 10 + Sequence_Header.Size_In_Bytes;

      -- OK, first let's try to send a sequence with an invalid slot number:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 4, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Sequence_Region => Seq_Region, Status => Invalid_Slot_Number));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 1);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (1), (Slot => 4, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Slot_Number_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Invalid_Slot_Number_History.Get (1), (Slot => 4));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Again
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 4_002, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (2), (Sequence_Region => Seq_Region, Status => Invalid_Slot_Number));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 2);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (2), (Slot => 4_002, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Slot_Number_History.Get_Count, 2);
      Packed_Slot_Number_Assert.Eq (T.Invalid_Slot_Number_History.Get (2), (Slot => 4_002));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      --
      -- Ok, now let's test writing to a slot that is currently in use.
      --

      -- Initialize slot header:
      Slot_2_Header :=
         (Slot_Info =>
             (Reserved => 22,
               State => Active, -- in use
               Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 12));

      -- OK, first let's try to send a sequence with an:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (3), (Sequence_Region => Seq_Region, Status => Slot_In_Use));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 3);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (3), (Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Sequence_Slot_Active_History.Get_Count, 1);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Sequence_Slot_Active_History.Get (1), (Slot => 2, Sequence_Region => Seq_Region));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_2_Header,
          (Slot_Info =>
               (Reserved => 22,
                State => Active, -- in use
                Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 12)));

      -- Again

      -- Initialize slot header:
      Slot_1_Header :=
         (Slot_Info =>
             (Reserved => 11,
               State => Active, -- in use
               Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12));

      -- OK, first let's try to send a sequence:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 4);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (4), (Sequence_Region => Seq_Region, Status => Slot_In_Use));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 4);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (4), (Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Sequence_Slot_Active_History.Get_Count, 2);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Sequence_Slot_Active_History.Get (2), (Slot => 1, Sequence_Region => Seq_Region));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_1_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Active, -- in use
                Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12)));

      --
      -- OK, now let's test a CRC error.
      --

      -- Initialize slot header:
      Slot_1_Header :=
         (Slot_Info =>
             (Reserved => 11,
               State => Inactive, -- not in use
               Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12));

      -- Corrupt CRC
      Seq_Header.Crc (1) := @ + 1;

      -- OK, first let's try to send a sequence:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 5);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (5), (Sequence_Region => Seq_Region, Status => Crc_Error));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 5);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (5), (Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Sequence_Crc_History.Get_Count, 1);
      Invalid_Sequence_Crc_Info_Assert.Eq
         (T.Invalid_Sequence_Crc_History.Get (1),
          (Store => (Slot => 1, Sequence_Region => Seq_Region), Header => Seq_Header, Computed_Crc => Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + Sequence_Header.Size_In_Bytes + 10 - 1))));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_1_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Inactive, -- not in use
                Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12)));

      -- Again
      -- Fix CRC
      Seq_Header.Crc (1) := @ - 1;
      -- Set length to bad, this will show up as a CRC error.
      Seq_Header.Length := @ + 1;
      Seq_Region.Length := @ + 1;

      -- OK, first let's try to send a sequence :
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 6);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (6), (Sequence_Region => Seq_Region, Status => Crc_Error));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 6);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (6), (Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Sequence_Crc_History.Get_Count, 2);
      Invalid_Sequence_Crc_Info_Assert.Eq
         (T.Invalid_Sequence_Crc_History.Get (2),
          (Store => (Slot => 1, Sequence_Region => Seq_Region), Header => Seq_Header, Computed_Crc => Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + Sequence_Header.Size_In_Bytes + 11 - 1))));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_1_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Inactive, -- not in use
                Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12)));

      --
      -- OK, now we need to check for length error handling
      --

      -- Reset lengths:
      Seq_Header.Length := @ - 1;
      Seq_Region.Length := @ - 1;

      -- OK, now if the region is too small to hold the entire sequence we
      -- should get an error thrown. So let's make the region too small
      -- by a single byte.
      Seq_Region.Length := @ - 1;

      -- OK, first let's try to send a sequence:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 7);
      Sequence_Store_Memory_Region_Release_Assert.Eq
         (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get
             (7),
          (Sequence_Region =>
               Seq_Region, Status =>
               Crc_Error -- A CRC can not be computed in this case, so it shows up as a CRC error
      ));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 14);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 7);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (7), (Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Sequence_Crc_History.Get_Count, 3);
      Invalid_Sequence_Crc_Info_Assert.Eq (T.Invalid_Sequence_Crc_History.Get (3), (Store => (Slot => 1, Sequence_Region => Seq_Region), Header => Seq_Header, Computed_Crc => [0, 0]));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_1_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Inactive, -- not in use
                Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12)));

      -- OK, now set the sequence region so small that it cannot hold a header.
      Seq_Region.Length := 2;

      -- OK, first let's try to send a sequence:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 8);
      Sequence_Store_Memory_Region_Release_Assert.Eq
         (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get
             (8),
          (Sequence_Region =>
               Seq_Region, Status =>
               Crc_Error -- A CRC can not be computed in this case, so it shows up as a CRC error
      ));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 16);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 8);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (8), (Slot => 1, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Sequence_Crc_History.Get_Count, 4);
      Invalid_Sequence_Crc_Info_Assert.Eq (T.Invalid_Sequence_Crc_History.Get (4), (Store => (Slot => 1, Sequence_Region => Seq_Region), Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 0), Computed_Crc => [0, 0]));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_1_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Inactive, -- not in use
                Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12)));

      --
      -- Ok now we test a different class of length errors, where
      -- we send a sequence that is too long to fit in slot
      --

      -- Initialize slot header:
      Slot_2_Header :=
         (Slot_Info =>
             (Reserved => 11,
               State => Inactive, -- not in use
               Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12));

      Seq_Header :=
         (Crc =>
             [0, 0], Version => 0, Category => 0, Id => 77, Length => Slot_2_Memory'Length - Sequence_Store_Slot_Header.Size_In_Bytes + Sequence_Header.Size_In_Bytes + 1 -- one byte too large to fit
      );
      Seq_Header.Crc := Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + Seq_Header.Length - 1));

      -- Set the slot region length correctly:
      Seq_Region.Length := Seq_Header.Length;

      -- OK, first let's try to send a sequence:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 9);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (9), (Sequence_Region => Seq_Region, Status => Length_Error));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 18);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 9);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (9), (Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Sequence_Length_History.Get_Count, 1);
      Invalid_Sequence_Length_Info_Assert.Eq (T.Invalid_Sequence_Length_History.Get (1), (Store => (Slot => 2, Sequence_Region => Seq_Region), Header => Seq_Header, Length_Bound => 50));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_2_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Inactive, -- not in use
                Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12)));

      -- Again with larger length
      Seq_Header :=
         (Crc =>
             [0, 0], Version =>
             0, Category =>
             0, Id =>
             77, Length =>
             101 -- one byte too large to fit
      );
      Seq_Header.Crc := Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + Seq_Header.Length - 1));

      -- Set the slot region length correctly:
      Seq_Region.Length := Seq_Header.Length;

      -- OK, first let's try to send a sequence:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 10);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (10), (Sequence_Region => Seq_Region, Status => Length_Error));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 20);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 10);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (10), (Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Invalid_Sequence_Length_History.Get_Count, 2);
      Invalid_Sequence_Length_Info_Assert.Eq (T.Invalid_Sequence_Length_History.Get (2), (Store => (Slot => 2, Sequence_Region => Seq_Region), Header => Seq_Header, Length_Bound => 50));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure slot header unchanged:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_2_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Inactive, -- not in use
                Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 54, Length => 12)));

      -- Test where region is larger than slot, but sequence is small enough
      -- that it will still fit, this should work fine.

      Seq_Header :=
         (Crc =>
             [0, 0], Version =>
             0, Category =>
             0, Id =>
             77, Length => 10 + Sequence_Header.Size_In_Bytes -- one byte too large to fit
      );
      Seq_Header.Crc := Crc_16.Compute_Crc_16 (Sequence_Source_Memory (Sequence_Source_Memory'First + 2 .. Sequence_Source_Memory'First + Seq_Header.Length - 1));

      -- Set the slot region to very large, much larger than sequence and slot.
      Seq_Region.Length := 1_000;

      -- OK, first let's try to send a sequence:
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check release:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 11);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (11), (Sequence_Region => Seq_Region, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 24);
      Natural_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get_Count, 11);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Writing_Sequence_To_Slot_History.Get (11), (Slot => 2, Sequence_Region => Seq_Region));
      Natural_Assert.Eq (T.Wrote_Sequence_To_Slot_History.Get_Count, 1);
      Slot_Written_Summary_Assert.Eq (T.Wrote_Sequence_To_Slot_History.Get (1), (Store => (Slot => 2, Sequence_Region => Seq_Region), Header => (Slot_Info => (Reserved => 11, State => Inactive, Validity => Valid), Seq_Header => Seq_Header)));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      -- Make sure slot header changed:
      Sequence_Store_Slot_Header_Assert.Eq
         (Slot_2_Header,
          (Slot_Info =>
               (Reserved => 11,
                State => Inactive, -- not in use
                Validity => Valid), Seq_Header => Seq_Header));
   end Test_Write_Sequence_Fail;

   overriding procedure Test_Sequence_Fetch (Self : in out Instance) is
      use Sequence_Store_Enums.Slot_State_Type;
      use Sequence_Store_Enums.Slot_Valid_Type;
      use Sequence_Store_Enums.Sequence_Fetch_Status;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Set the slot metadata to something:
      Slot_0_Header := (Slot_Info => (Reserved => 22, State => Inactive, Validity => Unchecked), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
      Slot_1_Header := (Slot_Info => (Reserved => 11, State => Active, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10));
      Slot_2_Header := (Slot_Info => (Reserved => 9, State => Inactive, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10));
      Slot_3_Header := (Slot_Info => (Reserved => 12, State => Active, Validity => Valid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 23, Length => 100));

      -- Re-initialize so we start fresh:
      Self.Tester.Component_Instance.Final;
      Self.Tester.Component_Instance.Init (Sequence_Slots => Test_Sequence_Store.Slots_Access, Check_Slots_At_Startup => False, Dump_Slot_Summary_At_Startup => True);

      -- OK let's get a valid sequence by id:
      Sequence_Store_Memory_Region_Fetch_Assert.Eq
         (T.Sequence_Store_Memory_Region_Fetch_T_Request ((Id => 23)), (Sequence_Region => (Address => Slot_3_Memory'Address + Storage_Offset (Sequence_Store_Slot_Metadata.Size_In_Bytes), Length => 100), Status => Success));

      -- OK let's get a valid sequence by id which has an inactive duplicate:
      Sequence_Store_Memory_Region_Fetch_Assert.Eq
         (T.Sequence_Store_Memory_Region_Fetch_T_Request ((Id => 22)), (Sequence_Region => (Address => Slot_1_Memory'Address + Storage_Offset (Sequence_Store_Slot_Metadata.Size_In_Bytes), Length => 10), Status => Success));

      -- OK let's get a sequence that is inactive:
      Sequence_Store_Memory_Region_Fetch_Assert.Eq (T.Sequence_Store_Memory_Region_Fetch_T_Request ((Id => 33)), (Sequence_Region => (Address => System.Null_Address, Length => 0), Status => Id_Not_Found));

      -- OK let's get a sequence that is not in store:
      Sequence_Store_Memory_Region_Fetch_Assert.Eq (T.Sequence_Store_Memory_Region_Fetch_T_Request ((Id => 99)), (Sequence_Region => (Address => System.Null_Address, Length => 0), Status => Id_Not_Found));

      -- Send command to activate slot:
      T.Command_T_Send (T.Commands.Activate_Slot ((Slot => 2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Success));

      -- Expect slot_2 to now be active:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_2_Header, (Slot_Info => (Reserved => 9, State => Active, Validity => Invalid), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 33, Length => 10)));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 1);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (1), Self.Create_Summary_Packet (0));

      -- Expect three events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Activated_Slot_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Activated_Slot_History.Get (1), (Slot => 2));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 1);

      -- OK fetch 33 again
      Sequence_Store_Memory_Region_Fetch_Assert.Eq
         (T.Sequence_Store_Memory_Region_Fetch_T_Request ((Id => 33)), (Sequence_Region => (Address => Slot_2_Memory'Address + Storage_Offset (Sequence_Store_Slot_Metadata.Size_In_Bytes), Length => 10), Status => Success));

      -- Deactivating slot
      T.Command_T_Send (T.Commands.Deactivate_Slot ((Slot => 1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Deactivate_Slot_Id, Status => Success));

      -- Expect slot_1 to now be inactive:
      Sequence_Store_Slot_Header_Assert.Eq (Slot_1_Header, (Slot_Info => (Reserved => 11, State => Inactive, Validity => Undefined), Seq_Header => (Crc => [0, 0], Version => 0, Category => 0, Id => 22, Length => 10)));

      -- Expect a packet to be produced:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Slot_Summaries_History.Get_Count, 2);
      Packet_Assert.Eq (T.Slot_Summaries_History.Get (2), Self.Create_Summary_Packet (1));

      -- Expect three events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Deactivated_Slot_History.Get_Count, 1);
      Packed_Slot_Number_Assert.Eq (T.Deactivated_Slot_History.Get (1), (Slot => 1));
      Natural_Assert.Eq (T.Dumping_Slot_Summary_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumped_Slot_Summary_History.Get_Count, 2);

      -- OK fetch 22 again and expect fail.
      Sequence_Store_Memory_Region_Fetch_Assert.Eq (T.Sequence_Store_Memory_Region_Fetch_T_Request ((Id => 22)), (Sequence_Region => (Address => System.Null_Address, Length => 0), Status => Id_Not_Found));
   end Test_Sequence_Fetch;

   overriding procedure Test_Full_Queue (Self : in out Instance) is
      use Sequence_Store_Enums.Sequence_Store_Status;
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Seq_Region : constant Memory_Region.T := (Address => Slot_0_Memory'Address, Length => 0);
   begin
      -- Send 3 commands to fill up queue.
      Cmd.Header.Arg_Buffer_Length := Cmd.Arg_Buffer'Length;
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);

      -- OK the next command should overflow the queue.
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (Cmd);

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_Dropped_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Dropped_History.Get (1), Cmd.Header);

      -- OK, first let's try to send a sequence:
      T.Expect_Sequence_Store_Memory_Region_Store_T_Send_Dropped := True;
      T.Sequence_Store_Memory_Region_Store_T_Send ((Slot => 2, Sequence_Region => Seq_Region));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Region_Store_Dropped_History.Get_Count, 1);
      Sequence_Store_Memory_Region_Store_Assert.Eq (T.Region_Store_Dropped_History.Get (1), (Slot => 2, Sequence_Region => Seq_Region));

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Store_Memory_Region_Release_Assert.Eq (T.Sequence_Store_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Sequence_Region => Seq_Region, Status => Dropped));
   end Test_Full_Queue;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Sequence_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Activate_Slot ((Slot => 0));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Activate_Slot_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Activate_Slot_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Sequence_Store_Tests.Implementation;
