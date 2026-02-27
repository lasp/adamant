--------------------------------------------------------------------------------
-- Ccsds_Router Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Test_Assembly_Ccsds_Router_Table;
with Ccsds_Space_Packet.Assertion; use Ccsds_Space_Packet.Assertion;
with Ccsds_Primary_Header.Assertion; use Ccsds_Primary_Header.Assertion;
use Ccsds_Primary_Header;
with Unexpected_Sequence_Count.Assertion; use Unexpected_Sequence_Count.Assertion;
with Interfaces; use Interfaces;
with Ccsds_Router_Types;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;
with Ccsds_Enums; use Ccsds_Enums;

package body Ccsds_Router_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3, Ccsds_Space_Packet_T_Send_Count => 6);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Table => Test_Assembly_Ccsds_Router_Table.Router_Table);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Insert custom cleanup code here.
      Self.Tester.Component_Instance.Final;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Test packets:
   -------------------------------------------------------------------------

   Packet_0 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (0),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 0
      ),
      Data => [others => 0]
   );
   Packet_1 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (1),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 6
      ),
      Data => [1, 2, 3, 4, 5, 6, 7, others => 0]
   );
   Packet_2 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (2),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (12),
         Packet_Length => Ccsds_Space_Packet.Ccsds_Data_Type'Length - 1
      ),
      Data => [others => 99]
   );
   Packet_3 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (3),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (1),
         Packet_Length => 17
      ),
      Data => [1, 2, 3, 4, 5, 6, 7, others => 255]
   );
   Packet_4 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (4),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 4
      ),
      Data => [others => 4]
   );
   Packet_5 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (5),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 5
      ),
      Data => [others => 5]
   );
   Packet_6 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (6),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 6
      ),
      Data => [others => 6]
   );
   Packet_7 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (7),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 7
      ),
      Data => [others => 7]
   );
   Packet_8 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (8),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 8
      ),
      Data => [others => 8]
   );
   Packet_9 : Ccsds_Space_Packet.T := (
      Header => (
         Version => 0,
         Packet_Type => Ccsds_Packet_Type.Telecommand,
         Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => Ccsds_Apid_Type (9),
         Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
         Sequence_Count => Ccsds_Sequence_Count_Type (53),
         Packet_Length => 9
      ),
      Data => [others => 9]
   );

   procedure Check_Routing
      (Self : in out Instance; Count_1 : in Natural; Count_2 : in Natural; Count_3 : in Natural; Count_4 : in Natural; Count_5 : in Natural; Count_6 : in Natural; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line)
   is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get_Count, Count_1, "", Filename, Line);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_2_History.Get_Count, Count_2, "", Filename, Line);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_3_History.Get_Count, Count_3, "", Filename, Line);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_4_History.Get_Count, Count_4, "", Filename, Line);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_5_History.Get_Count, Count_5, "", Filename, Line);
      Natural_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_6_History.Get_Count, Count_6, "", Filename, Line);
   end Check_Routing;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Initialization (Self : in out Instance) is
      use Ccsds_Router_Types;
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal is
         -- Router table entries:
         Router_Table : constant Router_Table_Entry_Array := [
            -- Table entry for APID: 1
            0 => (Apid => 1, Destinations => null, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 2
            1 => (Apid => 2, Destinations => null, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 3
            2 => (Apid => 3, Destinations => null, Sequence_Count_Mode => Warn)
         ];
      begin
         T.Component_Instance.Init (Router_Table);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init failed!");
      end Init_Nominal;

      procedure Init_Index_Out_Of_Range_1 is
         Destination_Table_1 : aliased Destination_Table := [0 => 100];
         -- Router table entries:
         Router_Table : constant Router_Table_Entry_Array := [
            -- Table entry for APID: 1
            0 => (Apid => 1, Destinations => Destination_Table_1'Unchecked_Access, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 2
            1 => (Apid => 2, Destinations => null, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 3
            2 => (Apid => 3, Destinations => null, Sequence_Count_Mode => Warn)
         ];
      begin
         T.Component_Instance.Init (Router_Table);
         -- Should never get here:
         Assert (False, "Index out of range did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Index_Out_Of_Range_1;

      procedure Init_Index_Out_Of_Range_2 is
         Destination_Table_1 : aliased Destination_Table := [0 => 7];
         -- Router table entries:
         Router_Table : constant Router_Table_Entry_Array := [
            -- Table entry for APID: 1
            0 => (Apid => 1, Destinations => null, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 2
            1 => (Apid => 2, Destinations => Destination_Table_1'Unchecked_Access, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 3
            2 => (Apid => 3, Destinations => null, Sequence_Count_Mode => Warn)
         ];
      begin
         T.Component_Instance.Init (Router_Table);
         -- Should never get here:
         Assert (False, "Index out of range did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Index_Out_Of_Range_2;

      procedure Init_Duplicate is
         -- Router table entries:
         Router_Table : constant Router_Table_Entry_Array := [
            -- Table entry for APID: 1
            0 => (Apid => 1, Destinations => null, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 2
            1 => (Apid => 2, Destinations => null, Sequence_Count_Mode => No_Check),
            -- Table entry for APID: 3
            2 => (Apid => 2, Destinations => null, Sequence_Count_Mode => Warn)
         ];
      begin
         T.Component_Instance.Init (Router_Table);
         -- Should never get here:
         Assert (False, "Duplicate APID did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Duplicate;
   begin
      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Test different start-up scenarios:
      T.Component_Instance.Final;
      Init_Nominal;
      T.Component_Instance.Final;
      Init_Index_Out_Of_Range_1;
      T.Component_Instance.Final;
      Init_Index_Out_Of_Range_2;
      T.Component_Instance.Final;
      Init_Duplicate;

      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
   end Test_Initialization;

   overriding procedure Test_Nominal_Routing (Self : in out Instance) is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send packet 1 sync and make sure it gets routed correctly:
      Packet_1.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_1);
      Self.Check_Routing (1, 0, 0, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (1), Packet_1);

      -- Send packet 1 async and make sure it gets routed correctly:
      Packet_1.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_1);
      Self.Check_Routing (1, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (2, 0, 0, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (2), Packet_1);

      -- Send packet 2 sync and make sure it gets routed correctly:
      Packet_2.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Self.Check_Routing (3, 0, 1, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (3), Packet_2);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_3_History.Get (1), Packet_2);

      -- Send packet 2 async and make sure it gets routed correctly:
      Packet_2.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_2);
      Self.Check_Routing (3, 0, 1, 0, 0, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (4, 0, 2, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (4), Packet_2);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_3_History.Get (2), Packet_2);

      -- Send packet 3 sync and make sure it gets routed correctly:
      Packet_3.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Self.Check_Routing (5, 0, 2, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (5), Packet_3);

      -- Send packet 3 async and make sure it gets routed correctly:
      Packet_3.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_3);
      Self.Check_Routing (5, 0, 2, 0, 0, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (6, 0, 2, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (6), Packet_3);

      -- Send packet 4 sync and make sure it gets routed correctly:
      Packet_4.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_4);
      Self.Check_Routing (6, 1, 2, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_2_History.Get (1), Packet_4);

      -- Send packet 4 async and make sure it gets routed correctly:
      Packet_4.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_4);
      Self.Check_Routing (6, 1, 2, 0, 0, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (6, 2, 2, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_2_History.Get (2), Packet_4);

      -- Send packet 5 sync and make sure it gets routed correctly:
      Packet_5.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Self.Check_Routing (6, 3, 2, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_2_History.Get (3), Packet_5);

      -- Send packet 5 async and make sure it gets routed correctly:
      Packet_5.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_5);
      Self.Check_Routing (6, 3, 2, 0, 0, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (6, 4, 2, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_2_History.Get (4), Packet_5);

      -- Send packet 6 sync and make sure it gets routed correctly:
      Packet_6.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Self.Check_Routing (6, 4, 2, 1, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_4_History.Get (1), Packet_6);

      -- Send packet 6 async and make sure it gets routed correctly:
      Packet_6.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_6);
      Self.Check_Routing (6, 4, 2, 1, 0, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (6, 4, 2, 2, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_4_History.Get (2), Packet_6);

      -- Send packet 7 sync and make sure it gets routed correctly:
      Packet_7.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_7);
      Self.Check_Routing (7, 4, 2, 2, 1, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (7), Packet_7);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_5_History.Get (1), Packet_7);

      -- Send packet 7 async and make sure it gets routed correctly:
      Packet_7.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_7);
      Self.Check_Routing (7, 4, 2, 2, 1, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (8, 4, 2, 2, 2, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (8), Packet_7);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_5_History.Get (2), Packet_7);

      -- Send packet 8 sync and make sure it gets ignored:
      Packet_8.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_8);
      Self.Check_Routing (8, 4, 2, 2, 2, 0);

      -- Send packet 9 async and make sure it gets ignored:
      Packet_8.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_8);
      Self.Check_Routing (8, 4, 2, 2, 2, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (8, 4, 2, 2, 2, 0);

      -- Make sure no events were thrown at all:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
   end Test_Nominal_Routing;

   overriding procedure Test_Unrecognized_Id (Self : in out Instance) is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send packet 0 sync and make sure it gets dropped:
      Packet_0.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_0);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (1), Packet_0.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet_0);

      -- Send packet 0 async and make sure it gets routed correctly:
      Packet_0.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_0);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 2);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (2), Packet_0.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet_0);

      -- Send packet 9 sync and make sure it gets dropped:
      Packet_9.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_9);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 3);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (3), Packet_9.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet_9);

      -- Send packet 9 async and make sure it gets dropped:
      Packet_9.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_9);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 4);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (4), Packet_9.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 4);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (4), Packet_9);
   end Test_Unrecognized_Id;

   overriding procedure Test_Dropped_Packet (Self : in out Instance) is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send (max sized) packet 2 async 3 times to fill queue:
      Packet_2.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_2);
      Packet_2.Header.Sequence_Count := 2;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_2);
      Packet_2.Header.Sequence_Count := 3;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_2);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);

      -- Send packet 2 sync and make sure it gets routed correctly:
      Packet_2.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Self.Check_Routing (1, 0, 1, 0, 0, 0);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_1_History.Get (1), Packet_2);
      Ccsds_Space_Packet_Assert.Eq (T.Ccsds_Space_Packet_T_Recv_Sync_3_History.Get (1), Packet_2);

      -- Overflow:
      T.Expect_Ccsds_Space_Packet_T_Send_2_Dropped := True;
      Packet_2.Header.Sequence_Count := 9;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_2);

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Packet_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Dropped_Packet_History.Get (1), Packet_2.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Error_Packet_History.Get (1).Header, Packet_2.Header);

      -- Do dispatch:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 3);
      Self.Check_Routing (4, 0, 4, 0, 0, 0);
   end Test_Dropped_Packet;

   overriding procedure Test_Sequence_Count_Warning (Self : in out Instance) is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send packet 1 sync and make sure it gets routed correctly with no events:
      Packet_1.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_1);
      Packet_1.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_1);
      Packet_1.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_1);
      Self.Check_Routing (3, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 2 sync and make sure it gets routed correctly with no events:
      Packet_2.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Packet_2.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Packet_2.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Self.Check_Routing (6, 0, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 3 sync and make sure it gets routed correctly with events:
      Packet_3.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Packet_3.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Packet_3.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Self.Check_Routing (9, 0, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 3);
      Packet_3.Header.Sequence_Count := 1;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (1), (Ccsds_Header => Packet_3.Header, Received_Sequence_Count => 1, Expected_Sequence_Count => 0));
      Packet_3.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (2), (Ccsds_Header => Packet_3.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 2));
      Packet_3.Header.Sequence_Count := 17;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (3), (Ccsds_Header => Packet_3.Header, Received_Sequence_Count => 17, Expected_Sequence_Count => 100));

      -- Send correct sequence count and make sure no event thrown:
      Packet_3.Header.Sequence_Count := 18;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Self.Check_Routing (10, 0, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 3);

      -- Send packet 4 sync and make sure it gets routed correctly with no events:
      Packet_4.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_4);
      Packet_4.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_4);
      Packet_4.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_4);
      Self.Check_Routing (10, 3, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 6);
      Packet_4.Header.Sequence_Count := 1;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (4), (Ccsds_Header => Packet_4.Header, Received_Sequence_Count => 1, Expected_Sequence_Count => 0));
      Packet_4.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (5), (Ccsds_Header => Packet_4.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 2));
      Packet_4.Header.Sequence_Count := 17;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (6), (Ccsds_Header => Packet_4.Header, Received_Sequence_Count => 17, Expected_Sequence_Count => 100));

      -- Send correct sequence count and make sure no event thrown:
      Packet_4.Header.Sequence_Count := 18;
      T.Ccsds_Space_Packet_T_Send (Packet_4);
      Self.Check_Routing (10, 4, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 6);

      -- Send packet 5 sync and make sure it gets routed correctly with no events:
      Packet_5.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Packet_5.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Packet_5.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Self.Check_Routing (10, 7, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 9);
      Packet_5.Header.Sequence_Count := 1;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (7), (Ccsds_Header => Packet_5.Header, Received_Sequence_Count => 1, Expected_Sequence_Count => 0));
      Packet_5.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (8), (Ccsds_Header => Packet_5.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 2));
      Packet_5.Header.Sequence_Count := 17;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (9), (Ccsds_Header => Packet_5.Header, Received_Sequence_Count => 17, Expected_Sequence_Count => 100));

      -- Send correct sequence count and make sure no event thrown:
      Packet_5.Header.Sequence_Count := 18;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Self.Check_Routing (10, 8, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 9);

      -- Send packet 6 sync and make sure it gets routed correctly with no events:
      Packet_6.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Packet_6.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Packet_6.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Self.Check_Routing (10, 8, 3, 3, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 12);
      Packet_6.Header.Sequence_Count := 1;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (10), (Ccsds_Header => Packet_6.Header, Received_Sequence_Count => 1, Expected_Sequence_Count => 0));
      Packet_6.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (11), (Ccsds_Header => Packet_6.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 2));
      Packet_6.Header.Sequence_Count := 17;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (12), (Ccsds_Header => Packet_6.Header, Received_Sequence_Count => 17, Expected_Sequence_Count => 100));

      -- Send correct sequence count and make sure no event thrown:
      Packet_6.Header.Sequence_Count := 18;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Self.Check_Routing (10, 8, 3, 4, 0, 0);
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 12);

      -- Send packet 7 sync and make sure it gets routed correctly with no events:
      Packet_7.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_7);
      Packet_7.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_7);
      Packet_7.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_7);
      Self.Check_Routing (13, 8, 3, 4, 3, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 8 sync and make sure it gets routed correctly with no events:
      Packet_8.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_8);
      Packet_8.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_8);
      Packet_8.Header.Sequence_Count := 17;
      T.Ccsds_Space_Packet_T_Send (Packet_8);
      Self.Check_Routing (13, 8, 3, 4, 3, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 15);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 15);
      Packet_8.Header.Sequence_Count := 1;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (13), (Ccsds_Header => Packet_8.Header, Received_Sequence_Count => 1, Expected_Sequence_Count => 0));
      Packet_8.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (14), (Ccsds_Header => Packet_8.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 2));
      Packet_8.Header.Sequence_Count := 17;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (15), (Ccsds_Header => Packet_8.Header, Received_Sequence_Count => 17, Expected_Sequence_Count => 100));

      -- Send correct sequence count and make sure no event thrown:
      Packet_8.Header.Sequence_Count := 18;
      T.Ccsds_Space_Packet_T_Send (Packet_8);
      Self.Check_Routing (13, 8, 3, 4, 3, 0);
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 15);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
   end Test_Sequence_Count_Warning;

   overriding procedure Test_Duplicate_Packet_Drop (Self : in out Instance) is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send packet 1 sync and make sure it gets routed correctly with no events:
      Packet_1.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_1);
      Packet_1.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_1);
      Packet_1.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_1);
      Self.Check_Routing (3, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 2 sync and make sure it gets routed correctly with no events:
      Packet_2.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Packet_2.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Packet_2.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_2);
      Self.Check_Routing (6, 0, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 3 sync and make sure it gets routed correctly with no events:
      Packet_3.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Packet_3.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Packet_3.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_3);
      Self.Check_Routing (9, 0, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 3);
      Packet_3.Header.Sequence_Count := 1;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (1), (Ccsds_Header => Packet_3.Header, Received_Sequence_Count => 1, Expected_Sequence_Count => 0));
      Packet_3.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (2), (Ccsds_Header => Packet_3.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 2));
      Packet_3.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (3), (Ccsds_Header => Packet_3.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 100));

      -- Send packet 5 sync and make sure it gets dropped with events:
      Packet_5.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Packet_5.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Packet_5.Header.Sequence_Count := 99;
      T.Ccsds_Space_Packet_T_Send (Packet_5);
      Self.Check_Routing (9, 2, 3, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 6);
      Packet_5.Header.Sequence_Count := 1;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (4), (Ccsds_Header => Packet_5.Header, Received_Sequence_Count => 1, Expected_Sequence_Count => 0));
      Packet_5.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (5), (Ccsds_Header => Packet_5.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 2));
      Packet_5.Header.Sequence_Count := 99;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (6), (Ccsds_Header => Packet_5.Header, Received_Sequence_Count => 99, Expected_Sequence_Count => 100));
      Natural_Assert.Eq (T.Dropped_Duplicate_Packet_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Dropped_Duplicate_Packet_History.Get (1), Packet_5.Header);

      -- Check packets:
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet_5);

      -- Send packet 6 sync and make sure it gets dropped with events:
      Packet_6.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Packet_6.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Packet_6.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_6);
      Self.Check_Routing (9, 2, 3, 1, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Check events:
      Natural_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get_Count, 8);
      Packet_6.Header.Sequence_Count := 0;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (7), (Ccsds_Header => Packet_6.Header, Received_Sequence_Count => 0, Expected_Sequence_Count => 1));
      Packet_6.Header.Sequence_Count := 0;
      Unexpected_Sequence_Count_Assert.Eq (T.Unexpected_Sequence_Count_Received_History.Get (8), (Ccsds_Header => Packet_6.Header, Received_Sequence_Count => 0, Expected_Sequence_Count => 1));
      Natural_Assert.Eq (T.Dropped_Duplicate_Packet_History.Get_Count, 3);
      Ccsds_Primary_Header_Assert.Eq (T.Dropped_Duplicate_Packet_History.Get (2), Packet_6.Header);
      Ccsds_Primary_Header_Assert.Eq (T.Dropped_Duplicate_Packet_History.Get (3), Packet_6.Header);

      -- Check packets:
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet_6);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet_6);
   end Test_Duplicate_Packet_Drop;

end Ccsds_Router_Tests.Implementation;
