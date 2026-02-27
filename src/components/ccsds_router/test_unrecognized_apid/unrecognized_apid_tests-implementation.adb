--------------------------------------------------------------------------------
-- Ccsds_Router Tests Body
--------------------------------------------------------------------------------

with Test_Assembly_Ccsds_Router_Table;
with Ccsds_Space_Packet.Assertion; use Ccsds_Space_Packet.Assertion;
with Ccsds_Primary_Header.Assertion; use Ccsds_Primary_Header.Assertion;
use Ccsds_Primary_Header;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;
with Ccsds_Enums; use Ccsds_Enums;

package body Unrecognized_Apid_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3, Ccsds_Space_Packet_T_Send_Count => 6);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Insert custom cleanup code here.
      Self.Tester.Component_Instance.Final;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   Packet_0 : Ccsds_Space_Packet.T := (Header => (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (0), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented, Sequence_Count => Ccsds_Sequence_Count_Type (53), Packet_Length => 0), Data => [others => 0]);

   Packet_9 : Ccsds_Space_Packet.T := (Header => (Version => 0, Packet_Type => Ccsds_Packet_Type.Telecommand, Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present, Apid => Ccsds_Apid_Type (9), Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented, Sequence_Count => Ccsds_Sequence_Count_Type (53), Packet_Length => 9), Data => [others => 9]);

   procedure Check_Routing (Self : in out Instance; Count_1 : in Natural; Count_2 : in Natural; Count_3 : in Natural; Count_4 : in Natural; Count_5 : in Natural; Count_6 : in Natural; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
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

   overriding procedure Test_Unrecognized_Id (Self : in out Instance) is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Call component init here.
      T.Component_Instance.Init (Table => Test_Assembly_Ccsds_Router_Table.Router_Table, Report_Unrecognized_Apids => True);

      -- Send packet 0 sync and make sure it gets forwarded and reported:
      Packet_0.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_0);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 1);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (1), Packet_0.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (1), Packet_0);

      -- Send packet 0 async and make sure it gets forwarded and reported:
      Packet_0.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_0);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), Packet_0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 2);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (2), Packet_0.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (2), Packet_0);

      -- Send packet 9 sync and make sure it gets forwarded and reported:
      Packet_9.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_9);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (3), Packet_9);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 3);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (3), Packet_9.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (3), Packet_9);

      -- Send packet 9 async and make sure it gets forwarded and reported:
      Packet_9.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_9);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (4), Packet_9);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unrecognized_Apid_History.Get_Count, 4);
      Ccsds_Primary_Header_Assert.Eq (T.Unrecognized_Apid_History.Get (4), Packet_9.Header);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Error_Packet_History.Get_Count, 4);
      Ccsds_Space_Packet_Assert.Eq (T.Error_Packet_History.Get (4), Packet_9);
   end Test_Unrecognized_Id;

   overriding procedure Test_Unrecognized_Id_No_Report (Self : in out Instance) is
      T : Component.Ccsds_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Call component init here.
      T.Component_Instance.Init (Table => Test_Assembly_Ccsds_Router_Table.Router_Table, Report_Unrecognized_Apids => False);

      -- Send packet 0 sync and make sure it gets forwarded and reported:
      Packet_0.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_0);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 1);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (1), Packet_0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 0 async and make sure it gets forwarded and reported:
      Packet_0.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_0);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 2);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (2), Packet_0);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 9 sync and make sure it gets forwarded and reported:
      Packet_9.Header.Sequence_Count := 0;
      T.Ccsds_Space_Packet_T_Send (Packet_9);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 3);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (3), Packet_9);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send packet 9 async and make sure it gets forwarded but not reported:
      Packet_9.Header.Sequence_Count := 1;
      T.Ccsds_Space_Packet_T_Send_2 (Packet_9);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Self.Check_Routing (0, 0, 0, 0, 0, 0);
      Natural_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get_Count, 4);
      Ccsds_Space_Packet_Assert.Eq (T.Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History.Get (4), Packet_9);

      -- Make sure proper event thrown
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
   end Test_Unrecognized_Id_No_Report;

end Unrecognized_Apid_Tests.Implementation;
