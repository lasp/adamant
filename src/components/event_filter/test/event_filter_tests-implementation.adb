--------------------------------------------------------------------------------
-- Event_Filter Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Command;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Event;
with Event_Filter_Enums; use Event_Filter_Enums;
with Event_Filter_Entry; use Event_Filter_Entry;
with Event_Filter_Entry_Enums; use Event_Filter_Entry_Enums;
with Event_Types; use Event_Types;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Interfaces; use Interfaces;
with Packet;
with Packet.Assertion; use Packet.Assertion;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Smart_Assert;
with Tick;

package body Event_Filter_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Custom Asserts:
   -------------------------------------------------------------------------
   package Component_State_Assert is new Smart_Assert.Discrete (Event_Filter_Entry_Enums.Global_Filter_State.E, Event_Filter_Entry_Enums.Global_Filter_State.E'Image);
   package Id_Assert is new Smart_Assert.Discrete (Event_Types.Event_Id, Event_Types.Event_Id'Image);
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
      -- Self.Tester.Component_Instance.Init (Event_Id_Start_Range => TBD, Event_Id_End_Range => TBD, Event_Filter_List => TBD);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Received_Event (Self : in out Instance) is
      T : Component.Event_Filter.Implementation.Tester.Instance_Access renames Self.Tester;
      Event_Filter_Init_List : constant Event_Id_List := [3, 6];
      Incoming_Event : Event.T;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Received Filtered Event:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start_Range => 0, Event_Id_End_Range => 10, Event_Filter_List => Event_Filter_Init_List);

      -- Test that we received an event for both the filtered and unfiltered cases
      -- Data product init
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Packed_U32_Assert.Eq (T.Total_Events_Filtered_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Total_Events_Unfiltered_History.Get (1), (Value => 0));
      Component_State_Assert.Eq (T.Component_Filter_State_History.Get (1).Component_Filter_State, Event_Filter_Entry_Enums.Global_Filter_State.Enabled);

      -- Start with making sure no events have been forwarded yet.
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      -- Send in a nominal ID. Empty init list should indicate this is enabled and this should be successful.
      Incoming_Event.Header.Id := 0;
      T.Event_T_Send (Incoming_Event);

      -- This event should not be filtered so make sure it comes through
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 0);

      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 6);

      -- Now use another event that should be filtered and make sure nothing comes out
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      Incoming_Event.Header.Id := 3;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      -- Lastly make sure that if we have an id out of range, then it still gets passed through
      Incoming_Event.Header.Id := 11;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      -- Make sure no data products were thrown since we don't call a tick
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

   end Test_Received_Event;

   overriding procedure Test_Data_Products (Self : in out Instance) is
      T : Component.Event_Filter.Implementation.Tester.Instance_Access renames Self.Tester;
      Event_Filter_Init_List : constant Event_Id_List := [2, 5];
      Incoming_Event : Event.T;
      Input_Tick : constant Tick.T := ((0, 0), 0);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Data Products:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start_Range => 1, Event_Id_End_Range => 12, Event_Filter_List => Event_Filter_Init_List);

      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Packed_U32_Assert.Eq (T.Total_Events_Filtered_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Total_Events_Unfiltered_History.Get (1), (Value => 0));
      Component_State_Assert.Eq (T.Component_Filter_State_History.Get (1).Component_Filter_State, Event_Filter_Entry_Enums.Global_Filter_State.Enabled);

      -- Send in events that are both filtered and not and check that we counted correctly
      Incoming_Event.Header.Id := 3;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);

      Incoming_Event.Header.Id := 6;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 6);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (6).Header.Id, 6);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 7);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 8);

      -- No filtered events yet...
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Packed_U32_Assert.Eq (T.Total_Events_Unfiltered_History.Get (2), (Value => 8));

      -- Test that no unfiltered counts come out here
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Incoming_Event.Header.Id := 2;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      Incoming_Event.Header.Id := 5;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      -- Only filtered events now
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Packed_U32_Assert.Eq (T.Total_Events_Filtered_History.Get (2), (Value => 6));

      -- Now a mix of both
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Incoming_Event.Header.Id := 7;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      Incoming_Event.Header.Id := 2;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      Incoming_Event.Header.Id := 9;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 6);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 7);

      Incoming_Event.Header.Id := 5;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 7);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 7);

      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Packed_U32_Assert.Eq (T.Total_Events_Filtered_History.Get (3), (Value => 11));
      Packed_U32_Assert.Eq (T.Total_Events_Unfiltered_History.Get (3), (Value => 15));

   end Test_Data_Products;

   overriding procedure Test_Issue_State_Packet (Self : in out Instance) is
      T : Component.Event_Filter.Implementation.Tester.Instance_Access renames Self.Tester;
      Event_Filter_Init_List : constant Event_Id_List := [2, 5, 9, 16, 20];
      Input_Tick : constant Tick.T := ((0, 0), 0);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing State Packet:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start_Range => 0, Event_Id_End_Range => 20, Event_Filter_List => Event_Filter_Init_List);

      -- Make sure no events generated so far
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Issue the packet and make sure the initialized setup looks correct
      T.Command_T_Send (T.Commands.Dump_Event_States);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Event_States_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 1);

      -- Induce the packet through the tick
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Filter_State_Packet_History.Get_Count, 1);

      Packet_Assert.Eq (T.Event_Filter_State_Packet_History.Get (1), (Header => (Time => (0, 0), Id => T.Packets.Get_Event_Filter_State_Packet_Id, Sequence_Count => 0, Buffer_Length => 3), Buffer => [0 => 36, 1 => 64, 2 => 136, others => 0]));

      -- Now change some of the states and issue the packet again
      T.Command_T_Send (T.Commands.Filter_Event ((Event_To_Update => (Id => 4), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Filtered_Event_History.Get_Count, 1);

      T.Command_T_Send (T.Commands.Filter_Event ((Event_To_Update => (Id => 10), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Filtered_Event_History.Get_Count, 2);

      T.Command_T_Send (T.Commands.Filter_Event ((Event_To_Update => (Id => 12), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Filtered_Event_History.Get_Count, 3);

      -- Issue packet
      T.Command_T_Send (T.Commands.Dump_Event_States);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Event_States_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 2);

      -- Induce the packet through the tick
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Filter_State_Packet_History.Get_Count, 2);

      Packet_Assert.Eq (T.Event_Filter_State_Packet_History.Get (2), (Header => (Time => (0, 0), Id => T.Packets.Get_Event_Filter_State_Packet_Id, Sequence_Count => 1, Buffer_Length => 3), Buffer => [0 => 16#2C#, 1 => 16#68#, 2 => 16#88#, others => 0]));

      -- Now unfilter a range and check the packet one more time. Issue the packet with the command for this one.
      T.Command_T_Send (T.Commands.Unfilter_Event_Range ((Start_Event_Id => (Id => 2), Stop_Event_Id => (Id => 12), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Range_Id, Status => Success));
      -- Increases twice here for the issue of the packet
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unfiltered_Event_Range_History.Get_Count, 1);

      -- Get the packet
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Filter_State_Packet_History.Get_Count, 3);

      Packet_Assert.Eq (T.Event_Filter_State_Packet_History.Get (3), (Header => (Time => (0, 0), Id => T.Packets.Get_Event_Filter_State_Packet_Id, Sequence_Count => 2, Buffer_Length => 3), Buffer => [0 => 0, 1 => 0, 2 => 16#88#, others => 0]));

   end Test_Issue_State_Packet;

   overriding procedure Test_Command_Single_State_Change (Self : in out Instance) is
      T : Component.Event_Filter.Implementation.Tester.Instance_Access renames Self.Tester;
      Event_Filter_Init_List : constant Event_Id_List := [5, 8];
      Incoming_Event : Event.T;
      Input_Tick : constant Tick.T := ((0, 0), 0);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Event Single State Change:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start_Range => 3, Event_Id_End_Range => 12, Event_Filter_List => Event_Filter_Init_List);

      -- Make sure no events generated so far
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Enable event filtering for a range of the events that are not filtered right now
      T.Command_T_Send (T.Commands.Filter_Event ((Event_To_Update => (Id => 4), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Filtered_Event_History.Get_Count, 1);

      -- Now check that it is filtered
      Incoming_Event.Header.Id := 4;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      -- Add another event to filter and this time issue the packet
      -- Enable event filtering for a range of the events that are not filtered right now
      T.Command_T_Send (T.Commands.Filter_Event ((Event_To_Update => (Id => 3), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 1);
      Natural_Assert.Eq (T.Filtered_Event_History.Get_Count, 2);

      -- Issue the packet through a tick
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Filter_State_Packet_History.Get_Count, 1);
      Packet_Assert.Eq (T.Event_Filter_State_Packet_History.Get (1), (Header => (Time => (0, 0), Id => T.Packets.Get_Event_Filter_State_Packet_Id, Sequence_Count => 0, Buffer_Length => 2), Buffer => [0 => 228, others => 0]));

      -- Enable event filtering for a range of the events that are not filtered right now
      T.Command_T_Send (T.Commands.Unfilter_Event ((Event_To_Update => (Id => 4), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unfiltered_Event_History.Get_Count, 1);

      -- Should be unfiltered now
      Incoming_Event.Header.Id := 4;
      T.Event_T_Send (Incoming_Event);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 4);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      -- Unfilter another event and this time issue the packet
      T.Command_T_Send (T.Commands.Unfilter_Event ((Event_To_Update => (Id => 3), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unfiltered_Event_History.Get_Count, 2);

      -- Issue the packet through a tick
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Filter_State_Packet_History.Get_Count, 2);
      Packet_Assert.Eq (T.Event_Filter_State_Packet_History.Get (2), (Header => (Time => (0, 0), Id => T.Packets.Get_Event_Filter_State_Packet_Id, Sequence_Count => 1, Buffer_Length => 2), Buffer => [0 => 36, others => 0]));

      -- Finally send invalid events
      T.Command_T_Send (T.Commands.Filter_Event ((Event_To_Update => (Id => 2), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Filter_Event_Invalid_Id_History.Get_Count, 1);

      T.Command_T_Send (T.Commands.Unfilter_Event ((Event_To_Update => (Id => 13), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Unfilter_Event_Invalid_Id_History.Get_Count, 1);

   end Test_Command_Single_State_Change;

   overriding procedure Test_Command_Range_State_Change (Self : in out Instance) is
      T : Component.Event_Filter.Implementation.Tester.Instance_Access renames Self.Tester;
      Event_Filter_Init_List : constant Event_Id_List := [1, 9];
      Incoming_Event : Event.T;
      Input_Tick : constant Tick.T := ((0, 0), 0);
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Event Range State Change:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start_Range => 1, Event_Id_End_Range => 15, Event_Filter_List => Event_Filter_Init_List);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Enable event filtering for a range of the events that are not filtered right now
      T.Command_T_Send (T.Commands.Filter_Event_Range ((Start_Event_Id => (Id => 2), Stop_Event_Id => (Id => 8), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Filtered_Event_Range_History.Get_Count, 1);

      -- Now check that each one is filtered
      Incoming_Event.Header.Id := 2;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      Incoming_Event.Header.Id := 3;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      Incoming_Event.Header.Id := 4;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      Incoming_Event.Header.Id := 5;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      Incoming_Event.Header.Id := 6;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      Incoming_Event.Header.Id := 7;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      Incoming_Event.Header.Id := 8;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 0);

      -- Disable event filtering for a range of the events that are   filtered right now
      T.Command_T_Send (T.Commands.Unfilter_Event_Range ((Start_Event_Id => (Id => 2), Stop_Event_Id => (Id => 8), Issue_State_Packet => Issue_Packet_Type.No_Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unfiltered_Event_Range_History.Get_Count, 1);

      Incoming_Event.Header.Id := 2;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);

      Incoming_Event.Header.Id := 3;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);

      Incoming_Event.Header.Id := 4;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 6);

      Incoming_Event.Header.Id := 5;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 7);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 8);

      Incoming_Event.Header.Id := 6;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 9);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 10);

      Incoming_Event.Header.Id := 7;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 11);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 12);

      Incoming_Event.Header.Id := 8;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 13);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 14);

      -- Filter a range again but this time issue the packet
      -- Enable event filtering for a range of the events that are not filtered right now
      T.Command_T_Send (T.Commands.Filter_Event_Range ((Start_Event_Id => (Id => 4), Stop_Event_Id => (Id => 7), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 1);
      Natural_Assert.Eq (T.Filtered_Event_Range_History.Get_Count, 4); -- This is 4 due to the conflict of event ids and how we pass them into the component

      -- Issue the packet through a tick
      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Filter_State_Packet_History.Get_Count, 1);
      Packet_Assert.Eq (T.Event_Filter_State_Packet_History.Get (1), (Header => (Time => (0, 0), Id => T.Packets.Get_Event_Filter_State_Packet_Id, Sequence_Count => 0, Buffer_Length => 2), Buffer => [0 => 158, 1 => 128, others => 0]));

      -- Back to the start with a packet issue this time
      T.Command_T_Send (T.Commands.Unfilter_Event_Range ((Start_Event_Id => (Id => 4), Stop_Event_Id => (Id => 7), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Range_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Dump_Event_States_Received_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unfiltered_Event_Range_History.Get_Count, 4);

      T.Tick_T_Send (Input_Tick);
      Natural_Assert.Eq (T.Event_Filter_State_Packet_History.Get_Count, 2);
      Packet_Assert.Eq (T.Event_Filter_State_Packet_History.Get (2), (Header => (Time => (0, 0), Id => T.Packets.Get_Event_Filter_State_Packet_Id, Sequence_Count => 1, Buffer_Length => 2), Buffer => [0 => 128, 1 => 128, others => 0]));

      -- Finally give some invalid ranges for both commands.
      T.Command_T_Send (T.Commands.Filter_Event_Range ((Start_Event_Id => (Id => 0), Stop_Event_Id => (Id => 7), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Filter_Event_Range_Invalid_Id_History.Get_Count, 1);

      T.Command_T_Send (T.Commands.Filter_Event_Range ((Start_Event_Id => (Id => 8), Stop_Event_Id => (Id => 16), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Filter_Event_Range_Invalid_Id_History.Get_Count, 2);

      T.Command_T_Send (T.Commands.Unfilter_Event_Range ((Start_Event_Id => (Id => 0), Stop_Event_Id => (Id => 7), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 7);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Unfilter_Event_Range_Invalid_Id_History.Get_Count, 1);

      T.Command_T_Send (T.Commands.Unfilter_Event_Range ((Start_Event_Id => (Id => 8), Stop_Event_Id => (Id => 16), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 8);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (8), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Unfilter_Event_Range_Invalid_Id_History.Get_Count, 2);

      -- Also test backward ranges are invalid for both commands.
      T.Command_T_Send (T.Commands.Filter_Event_Range ((Start_Event_Id => (Id => 14), Stop_Event_Id => (Id => 5), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 9);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (9), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Filter_Event_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Filter_Event_Range_Invalid_Id_History.Get_Count, 3);

      T.Command_T_Send (T.Commands.Unfilter_Event_Range ((Start_Event_Id => (Id => 12), Stop_Event_Id => (Id => 4), Issue_State_Packet => Issue_Packet_Type.Issue)));
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 10);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (10), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Range_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Unfilter_Event_Range_Invalid_Id_History.Get_Count, 3);

   end Test_Command_Range_State_Change;

   overriding procedure Test_Command_Component_State_Change (Self : in out Instance) is
      T : Component.Event_Filter.Implementation.Tester.Instance_Access renames Self.Tester;
      Event_Filter_Init_List : constant Event_Id_List := [4, 9];
      Incoming_Event : Event.T;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Component State Change:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start_Range => 4, Event_Id_End_Range => 9, Event_Filter_List => Event_Filter_Init_List);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Packed_U32_Assert.Eq (T.Total_Events_Filtered_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Total_Events_Unfiltered_History.Get (1), (Value => 0));
      Component_State_Assert.Eq (T.Component_Filter_State_History.Get (1).Component_Filter_State, Event_Filter_Entry_Enums.Global_Filter_State.Enabled);

      -- Enabled by default so disable here
      T.Command_T_Send (T.Commands.Disable_Event_Filtering);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Event_Filtering_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Disable_Event_Filter_History.Get_Count, 1);
      -- Data product
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Component_Filter_State_History.Get_Count, 2);
      Component_State_Assert.Eq (T.Component_Filter_State_History.Get (2).Component_Filter_State, Event_Filter_Entry_Enums.Global_Filter_State.Disabled);

      -- Make sure no filtering occurs even on the ones initialized to filter
      Incoming_Event.Header.Id := 6;
      T.Event_T_Send (Incoming_Event);

      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 6);

      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      Incoming_Event.Header.Id := 4;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (4).Header.Id, 4);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 6);

      Incoming_Event.Header.Id := 9;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 7);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (7).Header.Id, 9);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 8);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 9);

      -- Re-enable and test again
      T.Command_T_Send (T.Commands.Enable_Event_Filtering);
      -- Command and event info
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Event_Filtering_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Enable_Event_Filter_History.Get_Count, 1);
      -- Data product, no tick so the total counts don't get updated here
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Component_Filter_State_History.Get_Count, 3);
      Component_State_Assert.Eq (T.Component_Filter_State_History.Get (3).Component_Filter_State, Event_Filter_Entry_Enums.Global_Filter_State.Enabled);

      -- Now make sure there is filtering for those that were initialized to be filtered
      T.Event_Forward_T_Recv_Sync_History.Clear;
      Incoming_Event.Header.Id := 6;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 1);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (1).Header.Id, 6);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 2);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      Incoming_Event.Header.Id := 4;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      Incoming_Event.Header.Id := 9;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 3);

      Incoming_Event.Header.Id := 7;
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 4);
      Id_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get (4).Header.Id, 7);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 5);
      T.Event_T_Send (Incoming_Event);
      Natural_Assert.Eq (T.Event_Forward_T_Recv_Sync_History.Get_Count, 6);

   end Test_Command_Component_State_Change;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Event_Filter.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Unfilter_Event ((Event_To_Update => (Id => 1), Issue_State_Packet => Issue_Packet_Type.No_Issue));
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Invalid Command:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Event_Id_Start_Range => 0, Event_Id_End_Range => 10);

      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Unfilter_Event_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Unfilter_Event_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Event_Filter_Tests.Implementation;
