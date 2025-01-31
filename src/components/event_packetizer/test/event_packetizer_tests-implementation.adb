--------------------------------------------------------------------------------
-- Event_Packetizer Tests Body
--------------------------------------------------------------------------------

with Tick;
with Packet;
with Packet_Types; use Packet_Types;
with Event_Header;
with Event_Packetizer_Commands;
with Basic_Assertions; use Basic_Assertions;
with Event.Assertion; use Event.Assertion;
with Packet_Header.Assertion; use Packet_Header.Assertion;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Packed_Natural.Assertion; use Packed_Natural.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;

package body Event_Packetizer_Tests.Implementation is

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
      -- Custom cleanup code here.
      Self.Tester.Component_Instance.Destroy;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Global Event Definitions:
   -------------------------------------------------------------------------

   Event_1 : constant Event.T := (Header => ((1, 2), 1, 0), Param_Buffer => [others => 0]); -- Same size as event header, 14 bytes.
   Event_2 : constant Event.T := (Header => ((1, 2), 2, 6), Param_Buffer => [0 => 1, 1 => 2, 2 => 3, 3 => 4, 4 => 5, 5 => 6, others => 0]); -- Event header + 6 bytes, 20 bytes.
   Event_3 : constant Event.T := (Header => ((1, 2), 3, 3), Param_Buffer => [0 => 1, 1 => 2, 2 => 3, 3 => 4, 4 => 5, 5 .. 11 => 6, others => 0]); -- Event header + 3 bytes, 14 bytes.

   -------------------------------------------------------------------------
   -- Helper function:
   -------------------------------------------------------------------------
   -- Function which extracts an event from the packet at the given index, and checks it against,
   -- the passed in event. The next unchecked index into the packet is returned.
   function Check_Event (The_Packet : in Packet.T; Evt : in Event.T; Current_Index : in Natural) return Natural is
      Extracted_Event : Event.T := (((0, 0), 0, 0), [others => 0]);
      Param_Index : constant Natural := Current_Index + Event_Header.Serialization.Serialized_Length;
      Next_Index : Natural := Param_Index;
   begin
      -- Deserialize an event header from the packet:
      Extracted_Event.Header := Event_Header.Serialization.From_Byte_Array (The_Packet.Buffer (Current_Index .. Param_Index - 1));

      -- Grab the event parameters:
      if Extracted_Event.Header.Param_Buffer_Length > 0 then
         Next_Index := @ + Extracted_Event.Header.Param_Buffer_Length;
         Extracted_Event.Param_Buffer (Extracted_Event.Param_Buffer'First .. Extracted_Event.Param_Buffer'First + Extracted_Event.Header.Param_Buffer_Length - 1) := The_Packet.Buffer (Param_Index .. Next_Index - 1);
      end if;

      -- Check the event:
      Event_Assert.Eq (Extracted_Event, Evt);

      -- Return the next index:
      return Next_Index;
   end Check_Event;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Packetization (Self : in out Instance) is
      T : Component.Event_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((1, 2), 3);
      Expected_Packet_Header : Packet_Header.T := (Time => T.System_Time, Id => 0, Sequence_Count => 0, Buffer_Length => Packet_Types.Packet_Buffer_Type'Length);
      The_Packet : Packet.T;
      P_Idx : Natural;
      Bytes_Sent : Natural := 0;
   begin
      -- Initialize the component with 2 internal packets, and no packet timeout:
      T.Component_Instance.Init (Num_Internal_Packets => 2, Partial_Packet_Timeout => 0);

      -- Send some ticks and expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Some data products should have been send out however:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Bytes_Available_History.Get_Count, 1);
      Packed_Natural_Assert.Eq (T.Bytes_Available_History.Get (1), (Value => Packet_Types.Packet_Buffer_Type'Length * 2));

      -- Send an event:
      T.Event_T_Send (Event_3);
      Bytes_Sent := @ + (Event_Header.Serialization.Serialized_Length + Event_3.Header.Param_Buffer_Length);

      -- Send some ticks and expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Some data products should have been send out however:
      Natural_Assert.Eq (T.Bytes_Available_History.Get_Count, 2);
      Packed_Natural_Assert.Eq (T.Bytes_Available_History.Get (2), (Value => Packet_Types.Packet_Buffer_Type'Length * 2 - Event_Header.Serialization.Serialized_Length - Event_3.Header.Param_Buffer_Length));

      -- OK, now fill up a packet:
      for Idx in 1 .. ((Packet_Types.Packet_Buffer_Type'Length / (Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length)) - 1) loop
         T.Event_T_Send (Event_3);
         Bytes_Sent := @ + (Event_Header.Serialization.Serialized_Length + Event_3.Header.Param_Buffer_Length);
         T.Tick_T_Send (A_Tick);
         Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);
         -- Some data products should have been send out however:
         Natural_Assert.Eq (T.Bytes_Available_History.Get_Count, 2 + Idx);
         Packed_Natural_Assert.Eq (T.Bytes_Available_History.Get (2 + Idx), (Value => Packet_Types.Packet_Buffer_Type'Length * 2 - (Event_Header.Serialization.Serialized_Length + Event_3.Header.Param_Buffer_Length) * (Idx + 1)));
      end loop;

      -- Send one more event and then tick to overflow current packet and
      -- expect this packet to be emitted:
      --while Bytes_Sent < Packet_Types.Packet_Buffer_Type'Length loop
      T.Event_T_Send (Event_2);
      -- Bytes_Sent := @ + (Event_Header.Serialization.Serialized_Length + Event_2.Header.Param_Buffer_Length);
      --end loop;
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 1);

      -- Make sure that one packet is totally full + 1 additional event:
      Packed_Natural_Assert.Eq (T.Bytes_Available_History.Get (T.Bytes_Available_History.Get_Count), (Value => Packet_Types.Packet_Buffer_Type'Length - Event_Header.Serialization.Serialized_Length - Event_2.Header.Param_Buffer_Length));

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (1);
      Expected_Packet_Header.Buffer_Length := Bytes_Sent;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- OK, now fill the second packet and make sure that it is sent out:
      for Idx in 1 .. ((Packet_Types.Packet_Buffer_Type'Length - (Event_Header.Serialization.Serialized_Length + Event_2.Header.Param_Buffer_Length)) / (Event_Header.T'Object_Size / 8 + Event_1.Header.Param_Buffer_Length)) loop
         T.Event_T_Send (Event_1);
         T.Tick_T_Send (A_Tick);
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      end loop;

      -- Send one more event and then tick to overflow current packet and
      -- expect this packet to be emitted:
      T.Event_T_Send (Event_2);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 2);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (2);
      Expected_Packet_Header.Buffer_Length := ((Packet_Types.Packet_Buffer_Type'Length - (Event_Header.Serialization.Serialized_Length + Event_2.Header.Param_Buffer_Length)) / (Event_Header.T'Object_Size / 8 + Event_1.Header.Param_Buffer_Length)) * (Event_Header.T'Object_Size / 8 + Event_1.Header.Param_Buffer_Length) + (Event_Header.Serialization.Serialized_Length + Event_2.Header.Param_Buffer_Length);
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      P_Idx := Check_Event (The_Packet, Event_2, @);
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_1, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- OK, now fill the first packet again and make sure that it is sent out:
      for Idx in 1 .. ((Packet_Types.Packet_Buffer_Type'Length / (Event_Header.T'Object_Size / 8 + Event_2.Header.Param_Buffer_Length)) - 1) loop
         T.Event_T_Send (Event_2);
         T.Tick_T_Send (A_Tick);
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      end loop;

      -- Send one more event and then tick to overflow current packet and
      -- expect this packet to be emitted:
      T.Event_T_Send (Event_3);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 3);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (3);
      Expected_Packet_Header.Buffer_Length := (Packet_Types.Packet_Buffer_Type'Length / (Event_Header.T'Object_Size / 8 + Event_2.Header.Param_Buffer_Length)) * (Event_Header.T'Object_Size / 8 + Event_2.Header.Param_Buffer_Length);
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_2, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Make sure no events were dropped:
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 0);
   end Test_Nominal_Packetization;

   overriding procedure Test_Partial_Packet_Timeout (Self : in out Instance) is
      T : Component.Event_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((1, 2), 3);
      Expected_Packet_Header : Packet_Header.T := (Time => T.System_Time, Id => 0, Sequence_Count => 0, Buffer_Length => Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length);
      The_Packet : Packet.T;
      P_Idx : Natural;
   begin
      -- Initialize the component with 2 internal packets, and a packet timeout of 3 ticks:
      T.Component_Instance.Init (Num_Internal_Packets => 2, Partial_Packet_Timeout => 3);

      -- Send some ticks and expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send an event:
      T.Event_T_Send (Event_3);

      -- Send some ticks and expect packet on third tick:
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 1);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (1);
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Send some ticks and expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send two events:
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);

      -- Send some ticks and expect packet on third tick:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 2);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (2);
      Expected_Packet_Header.Buffer_Length := (Event_Header.T'Object_Size / 8 + Event_2.Header.Param_Buffer_Length) * 2;
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_2, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Make sure no events were dropped:
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 0);
   end Test_Partial_Packet_Timeout;

   overriding procedure Test_Partial_Packet_Timeout_Of_1 (Self : in out Instance) is
      T : Component.Event_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((1, 2), 3);
      Expected_Packet_Header : Packet_Header.T := (Time => T.System_Time, Id => 0, Sequence_Count => 0, Buffer_Length => Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length);
      The_Packet : Packet.T;
      P_Idx : Natural;
   begin
      -- Initialize the component with 2 internal packets, and a packet timeout of 3 ticks:
      T.Component_Instance.Init (Num_Internal_Packets => 2, Partial_Packet_Timeout => 1);

      -- Send some ticks and expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send an event:
      T.Event_T_Send (Event_3);

      -- Send some ticks and expect packet on third tick:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 1);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (1);
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Send some ticks and expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send two events:
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);

      -- Send some ticks and expect packet on third tick:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 2);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (2);
      Expected_Packet_Header.Buffer_Length := (Event_Header.T'Object_Size / 8 + Event_2.Header.Param_Buffer_Length) * 2;
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_2, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Make sure no events were dropped:
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 0);
   end Test_Partial_Packet_Timeout_Of_1;

   overriding procedure Test_Commanded_Packetization (Self : in out Instance) is
      T : Component.Event_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((1, 2), 3);
      Expected_Packet_Header : Packet_Header.T := (Time => T.System_Time, Id => 0, Sequence_Count => 0, Buffer_Length => Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length);
      The_Packet : Packet.T;
      P_Idx : Natural;
      Commands : Event_Packetizer_Commands.Instance renames Self.Tester.Commands;
   begin
      -- Initialize the component with 2 internal packets, and a disabled packet timeout:
      T.Component_Instance.Init (Num_Internal_Packets => 2, Partial_Packet_Timeout => 0);

      -- Send some ticks and expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send command to spit out a packet:
      T.Command_T_Send (Commands.Send_Packet);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Success));

      -- Since there are no packets in the component, expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send an event:
      T.Event_T_Send (Event_3);

      -- Since there the packet is not full, expect no packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send the command again:
      T.Command_T_Send (Commands.Send_Packet);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Success));

      -- Now a tick should release a partial packet:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 1);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (1);
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Send the command again:
      T.Command_T_Send (Commands.Send_Packet);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Success));

      -- Since there the packet is not full, expect no additional packets:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send the command again:
      T.Command_T_Send (Commands.Send_Packet);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Success));

      -- Send an event:
      T.Event_T_Send (Event_2);

      -- Now a tick should release a partial packet:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 2);

      -- Check the packet header:
      The_Packet := T.Events_Packet_History.Get (2);
      Expected_Packet_Header.Buffer_Length := Event_Header.T'Object_Size / 8 + Event_2.Header.Param_Buffer_Length;
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_2, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Make sure no events were dropped:
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 0);
   end Test_Commanded_Packetization;

   overriding procedure Test_Dropped_Events (Self : in out Instance) is
      T : Component.Event_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((1, 2), 3);
      Expected_Packet_Header : Packet_Header.T := (Time => T.System_Time, Id => 0, Sequence_Count => 0, Buffer_Length => Packet_Types.Packet_Buffer_Type'Length);
      The_Packet : Packet.T;
      P_Idx : Natural;
      Bytes_Sent : Natural := 0;
   begin
      -- Initialize the component with 2 internal packets, and no packet timeout:
      T.Component_Instance.Init (Num_Internal_Packets => 2, Partial_Packet_Timeout => 0);

      -- OK, fill up a packet, and make sure that no packets are dropped:
      for Idx in 1 .. ((Packet_Types.Packet_Buffer_Type'Length / (Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length))) loop
         T.Event_T_Send (Event_3);
         Bytes_Sent := @ + (Event_Header.Serialization.Serialized_Length + Event_3.Header.Param_Buffer_Length);
         Boolean_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Is_Empty, True);
      end loop;

      -- OK, fill up a second packet, and make sure that no packets are dropped:
      for Idx in 1 .. ((Packet_Types.Packet_Buffer_Type'Length / (Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length))) loop
         T.Event_T_Send (Event_3);
         Boolean_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Is_Empty, True);
      end loop;

      -- Make sure no data product packet is emitted.
      Boolean_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Is_Empty, True);

      -- OK, next event should be dropped:
      T.Event_T_Send (Event_2);

      -- Send a tick, and expect a dropped event data product:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 1);
      Natural_Assert.Eq (T.Bytes_Available_History.Get_Count, 1);

      -- Check data product:
      Packed_U32_Assert.Eq (T.Events_Dropped_Count_History.Get (1), (Value => 1));
      Packed_Natural_Assert.Eq (T.Bytes_Available_History.Get (1), (Value => 0));

      -- Check the packet header:
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 1);
      The_Packet := T.Events_Packet_History.Get (1);
      Expected_Packet_Header.Buffer_Length := Bytes_Sent;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Send another tick, expect no dropped packet, since packet has been released:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 1);
      Natural_Assert.Eq (T.Bytes_Available_History.Get_Count, 2);
      Packed_Natural_Assert.Eq (T.Bytes_Available_History.Get (2), (Value => Packet_Types.Packet_Buffer_Type'Length));

      -- Check the packet header:
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 2);
      The_Packet := T.Events_Packet_History.Get (2);
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- OK, fill up a packets again, and make sure that no packets are dropped:
      for Idx in 1 .. ((Packet_Types.Packet_Buffer_Type'Length / (Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length))) loop
         T.Event_T_Send (Event_3);
         Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 1);
      end loop;

      for Idx in 1 .. ((Packet_Types.Packet_Buffer_Type'Length / (Event_Header.T'Object_Size / 8 + Event_3.Header.Param_Buffer_Length))) loop
         T.Event_T_Send (Event_3);
         Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 1);
      end loop;

      -- OK, the next 5 events should be dropped:
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);

      -- Send a tick, and expect a dropped event data product:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 2);

      -- Check data product:
      Packed_U32_Assert.Eq (T.Events_Dropped_Count_History.Get (2), (Value => 6));

      -- Check the packet header:
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 3);
      The_Packet := T.Events_Packet_History.Get (3);
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Send a tick, and expect a final packet to be sent, but no DP:
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 2);

      -- Check the packet header:
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 4);
      The_Packet := T.Events_Packet_History.Get (4);
      Expected_Packet_Header.Sequence_Count := @ + 1;
      Packet_Header_Assert.Eq (The_Packet.Header, Expected_Packet_Header);

      -- Check the packet contents:
      P_Idx := Packet_Types.Packet_Buffer_Type'First;
      while P_Idx <= Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length - 1 loop
         P_Idx := Check_Event (The_Packet, Event_3, @);
      end loop;
      Natural_Assert.Eq (P_Idx, Packet_Types.Packet_Buffer_Type'First + Expected_Packet_Header.Buffer_Length);

      -- Send a few ticks, and expect nothing to be sent:
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 4);
   end Test_Dropped_Events;

   overriding procedure Uninitialized (Self : in out Instance) is
      T : Component.Event_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((1, 2), 3);
   begin
      -- OK, send events to uninitialized component:
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);
      T.Event_T_Send (Event_2);

      -- Initialize the component with 2 internal packets, and no packet timeout:
      T.Component_Instance.Init (Num_Internal_Packets => 2, Partial_Packet_Timeout => 0);

      -- Send tick and expect data product to be emitted:
      T.Tick_T_Send (A_Tick);

      -- Make sure no data product packet is emitted.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Events_Dropped_Count_History.Get_Count, 1);

      -- Check data product:
      Packed_U32_Assert.Eq (T.Events_Dropped_Count_History.Get (1), (Value => 5));

      -- Make sure no packet was sent out:
      Natural_Assert.Eq (T.Events_Packet_History.Get_Count, 0);
   end Uninitialized;

end Event_Packetizer_Tests.Implementation;
