--------------------------------------------------------------------------------
-- Product_Packetizer Tests Body
--------------------------------------------------------------------------------

with Tick;
with Packet_Types;
with Sys_Time;
with Packed_U32;
with Product_Packetizer_Commands;
with Basic_Assertions; use Basic_Assertions;
with Packet.Assertion; use Packet.Assertion;
with Packet_Data_Product_Ids.Assertion; use Packet_Data_Product_Ids.Assertion;
with Packet_Period.Assertion; use Packet_Period.Assertion;
with Invalid_Packet_Id.Assertion; use Invalid_Packet_Id.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Packed_U16;
with Smart_Assert;
with Data_Product_Enums; use Data_Product_Enums;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Invalid_Data_Product_Length.Assertion; use Invalid_Data_Product_Length.Assertion;
with Command_Types;
with Command_Header.Assertion; use Command_Header.Assertion;
with Command;
with Packed_Natural;
with Test_Assembly_Product_Packets_Test_Packets;

package body Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Initialize the component:
      Self.Tester.Component_Instance.Init;

      -- Set the desired time for the tests
      Self.Tester.System_Time := (3, 17);

      -- Set count to zero:
      Self.Tester.Reset_Count;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helper functions to check the contents of packets:
   -------------------------------------------------------------------------

   procedure Check_Packet_1 (Self : in Instance; To_Compare : Packet.T; Expected_Sequence_Count : Packet_Types.Sequence_Count_Mod_Type; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Expected : Packet.T := (Header => (Time => T.System_Time, Id => 7, Sequence_Count => Expected_Sequence_Count, Buffer_Length => 0), Buffer => [others => 0]);
      Idx : Natural := Expected.Buffer'First;
      End_Idx : Natural := 0;
   begin
      -- First serialize time stamp:
      End_Idx := Idx + Sys_Time.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Sys_Time.Serialization.To_Byte_Array ((5, 11));
      Idx := End_Idx + 1;

      -- Next Serialize Data product A:
      End_Idx := Idx + Packed_U32.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_U32.Serialization.To_Byte_Array ((Value => 23));
      Idx := End_Idx + 1;

      -- Next serialize Data product C:
      End_Idx := Idx + Tick.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Tick.Serialization.To_Byte_Array (((5, 11), 14));
      Idx := End_Idx + 1;

      -- Calculate packet length:
      Expected.Header.Buffer_Length := Idx - Expected.Buffer'First;

      -- Compare packets:
      Packet_Assert.Eq (To_Compare, Expected, "", Filename, Line);
   end Check_Packet_1;

   procedure Check_Packet_Zeros (Self : in Instance; To_Compare : Packet.T; Expected_Sequence_Count : Packet_Types.Sequence_Count_Mod_Type; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Expected : Packet.T := (Header => (Time => T.System_Time, Id => 7, Sequence_Count => Expected_Sequence_Count, Buffer_Length => 0), Buffer => [others => 0]);
      Idx : Natural := Expected.Buffer'First;
      End_Idx : Natural := 0;
   begin
      -- First serialize time stamp:
      End_Idx := Idx + Sys_Time.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Sys_Time.Serialization.To_Byte_Array ((0, 0));
      Idx := End_Idx + 1;

      -- Next Serialize Data product A:
      End_Idx := Idx + Packed_U32.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_U32.Serialization.To_Byte_Array ((Value => 0));
      Idx := End_Idx + 1;

      -- Next serialize Data product C:
      End_Idx := Idx + Tick.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Tick.Serialization.To_Byte_Array (((0, 0), 0));
      Idx := End_Idx + 1;

      -- Calculate packet length:
      Expected.Header.Buffer_Length := Idx - Expected.Buffer'First;

      -- Compare packets:
      Packet_Assert.Eq (To_Compare, Expected, "", Filename, Line);
   end Check_Packet_Zeros;

   procedure Check_Packet_2 (Self : in Instance; To_Compare : Packet.T; Expected_Sequence_Count : Packet_Types.Sequence_Count_Mod_Type; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
      Ignore : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Expected : Packet.T := (Header => (Time => (5, 11), Id => 9, Sequence_Count => Expected_Sequence_Count, Buffer_Length => 0), Buffer => [others => 0]);
      Idx : Natural := Expected.Buffer'First;
      End_Idx : Natural := 0;
   begin
      -- Next Serialize Data product A:
      End_Idx := Idx + Packed_U16.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_U16.Serialization.To_Byte_Array ((Value => 33));
      Idx := End_Idx + 1;

      -- Next serialize Data product C:
      End_Idx := Idx + Tick.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Tick.Serialization.To_Byte_Array (((5, 11), 13));
      Idx := End_Idx + 1;

      -- Calculate packet length:
      Expected.Header.Buffer_Length := Idx - Expected.Buffer'First;

      -- Compare packets:
      Packet_Assert.Eq (To_Compare, Expected, "", Filename, Line);
   end Check_Packet_2;

   procedure Check_Packet_3 (Self : in Instance; To_Compare : Packet.T; Expected_Sequence_Count : Packet_Types.Sequence_Count_Mod_Type; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Expected : Packet.T := (Header => (Time => T.System_Time, Id => 8, Sequence_Count => Expected_Sequence_Count, Buffer_Length => 0), Buffer => [others => 0]);
      Idx : Natural := Expected.Buffer'First;
      End_Idx : Natural := 0;
   begin
      -- First serialize time stamp:
      End_Idx := Idx + Sys_Time.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Sys_Time.Serialization.To_Byte_Array ((5, 11));
      Idx := End_Idx + 1;

      -- Next Serialize Data product A:
      End_Idx := Idx + Packed_U32.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_U32.Serialization.To_Byte_Array ((Value => 23));
      Idx := End_Idx + 1;

      -- Next serialize Data product C:
      End_Idx := Idx + Tick.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Tick.Serialization.To_Byte_Array (((5, 11), 14));
      Idx := End_Idx + 1;

      -- Calculate packet length:
      Expected.Header.Buffer_Length := Idx - Expected.Buffer'First;

      -- Compare packets:
      Packet_Assert.Eq (To_Compare, Expected, "", Filename, Line);
   end Check_Packet_3;

   procedure Check_Packet_4 (Self : in Instance; To_Compare : Packet.T; The_Time : Sys_Time.T; Expected_Sequence_Count : Packet_Types.Sequence_Count_Mod_Type; Filename : in String := Smart_Assert.Sinfo.File; Line : in Natural := Smart_Assert.Sinfo.Line) is
      Ignore : Instance renames Self;
      Expected : Packet.T := (Header => (Time => The_Time, Id => 12, Sequence_Count => Expected_Sequence_Count, Buffer_Length => 0), Buffer => [others => 0]);
      Idx : Natural := Expected.Buffer'First;
      End_Idx : Natural := 0;
   begin
      -- First serialize 5 pad bytes:
      End_Idx := Idx + 5 - 1;
      Expected.Buffer (Idx .. End_Idx) := [0 .. 5 - 1 => 0];
      Idx := End_Idx + 1;

      -- Next Serialize Data product A:
      End_Idx := Idx + Packed_U32.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_U32.Serialization.To_Byte_Array ((Value => 23));
      Idx := End_Idx + 1;

      -- Next serialize 3 pad bytes:
      End_Idx := Idx + 3 - 1;
      Expected.Buffer (Idx .. End_Idx) := [0 .. 3 - 1 => 0];
      Idx := End_Idx + 1;

      -- Calculate packet length:
      Expected.Header.Buffer_Length := Idx - Expected.Buffer'First;

      -- Compare packets:
      Packet_Assert.Eq (To_Compare, Expected, "", Filename, Line);
   end Check_Packet_4;

   procedure Check_Packet_5
      (Self : in Instance; To_Compare : Packet.T; Expected_Sequence_Count : Packet_Types.Sequence_Count_Mod_Type; Packet_4_Period : in Natural; Packet_5_Period : in Natural; Packet_3_Period : in Natural; Filename : in String := Smart_Assert.Sinfo.File;
       Line : in Natural := Smart_Assert.Sinfo.Line)
   is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      Expected : Packet.T := (Header => (Time => T.System_Time, Id => 15, Sequence_Count => Expected_Sequence_Count, Buffer_Length => 0), Buffer => [others => 0]);
      Idx : Natural := Expected.Buffer'First;
      End_Idx : Natural := 0;
   begin
      -- First serialize time stamp:
      End_Idx := Idx + Packed_Natural.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_Natural.Serialization.To_Byte_Array ((Value => Packet_4_Period));
      Idx := End_Idx + 1;

      -- Next Serialize Data product A:
      End_Idx := Idx + Packed_Natural.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_Natural.Serialization.To_Byte_Array ((Value => Packet_5_Period));
      Idx := End_Idx + 1;

      -- Next serialize Data product C:
      End_Idx := Idx + Packed_Natural.Serialization.Serialized_Length - 1;
      Expected.Buffer (Idx .. End_Idx) := Packed_Natural.Serialization.To_Byte_Array ((Value => Packet_3_Period));
      Idx := End_Idx + 1;

      -- Calculate packet length:
      Expected.Header.Buffer_Length := Idx - Expected.Buffer'First;

      -- Compare packets:
      Packet_Assert.Eq (To_Compare, Expected, "", Filename, Line);
   end Check_Packet_5;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Packetizing (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;
   begin
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_1 (Self, The_Packet, 0);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (2);
      Check_Packet_1 (Self, The_Packet, 1);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Nominal_Packetizing;

   overriding procedure Test_Packet_Enable_Disable (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_1 (Self, The_Packet, 0);

      -- Send command to disable packet 1:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 7)));

      -- Send tick, expect command verification:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Disabled_History.Get_Count, 1);
      Packet_Period_Assert.Eq (T.Packet_Disabled_History.Get (1), (Id => 7, Period => 3));

      -- Send many ticks, expect no packets received:
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send command to enable packet 2:
      T.Command_T_Send (Commands.Enable_Packet ((Id => 9)));

      -- Send ticks and expect packet 2 every time:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Enabled_History.Get_Count, 1);
      Packet_Period_Assert.Eq (T.Packet_Enabled_History.Get (1), (Id => 9, Period => 1));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (4);
      Check_Packet_2 (Self, The_Packet, 2);

      -- Send command to disable packet 2:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 9)));

      -- Send tick, expect command verification:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Packet_Disabled_History.Get_Count, 2);
      Packet_Period_Assert.Eq (T.Packet_Disabled_History.Get (2), (Id => 9, Period => 1));

      -- Send many ticks, expect no packets received:
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Send command to enable packet 1:
      T.Command_T_Send (Commands.Enable_Packet ((Id => 7)));
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Packet_Enabled_History.Get_Count, 2);
      Packet_Period_Assert.Eq (T.Packet_Enabled_History.Get (2), (Id => 7, Period => 3));
   end Test_Packet_Enable_Disable;

   overriding procedure Test_Packet_Set_Period (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_1 (Self, The_Packet, 0);

      -- Send command to period to 1:
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 7, Period => 1)));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect event:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 1);
      Packet_Period_Assert.Eq (T.Packet_Period_Set_History.Get (1), (Id => 7, Period => 1));

      -- Send many ticks, expect packets for each:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 7);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (7);
      Check_Packet_1 (Self, The_Packet, 6);

      -- Send command to period to 3:
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 7, Period => 3)));

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 2);
      Packet_Period_Assert.Eq (T.Packet_Period_Set_History.Get (2), (Id => 7, Period => 3));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 7);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 7);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 8);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (8);
      Check_Packet_1 (Self, The_Packet, 7);
   end Test_Packet_Set_Period;

   overriding procedure Test_Missing_Data_Product (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;
      Pd_Ids : constant Packet_Data_Product_Ids.T := (Packet_Id => 7, Data_Product_Id => 1);
   begin
      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_1 (Self, The_Packet, 0);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Set the data product return status to not available:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Not_Available;

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (2);
      Check_Packet_Zeros (Self, The_Packet, 1);

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_Missing_On_Fetch_History.Get_Count, 1);
      Packet_Data_Product_Ids_Assert.Eq (T.Data_Product_Missing_On_Fetch_History.Get (1), Pd_Ids);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (3);
      Check_Packet_Zeros (Self, The_Packet, 2);

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_Missing_On_Fetch_History.Get_Count, 2);
      Packet_Data_Product_Ids_Assert.Eq (T.Data_Product_Missing_On_Fetch_History.Get (2), Pd_Ids);

      -- Set the data product return status to success:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Success;

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
   end Test_Missing_Data_Product;

   overriding procedure Test_Bad_Id_Data_Product (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;
   begin
      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_1 (Self, The_Packet, 0);

      -- Set the data product return status to not available:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Id_Out_Of_Range;

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (2);
      Check_Packet_Zeros (Self, The_Packet, 1);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (3);
      Check_Packet_Zeros (Self, The_Packet, 2);

      -- Set the data product return status to success:
      T.Data_Product_Fetch_Return_Status := Fetch_Status.Success;

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Bad_Id_Data_Product;

   overriding procedure Test_Data_Product_Size_Mismatch (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;
   begin
      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_1 (Self, The_Packet, 0);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Override length to induce an event.
      T.Data_Product_Length_Override := 17;
      T.Dp_Time := (0, 0); -- Do this to make checking for all zeros easier

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (2);
      Check_Packet_Zeros (Self, The_Packet, 1);

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get_Count, 2);
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (1), ((Time => (0, 0), Id => 0, Buffer_Length => 17), Expected_Length => Packed_U32.Size_In_Bytes));
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (2), ((Time => (0, 0), Id => 2, Buffer_Length => 17), Expected_Length => Tick.Size_In_Bytes));

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (3);
      Check_Packet_Zeros (Self, The_Packet, 2);

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get_Count, 4);
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (3), ((Time => (0, 0), Id => 0, Buffer_Length => 17), Expected_Length => Packed_U32.Size_In_Bytes));
      Invalid_Data_Product_Length_Assert.Eq (T.Data_Product_Length_Mismatch_History.Get (4), ((Time => (0, 0), Id => 2, Buffer_Length => 17), Expected_Length => Tick.Size_In_Bytes));

      -- Stop overriding length:
      T.Data_Product_Length_Override := 0;

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
   end Test_Data_Product_Size_Mismatch;

   -- This unit tests the component's behavior when rolling over its internal count.
   overriding procedure Test_Roll_Over (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      Scale_Factor : constant Natural := Natural'Last / 3;
      Roll_Over_Value : constant Natural := Scale_Factor * 3;
      Set_Value : constant Natural := Roll_Over_Value - 5;
   begin
      -- Check roll over value:
      Natural_Assert.Eq (T.Get_Roll_Over, Roll_Over_Value);

      -- Make sure count starts at 1:
      Natural_Assert.Eq (T.Get_Count, 1);

      -- Send some ticks and make sure it increments:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Get_Count, 2);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Get_Count, 3);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Get_Count, 4);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Get_Count, 5);

      -- Set the tick close to the roll over value:
      T.Set_Count (Set_Value);
      Natural_Assert.Eq (T.Get_Count, Set_Value);

      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Get_Count, Set_Value + 1);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Get_Count, Set_Value + 2);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Get_Count, Set_Value + 3);

      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Get_Count, Set_Value + 4);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Get_Count, Set_Value + 5);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Get_Count, 1);

      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Get_Count, 2);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Get_Count, 3);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Get_Count, 4);

      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Get_Count, 5);

   end Test_Roll_Over;

   overriding procedure Test_Bad_Commands (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      -- Send set packet period command with invalid id:
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 4, Period => 1)));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect event:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get_Count, 1);
      Invalid_Packet_Id_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get (1), (Packet_Id => 4, Command_Id => Commands.Get_Set_Packet_Period_Id));

      -- Send enable command with invalid id:
      T.Command_T_Send (Commands.Enable_Packet ((Id => 6)));

      -- Send tick, expect event:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get_Count, 2);
      Invalid_Packet_Id_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get (2), (Packet_Id => 6, Command_Id => Commands.Get_Enable_Packet_Id));

      -- Send disable command with invalid id:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 5)));

      -- Send tick, expect event:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get_Count, 3);
      Invalid_Packet_Id_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get (3), (Packet_Id => 5, Command_Id => Commands.Get_Disable_Packet_Id));

      -- Send send command with invalid id:
      T.Command_T_Send (Commands.Send_Packet ((Id => 5)));

      -- Send tick, expect event:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get_Count, 4);
      Invalid_Packet_Id_Assert.Eq (T.Invalid_Packet_Id_Commanded_History.Get (4), (Packet_Id => 5, Command_Id => Commands.Get_Send_Packet_Id));

      -- Send command with packet period over the size of a natural and expect the command argument validation to fail:
      T.Command_T_Send (Commands.Set_Packet_Period (Packet_Period.Serialization.From_Byte_Array ([255, 255, 255, 255, 255, 255])));

      -- Send tick, expect event:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Validation_Error));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => Commands.Get_Set_Packet_Period_Id, Errant_Field_Number => 2, Errant_Field => [0, 0, 0, 0, 255, 255, 255, 255]));
   end Test_Bad_Commands;

   overriding procedure Test_Send_Packet_Command (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_1 (Self, The_Packet, 0);

      -- Send command to send first packet:
      T.Command_T_Send (Commands.Send_Packet ((Id => 7)));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect packet:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (2);
      Check_Packet_1 (Self, The_Packet, 1);

      -- Send command to send second packet:
      T.Command_T_Send (Commands.Send_Packet ((Id => 9)));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect packet:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (3);
      Check_Packet_2 (Self, The_Packet, 0);

      -- Send command to send first packet:
      -- This command is timed with the packet period. We still
      -- only want to send 1 packet, not two.
      T.Command_T_Send (Commands.Send_Packet ((Id => 7)));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (4);
      Check_Packet_1 (Self, The_Packet, 2);

      -- Send 3 ticks, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (5);
      Check_Packet_1 (Self, The_Packet, 3);

   end Test_Send_Packet_Command;

   overriding procedure Test_Offset (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      -- We just want to test packet 3, since it is the only one with
      -- a positive offset value.
      -- Send command to disable packet 1:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 7)));
      -- Send command to enable packet 3:
      T.Command_T_Send (Commands.Enable_Packet ((Id => 8)));

      -- Send a few ticks, and expect to get the packet on tick 2 instead of 3 and then
      -- again on 5:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- OK now check the packets:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_3 (Self, The_Packet, 0);
      The_Packet := T.Packet_T_Recv_Sync_History.Get (2);
      Check_Packet_3 (Self, The_Packet, 1);
      The_Packet := T.Packet_T_Recv_Sync_History.Get (3);
      Check_Packet_3 (Self, The_Packet, 2);

      -- Send command to disable packet:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 8)));
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
   end Test_Offset;

   overriding procedure Test_Padding (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((15, 77), 1);
      The_Packet : Packet.T;

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      -- We just want to test packet 12, since it is the only one with
      -- padding
      -- Send command to disable packet 1:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 7)));
      -- Send command to enable packet 3:
      T.Command_T_Send (Commands.Enable_Packet ((Id => 12)));

      -- Send a tick:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- OK now check the packet. This also tests the use_tick_timestamp feature.
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_4 (Self, The_Packet, The_Tick.Time, 0);

      -- Send command to disable packet 3:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 12)));
      T.Command_T_Send (Commands.Disable_Packet ((Id => 7)));
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
   end Test_Padding;

   overriding procedure Test_Zero_Period (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);
      The_Packet : Packet.T;

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Send tick, expect no packets received:
      T.Tick_T_Send (The_Tick);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, True);

      -- Enable packet id 9:
      T.Command_T_Send (Commands.Enable_Packet ((Id => 9)));

      -- Send tick, expect to have received 1 packet:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Boolean_Assert.Eq (T.Packet_T_Recv_Sync_History.Is_Empty, False);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (1);
      Check_Packet_2 (Self, The_Packet, 0);

      -- Send command to set packet 1 period to zero:
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 9, Period => 0)));

      -- Send tick, expect command verification:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 1);
      Packet_Period_Assert.Eq (T.Packet_Period_Set_History.Get (1), (Id => 9, Period => 0));

      -- Send many ticks, expect no packets received:
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Send command to set packet 1 period to 1:
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 9, Period => 1)));

      -- Send tick, expect command verification:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 2);
      Packet_Period_Assert.Eq (T.Packet_Period_Set_History.Get (2), (Id => 9, Period => 1));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check the packet contents:
      The_Packet := T.Packet_T_Recv_Sync_History.Get (2);
      Check_Packet_2 (Self, The_Packet, 1);

      -- Disable 9:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 9)));
      T.Tick_T_Send (The_Tick);
   end Test_Zero_Period;

   overriding procedure Test_Full_Queue (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Define full sized command:
      Buffer : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 92];
      A_Command : constant Command.T := ((Source_Id => 0, Id => 15, Arg_Buffer_Length => Buffer'Length), Arg_Buffer => Buffer);

      procedure Fill_Queue (N : in Natural := 3) is
      begin
         for Idx in 1 .. N loop
            T.Command_T_Send (A_Command);
         end loop;
      end Fill_Queue;
   begin
      -- Fill the queue:
      Fill_Queue;

      -- Send another command and expect an event to be thrown:
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (A_Command);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Dropped_Command_History.Get (1), A_Command.Header);

      -- Send another command and expect an event to be thrown:
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (A_Command);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dropped_Command_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Dropped_Command_History.Get (2), A_Command.Header);
   end Test_Full_Queue;

   overriding procedure Test_Packet_Period_Items (Self : in out Instance) is
      T : Component.Product_Packetizer.Implementation.Tester.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 1);

      -- Command object:
      Commands : Product_Packetizer_Commands.Instance;
   begin
      -- Enable packet id 15:
      T.Command_T_Send (Commands.Disable_Packet ((Id => 7)));
      T.Command_T_Send (Commands.Disable_Packet ((Id => 15)));
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 15, Period => 1)));

      -- Check command response:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Packet_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));

      -- Change packet periods
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 8, Period => 44)));
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 12, Period => 55)));
      T.Command_T_Send (Commands.Enable_Packet ((Id => 15)));

      -- Check command response:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Packet_Id, Status => Success));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Self.Check_Packet_5 (T.Packet_T_Recv_Sync_History.Get (1), Expected_Sequence_Count => 0, Packet_4_Period => 55, Packet_5_Period => 1, Packet_3_Period => 44);

      -- Change packet periods
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 8, Period => 33)));
      T.Command_T_Send (Commands.Set_Packet_Period ((Id => 12, Period => 22)));

      -- Check command response:
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 8);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (8), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Self.Check_Packet_5 (T.Packet_T_Recv_Sync_History.Get (2), Expected_Sequence_Count => 1, Packet_4_Period => 22, Packet_5_Period => 1, Packet_3_Period => 33);

      --
      -- Error testing.
      --

      -- Set one of the special item IDs to something nonsensical.
      Test_Assembly_Product_Packets_Test_Packets.Packet_5_Items (3).Data_Product_Id := 999;

      -- Send tick:
      T.Event_T_Recv_Sync_History.Clear;
      T.Tick_T_Send (The_Tick);

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Self.Check_Packet_5 (T.Packet_T_Recv_Sync_History.Get (3), Expected_Sequence_Count => 2, Packet_4_Period => 22, Packet_5_Period => 1, Packet_3_Period => 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_Item_Bad_Id_History.Get_Count, 1);
      Packet_Data_Product_Ids_Assert.Eq (T.Packet_Period_Item_Bad_Id_History.Get (1), (Packet_Id => 15, Data_Product_Id => 999));
   end Test_Packet_Period_Items;

end Tests.Implementation;
