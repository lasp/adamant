--------------------------------------------------------------------------------
-- Logger Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Safe_Deallocator;

with Circular_Buffer_Meta.Assertion; use Circular_Buffer_Meta.Assertion;
with Basic_Types.Representation;
with Logger_Info.Assertion; use Logger_Info.Assertion;
with Basic_Assertions; use Basic_Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Memory_Packetizer_Types;
with Packet_Types;
with Byte_Array_Pointer.Packed;
with Smart_Assert;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Logger_Enums.Assertion; use Logger_Enums;
use Logger_Enums.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Interfaces; use Interfaces;

package body Logger_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Dynamically allocate the generic component tester:
      Self.Tester := new Component_Tester_Package.Instance;

      -- Set the logger in the component
      Self.Tester.Set_Logger (Self.Logger'Unchecked_Access);

      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Component_Tester_Package.Instance, Name => Component_Tester_Package.Instance_Access);
   begin
      -- Free component heap:
      Self.Tester.Component_Instance.Final;
      Self.Tester.Final_Base;

      -- Free the tester:
      Free_If_Testing (Self.Tester);
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   procedure Check_Dump (Dump : in Memory_Packetizer_Types.Memory_Dump; Id : in Packet_Types.Packet_Id; Len : in Natural; Not_Null : in Boolean := True; Filename : in String := Smart_Assert.Sinfo.File; Lin : in Natural := Smart_Assert.Sinfo.Line) is
      use Packet_Types;
      use Byte_Array_Pointer;
   begin
      pragma Assert (Dump.Id = Id, "Check dump failed checking id " & Packet_Types.Packet_Id'Image (Dump.Id) & " = " & Packet_Types.Packet_Id'Image (Id) & " " & Filename & ":" & Natural'Image (Lin));
      if Not_Null then
         pragma Assert (not Is_Null (Dump.Memory_Pointer), "Check dump failed checking ptr not null at " & Filename & ":" & Natural'Image (Lin));
      else
         pragma Assert (Is_Null (Dump.Memory_Pointer), "Check dump failed checking ptr = null at " & Filename & ":" & Natural'Image (Lin));
      end if;
      pragma Assert (Byte_Array_Pointer.Length (Dump.Memory_Pointer) = Len, "Check dump failed checking length at " & Filename & ":" & Natural'Image (Lin));
   end Check_Dump;

   procedure Check_Meta_Dump (Dump : in Memory_Packetizer_Types.Memory_Dump; To_Compare : in Circular_Buffer_Meta.T; Filename : in String := Smart_Assert.Sinfo.File; Lin : in Natural := Smart_Assert.Sinfo.Line) is
      function To_Meta is new Byte_Array_Pointer.To_Type (Circular_Buffer_Meta.T, Circular_Buffer_Meta.Serialization.From_Byte_Array);
   begin
      Check_Dump (Dump, 0, Circular_Buffer_Meta.Serialization.Serialized_Length, True, Filename, Lin);
      Circular_Buffer_Meta_Assert.Eq (To_Meta (Dump.Memory_Pointer), To_Compare);
   end Check_Meta_Dump;

   overriding procedure Test_Log_And_Dump_Enabled (Self : in out Instance) is
      use Byte_Array_Pointer;
      use Byte_Array_Pointer.Packed;
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : Tick_32.T;
      Bytes : Basic_Types.Byte_Array := [0 .. 99 => 0];
      Bytes_To_Compare : Basic_Types.Byte_Array := [0 .. 99 => 0];
      Ptr : Byte_Array_Pointer.Instance;
      Idx : Natural := 0;
   begin
      -- Initialize the component:
      T.Component_Instance.Init (Size => 100, Initial_Mode => Logger_Mode.Enabled);

      -- Call the component setup method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- After initialization a data product should have been produced.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Mode_History.Get_Count, 1);
      Logger_Mode_Assert.Eq (T.Mode_History.Get (1).Current_Mode, Logger_Mode.Enabled);

      -- Send some data to the logger while it is disabled:
      The_Tick := ((1, 2), 3);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((4, 5), 6);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((7, 8), 9);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((10, 11), 12);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((13, 14), 15);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 1);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (1), ((Head => 0, Count => Unsigned_32 (Tick_32.Serialization.Serialized_Length) * 5, Size => 100), Current_Mode => Logger_Mode.Enabled));

      -- Dump the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Log);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Log_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 2);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 2);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (1), (Head => 0, Count => Unsigned_32 (Tick_32.Serialization.Serialized_Length) * 5, Size => 100));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (1), Pack (T.Memory_Dump_Recv_Sync_History.Get (1).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (2), 0, Tick_32.Serialization.Serialized_Length * 5);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (2), Pack (T.Memory_Dump_Recv_Sync_History.Get (2).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (2).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Length (Ptr) - 1), Bytes_To_Compare (0 .. Length (Ptr) - 1));

      -- Dump the log memory and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Log_Memory);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Log_Memory_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 4);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 4);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (3), (Head => 0, Count => Unsigned_32 (Tick_32.Serialization.Serialized_Length) * 5, Size => 100));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (3), Pack (T.Memory_Dump_Recv_Sync_History.Get (3).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (4), 0, 100);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (4), Pack (T.Memory_Dump_Recv_Sync_History.Get (4).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (4).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Length (Ptr) - 1), Bytes_To_Compare (0 .. Length (Ptr) - 1));

      -- Dump the oldest 25 bytes of the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Oldest_Data ((Length => 25)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Oldest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 6);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 6);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (5), (Head => 0, Count => Unsigned_32 (Tick_32.Serialization.Serialized_Length) * 5, Size => 100));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (5), Pack (T.Memory_Dump_Recv_Sync_History.Get (5).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (6), 0, 25);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (6), Pack (T.Memory_Dump_Recv_Sync_History.Get (6).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (6).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Length (Ptr) - 1), Bytes_To_Compare (0 .. Length (Ptr) - 1));

      -- Dump the newest 24 bytes of the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Newest_Data ((Length => 24)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Newest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 8);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 8);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (7), (Head => 0, Count => Unsigned_32 (Tick_32.Serialization.Serialized_Length) * 5, Size => 100));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (7), Pack (T.Memory_Dump_Recv_Sync_History.Get (7).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (8), 0, 24);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (8), Pack (T.Memory_Dump_Recv_Sync_History.Get (8).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (8).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Length (Ptr) - 1), Bytes_To_Compare (Idx - 24 .. Idx - 1));

      -- No more dp updates expected:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
   end Test_Log_And_Dump_Enabled;

   overriding procedure Test_Log_And_Dump_Disabled (Self : in out Instance) is
      use Byte_Array_Pointer;
      use Byte_Array_Pointer.Packed;
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : Tick_32.T;
   begin
      -- Initialize the component:
      T.Component_Instance.Init (Size => 50);

      -- Call the component setup method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- After initialization a data product should have been produced.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Mode_History.Get_Count, 1);
      Logger_Mode_Assert.Eq (T.Mode_History.Get (1).Current_Mode, Logger_Mode.Disabled);

      -- Send some data to the logger while it is disabled:
      The_Tick := ((1, 2), 3);
      T.T_Send (The_Tick);
      The_Tick := ((4, 5), 6);
      T.T_Send (The_Tick);
      The_Tick := ((7, 8), 9);
      T.T_Send (The_Tick);
      The_Tick := ((10, 11), 12);
      T.T_Send (The_Tick);
      The_Tick := ((13, 14), 15);
      T.T_Send (The_Tick);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 1);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (1), ((Head => 0, Count => 0, Size => 50), Current_Mode => Logger_Mode.Disabled));

      -- Dump the log and make sure only the meta data was sent:
      T.Command_T_Send (T.Commands.Dump_Log);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Log_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 1);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 1);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (1), (Head => 0, Count => 0, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (1), Pack (T.Memory_Dump_Recv_Sync_History.Get (1).Memory_Pointer));

      -- Dump the log memory:
      T.Command_T_Send (T.Commands.Dump_Log_Memory);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Log_Memory_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 3);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 3);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (2), (Head => 0, Count => 0, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (2), Pack (T.Memory_Dump_Recv_Sync_History.Get (2).Memory_Pointer));

      -- Check the dumped memory:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (3), 0, 50);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (3), Pack (T.Memory_Dump_Recv_Sync_History.Get (3).Memory_Pointer));

      -- Dump the oldest data:
      T.Command_T_Send (T.Commands.Dump_Oldest_Data ((Length => 17)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Oldest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 4);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 4);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (4), (Head => 0, Count => 0, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (4), Pack (T.Memory_Dump_Recv_Sync_History.Get (4).Memory_Pointer));

      -- Dump the newest data:
      T.Command_T_Send (T.Commands.Dump_Newest_Data ((Length => 14)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Newest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 5);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 5);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (5), (Head => 0, Count => 0, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (5), Pack (T.Memory_Dump_Recv_Sync_History.Get (5).Memory_Pointer));

   end Test_Log_And_Dump_Disabled;

   overriding procedure Test_Log_Overwrite_And_Dump (Self : in out Instance) is
      use Byte_Array_Pointer;
      use Byte_Array_Pointer.Packed;
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : Tick_32.T;
      Bytes : Basic_Types.Byte_Array := [0 .. 99 => 0];
      Bytes_To_Compare : Basic_Types.Byte_Array := [0 .. 99 => 0];
      Ptr : Byte_Array_Pointer.Instance;
      Idx : Natural := 0;
      Len : Natural := 0;
   begin
      -- Initialize the component:
      T.Component_Instance.Init (Size => 50, Initial_Mode => Logger_Mode.Enabled);

      -- Call the component setup method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- After initialization a data product should have been produced.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Mode_History.Get_Count, 1);
      Logger_Mode_Assert.Eq (T.Mode_History.Get (1).Current_Mode, Logger_Mode.Enabled);

      -- Send some data to the logger while it is disabled:
      The_Tick := ((1, 2), 3);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((4, 5), 6);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((7, 8), 9);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((10, 11), 12);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;
      The_Tick := ((13, 14), 15);
      T.T_Send (The_Tick);
      Bytes_To_Compare (Idx .. Idx + Tick_32.Serialization.Serialized_Length - 1) := Tick_32.Serialization.To_Byte_Array (The_Tick);
      Idx := @ + Tick_32.Serialization.Serialized_Length;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 1);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (1), ((Head => (Unsigned_32 (Tick_32.Serialization.Serialized_Length) * 5) mod 50, Count => 50, Size => 50), Current_Mode => Logger_Mode.Enabled));

      -- Dump the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Log);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Log_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 3);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 3);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (1), (Head => Unsigned_32 (Tick_32.Serialization.Serialized_Length * 5) mod 50, Count => 50, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (1), Pack (T.Memory_Dump_Recv_Sync_History.Get (1).Memory_Pointer));

      -- Check the first dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (2), 0, 40);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (2), Pack (T.Memory_Dump_Recv_Sync_History.Get (2).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (2).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Len := Length (Ptr);

      -- Check the first dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (3), 0, 10);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (3), Pack (T.Memory_Dump_Recv_Sync_History.Get (3).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (3).Memory_Pointer;
      Bytes (Len .. Len + Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Len + Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Len + Length (Ptr) - 1), Bytes_To_Compare (Idx - 50 .. Idx - 1));

      -- Dump the log memory and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Log_Memory);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Log_Memory_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 5);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 5);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (4), (Head => Unsigned_32 (Tick_32.Serialization.Serialized_Length * 5) mod 50, Count => 50, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (4), Pack (T.Memory_Dump_Recv_Sync_History.Get (4).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (5), 0, 50);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (5), Pack (T.Memory_Dump_Recv_Sync_History.Get (5).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (5).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (10 .. 49), Bytes_To_Compare (10 .. 49));
      Byte_Array_Assert.Eq (Bytes (0 .. 9), Bytes_To_Compare (50 .. 59));

      -- Dump the oldest 42 bytes of the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Oldest_Data ((Length => 42)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Oldest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 8);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 8);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (6), (Head => Unsigned_32 (Tick_32.Serialization.Serialized_Length * 5) mod 50, Count => 50, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (6), Pack (T.Memory_Dump_Recv_Sync_History.Get (6).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (7), 0, 40);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (7), Pack (T.Memory_Dump_Recv_Sync_History.Get (7).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (7).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Len := Length (Ptr);

      -- Check the first dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (8), 0, 2);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (8), Pack (T.Memory_Dump_Recv_Sync_History.Get (8).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (8).Memory_Pointer;
      Bytes (Len .. Len + Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Len + Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Len + Length (Ptr) - 1), Bytes_To_Compare (10 .. 51));

      -- Dump the oldest 15 bytes of the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Oldest_Data ((Length => 15)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Oldest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 10);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 10);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (9), (Head => Unsigned_32 (Tick_32.Serialization.Serialized_Length * 5) mod 50, Count => 50, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (9), Pack (T.Memory_Dump_Recv_Sync_History.Get (9).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (10), 0, 15);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (10), Pack (T.Memory_Dump_Recv_Sync_History.Get (10).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (10).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Len + Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Length (Ptr) - 1), Bytes_To_Compare (10 .. 24));

      -- Dump the newest 18 bytes of the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Newest_Data ((Length => 18)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Newest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 14);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 13);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 13);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (11), (Head => Unsigned_32 (Tick_32.Serialization.Serialized_Length * 5) mod 50, Count => 50, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (11), Pack (T.Memory_Dump_Recv_Sync_History.Get (11).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (12), 0, 8);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (12), Pack (T.Memory_Dump_Recv_Sync_History.Get (12).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (12).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Len := Length (Ptr);

      -- Check the first dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (13), 0, 10);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (13), Pack (T.Memory_Dump_Recv_Sync_History.Get (13).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (13).Memory_Pointer;
      Bytes (Len .. Len + Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Len + Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Len + Length (Ptr) - 1), Bytes_To_Compare (Idx - 18 .. Idx - 1));

      -- Dump the newest 9 bytes of the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Newest_Data ((Length => 9)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 7);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Newest_Data_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 16);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 15);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 15);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (14), (Head => Unsigned_32 (Tick_32.Serialization.Serialized_Length * 5) mod 50, Count => 50, Size => 50));
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (14), Pack (T.Memory_Dump_Recv_Sync_History.Get (14).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (15), 0, 9);
      -- Make sure pointer sent in event matches the pointer send via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (15), Pack (T.Memory_Dump_Recv_Sync_History.Get (15).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (15).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Len + Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Length (Ptr) - 1), Bytes_To_Compare (Idx - 9 .. Idx - 1));
   end Test_Log_Overwrite_And_Dump;

   overriding procedure Test_Enable_Disable (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : Tick_32.T;
   begin
      -- Initialize the component:
      T.Component_Instance.Init (Size => 100);

      -- Call the component setup method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- After initialization a data product should have been produced.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Mode_History.Get_Count, 1);
      Logger_Mode_Assert.Eq (T.Mode_History.Get (1).Current_Mode, Logger_Mode.Disabled);

      -- Send some data to the logger while it is disabled:
      The_Tick := ((1, 2), 3);
      T.T_Send (The_Tick);
      The_Tick := ((4, 5), 6);
      T.T_Send (The_Tick);
      The_Tick := ((7, 8), 9);
      T.T_Send (The_Tick);
      The_Tick := ((10, 11), 12);
      T.T_Send (The_Tick);
      The_Tick := ((13, 14), 15);
      T.T_Send (The_Tick);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 1);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (1), ((Head => 0, Count => 0, Size => 100), Current_Mode => Logger_Mode.Disabled));

      -- Send enabled command:
      T.Command_T_Send (T.Commands.Enable);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Log_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Mode_History.Get_Count, 2);
      Logger_Mode_Assert.Eq (T.Mode_History.Get (2).Current_Mode, Logger_Mode.Enabled);

      -- Send some data to the logger while it is disabled:
      The_Tick := ((1, 2), 3);
      T.T_Send (The_Tick);
      The_Tick := ((4, 5), 6);
      T.T_Send (The_Tick);
      The_Tick := ((7, 8), 9);
      T.T_Send (The_Tick);
      The_Tick := ((10, 11), 12);
      T.T_Send (The_Tick);
      The_Tick := ((13, 14), 15);
      T.T_Send (The_Tick);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 2);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (2), ((Head => 0, Count => Interfaces.Unsigned_32 (Tick_32.Size_In_Bytes * 5), Size => 100), Current_Mode => Logger_Mode.Enabled));

      -- Send disable command:
      T.Command_T_Send (T.Commands.Disable);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Log_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Log_Disabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Mode_History.Get_Count, 3);
      Logger_Mode_Assert.Eq (T.Mode_History.Get (3).Current_Mode, Logger_Mode.Disabled);

      -- Send some data to the logger while it is disabled:
      The_Tick := ((1, 2), 3);
      T.T_Send (The_Tick);
      The_Tick := ((4, 5), 6);
      T.T_Send (The_Tick);
      The_Tick := ((7, 8), 9);
      T.T_Send (The_Tick);
      The_Tick := ((10, 11), 12);
      T.T_Send (The_Tick);
      The_Tick := ((13, 14), 15);
      T.T_Send (The_Tick);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 3);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (3), ((Head => 0, Count => Interfaces.Unsigned_32 (Tick_32.Size_In_Bytes * 5), Size => 100), Current_Mode => Logger_Mode.Disabled));

      -- Send enabled command:
      T.Command_T_Send (T.Commands.Enable);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 6);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (6), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Log_Enabled_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Mode_History.Get_Count, 4);
      Logger_Mode_Assert.Eq (T.Mode_History.Get (4).Current_Mode, Logger_Mode.Enabled);

      -- Send some data to the logger while it is enabled:
      The_Tick := ((1, 2), 3);
      T.T_Send (The_Tick);
      The_Tick := ((4, 5), 6);
      T.T_Send (The_Tick);
      The_Tick := ((7, 8), 9);
      T.T_Send (The_Tick);
      The_Tick := ((10, 11), 12);
      T.T_Send (The_Tick);
      The_Tick := ((13, 14), 15);
      T.T_Send (The_Tick);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 7);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (7), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 4);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (4), ((Head => Interfaces.Unsigned_32 ((Tick_32.Size_In_Bytes * 10) mod 100), Count => 100, Size => 100), Current_Mode => Logger_Mode.Enabled));
   end Test_Enable_Disable;

   overriding procedure Test_Init (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;

      procedure Init_Nominal_Heap is
      begin
         T.Component_Instance.Init (Size => 50);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init heap failed!");
      end Init_Nominal_Heap;

      procedure Init_Nominal_Static is
         Bytes : aliased Basic_Types.Byte_Array := [0 .. 50 => 0];
         Meta_Data : aliased Circular_Buffer_Meta.T;
      begin
         T.Component_Instance.Init (Bytes => Bytes'Unchecked_Access, Meta_Data => Meta_Data'Unchecked_Access);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init static failed!");
      end Init_Nominal_Static;

      procedure Init_Nominal_Mode is
         Bytes : aliased Basic_Types.Byte_Array := [0 .. 50 => 0];
         Meta_Data : aliased Circular_Buffer_Meta.T;
      begin
         T.Component_Instance.Init (Bytes => Bytes'Unchecked_Access, Meta_Data => Meta_Data'Unchecked_Access, Initial_Mode => Logger_Mode.Enabled);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init static failed!");
      end Init_Nominal_Mode;

      procedure Init_Everything is
         Bytes : aliased Basic_Types.Byte_Array := [0 .. 50 => 0];
         Meta_Data : aliased Circular_Buffer_Meta.T;
      begin
         T.Component_Instance.Init (Bytes => Bytes'Unchecked_Access, Meta_Data => Meta_Data'Unchecked_Access, Size => 50);
         -- Should never get here:
         Assert (False, "Init everything did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Everything;

      procedure Init_Nothing is
      begin
         T.Component_Instance.Init;
         -- Should never get here:
         Assert (False, "Init nothing did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Nothing;

      procedure Init_Some_1 is
         Bytes : aliased Basic_Types.Byte_Array := [0 .. 50 => 0];
      begin
         T.Component_Instance.Init (Bytes => Bytes'Unchecked_Access);
         -- Should never get here:
         Assert (False, "Init some 1 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Some_1;

      procedure Init_Some_2 is
         Meta_Data : aliased Circular_Buffer_Meta.T;
      begin
         T.Component_Instance.Init (Meta_Data => Meta_Data'Unchecked_Access);
         -- Should never get here:
         Assert (False, "Init some 1 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Some_2;

      procedure Init_Some_3 is
         Bytes : aliased Basic_Types.Byte_Array := [0 .. 50 => 0];
      begin
         T.Component_Instance.Init (Bytes => Bytes'Unchecked_Access, Size => 40);
         -- Should never get here:
         Assert (False, "Init some 1 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Some_3;

      procedure Init_Some_4 is
         Meta_Data : aliased Circular_Buffer_Meta.T;
      begin
         T.Component_Instance.Init (Meta_Data => Meta_Data'Unchecked_Access, Size => 50);
         -- Should never get here:
         Assert (False, "Init some 1 did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Some_4;
   begin
      Init_Nominal_Heap;
      Init_Nominal_Static;
      Init_Nominal_Mode;
      Init_Everything;
      Init_Nothing;
      Init_Some_1;
      Init_Some_2;
      Init_Some_3;
      Init_Some_4;
   end Test_Init;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Dump_Oldest_Data ((Length => 25));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Oldest_Data_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Dump_Oldest_Data_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Logger_Tests.Implementation;
