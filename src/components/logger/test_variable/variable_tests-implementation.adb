--------------------------------------------------------------------------------
-- Logger Tests Body
--------------------------------------------------------------------------------

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
with Serializer_Types;
with Logger_Error.Assertion; use Logger_Error.Assertion;
with Logger_Enums; use Logger_Enums;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Interfaces; use Interfaces;

package body Variable_Tests.Implementation is

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
      -- Free the tester component:
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Component_Tester_Package.Instance, Name => Component_Tester_Package.Instance_Access);
   begin
      -- Free component heap:
      Self.Tester.Component_Instance.Final;
      Self.Tester.Final_Base;

      -- Delete tester:
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

   overriding procedure Test_Log_And_Dump (Self : in out Instance) is
      use Byte_Array_Pointer;
      use Byte_Array_Pointer.Packed;
      use Serializer_Types;
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Var : Simple_Variable.T;
      Bytes : Basic_Types.Byte_Array := [0 .. 99 => 0];
      Bytes_To_Compare : Basic_Types.Byte_Array := [0 .. 99 => 0];
      Ptr : Byte_Array_Pointer.Instance;
      Idx : Natural := 0;
      Len : Natural := 0;
      Log_Bytes : aliased Basic_Types.Byte_Array := [0 .. 49 => 0];
      Meta_Data : aliased Circular_Buffer_Meta.T := (0, 0, 0);
   begin
      -- Initialize the component:
      T.Component_Instance.Init (Bytes => Log_Bytes'Unchecked_Access, Meta_Data => Meta_Data'Unchecked_Access, Initial_Mode => Logger_Mode.Enabled);

      -- Send some data to the logger while it is disabled:
      Var := (0, [0 => 1, 1 => 2, 2 => 3, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Success);
      pragma Assert (Simple_Variable.Serialization.To_Byte_Array (Bytes_To_Compare (Idx .. Idx + Len - 1), Var) = Success);
      Idx := @ + Len;
      Var := (1, [0 => 1, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Success);
      pragma Assert (Simple_Variable.Serialization.To_Byte_Array (Bytes_To_Compare (Idx .. Idx + Len - 1), Var) = Success);
      Idx := @ + Len;
      Var := (2, [0 => 2, 1 => 3, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Success);
      pragma Assert (Simple_Variable.Serialization.To_Byte_Array (Bytes_To_Compare (Idx .. Idx + Len - 1), Var) = Success);
      Idx := @ + Len;
      Var := (3, [0 => 4, 1 => 5, 2 => 6, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Success);
      pragma Assert (Simple_Variable.Serialization.To_Byte_Array (Bytes_To_Compare (Idx .. Idx + Len - 1), Var) = Success);
      Idx := @ + Len;
      Var := (4, [0 => 7, 1 => 8, 2 => 9, 3 => 10, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Success);
      pragma Assert (Simple_Variable.Serialization.To_Byte_Array (Bytes_To_Compare (Idx .. Idx + Len - 1), Var) = Success);
      Idx := @ + Len;

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 1);
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (1), ((Head => 0, Count => Unsigned_32 (Idx), Size => 50), Current_Mode => Logger_Mode.Enabled));

      -- Make sure our aliased meta data is getting written to.
      Circular_Buffer_Meta_Assert.Eq (Meta_Data, (Head => 0, Count => Unsigned_32 (Idx), Size => 50));

      -- Dump the log and make sure the meta data and data was sent:
      T.Command_T_Send (T.Commands.Dump_Log);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Log_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dumping_Log_Memory_History.Get_Count, 2);

      -- Check the dumped memory meta data:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 2);
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (1), (Head => 0, Count => Unsigned_32 (Idx), Size => 50));
      -- Make sure pointer sent in event matches the pointer sent via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (1), Pack (T.Memory_Dump_Recv_Sync_History.Get (1).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (2), 0, Idx);
      -- Make sure pointer sent in event matches the pointer sent via the dump:
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
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (3), (Head => 0, Count => Unsigned_32 (Idx), Size => 50));
      -- Make sure pointer sent in event matches the pointer sent via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (3), Pack (T.Memory_Dump_Recv_Sync_History.Get (3).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (4), 0, 50);
      -- Make sure pointer sent in event matches the pointer sent via the dump:
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
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (5), (Head => 0, Count => Unsigned_32 (Idx), Size => 50));
      -- Make sure pointer sent in event matches the pointer sent via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (5), Pack (T.Memory_Dump_Recv_Sync_History.Get (5).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (6), 0, Idx);
      -- Make sure pointer sent in event matches the pointer sent via the dump:
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
      Check_Meta_Dump (T.Memory_Dump_Recv_Sync_History.Get (7), (Head => 0, Count => Unsigned_32 (Idx), Size => 50));
      -- Make sure pointer sent in event matches the pointer sent via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (7), Pack (T.Memory_Dump_Recv_Sync_History.Get (7).Memory_Pointer));

      -- Check the dumped memory data:
      Check_Dump (T.Memory_Dump_Recv_Sync_History.Get (8), 0, Idx);
      -- Make sure pointer sent in event matches the pointer sent via the dump:
      Memory_Region_Assert.Eq (T.Dumping_Log_Memory_History.Get (8), Pack (T.Memory_Dump_Recv_Sync_History.Get (8).Memory_Pointer));
      Ptr := T.Memory_Dump_Recv_Sync_History.Get (8).Memory_Pointer;
      Bytes (0 .. Length (Ptr) - 1) := To_Byte_Array (Ptr);
      Put_Line (Basic_Types.Representation.Image (Bytes (0 .. Length (Ptr) - 1)));
      Byte_Array_Assert.Eq (Bytes (0 .. Length (Ptr) - 1), Bytes_To_Compare (0 .. Length (Ptr) - 1));

   end Test_Log_And_Dump;

   overriding procedure Test_Logger_Error (Self : in out Instance) is
      use Serializer_Types;
      use Logger_Error;
      use Logger_Enums.Log_Attempt_Status;
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Var : Simple_Variable.T;
      Len : Natural := 0;
      Log_Bytes : aliased Basic_Types.Byte_Array := [0 .. 49 => 0];
      Meta_Data : aliased Circular_Buffer_Meta.T := (0, 0, 0);
   begin
      -- Initialize the component:
      T.Component_Instance.Init (Bytes => Log_Bytes'Unchecked_Access, Meta_Data => Meta_Data'Unchecked_Access, Initial_Mode => Logger_Mode.Enabled);

      -- Send good log data:
      Var := (3, [others => 9]);
      T.T_Send (Var);

      -- Send some bad log data:
      Var := (21, [0 => 1, 1 => 2, 2 => 3, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Failure);
      Var := (255, [0 => 1, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Failure);
      Var := (22, [0 => 2, 1 => 3, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Failure);
      Var := (23, [0 => 4, 1 => 5, 2 => 6, others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));
      pragma Assert (Simple_Variable.Serialized_Length (Var, Len) = Failure);
      pragma Unreferenced (Len);

      -- Make sure 4 error events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Log_Attempt_Failed_History.Get_Count, 4);

      -- Check errors:
      Logger_Error_Assert.Eq (T.Log_Attempt_Failed_History.Get (1), (21 + 1, Serialization_Failure));
      Logger_Error_Assert.Eq (T.Log_Attempt_Failed_History.Get (2), (255 + 1, Serialization_Failure));
      Logger_Error_Assert.Eq (T.Log_Attempt_Failed_History.Get (3), (22 + 1, Serialization_Failure));
      Logger_Error_Assert.Eq (T.Log_Attempt_Failed_History.Get (4), (23 + 1, Serialization_Failure));

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 1);
      -- Make sure no data was logged:
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (1), ((Head => 0, Count => 4, Size => 50), Current_Mode => Logger_Mode.Enabled));

      -- Reinitialize the component to have tiny log:
      T.Component_Instance.Init (Size => 5, Initial_Mode => Logger_Mode.Enabled);

      -- Send some good log data:
      Var := (4, [others => 9]);
      T.T_Send (Var);
      Put_Line (Basic_Types.Representation.Image (Log_Bytes));

      -- Send some bad log data:
      Var := (5, [others => 8]);
      T.T_Send (Var);
      Var := (6, [others => 8]);
      T.T_Send (Var);
      Var := (255, [others => 8]);
      T.T_Send (Var);

      -- Make sure 4 error events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Log_Attempt_Failed_History.Get_Count, 7);

      -- Check errors:
      Logger_Error_Assert.Eq (T.Log_Attempt_Failed_History.Get (5), (5 + 1, Too_Full));
      Logger_Error_Assert.Eq (T.Log_Attempt_Failed_History.Get (6), (6 + 1, Too_Full));
      Logger_Error_Assert.Eq (T.Log_Attempt_Failed_History.Get (7), (255 + 1, Serialization_Failure));

      -- Request a meta data event:
      T.Command_T_Send (T.Commands.Send_Meta_Data_Event);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Send_Meta_Data_Event_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Log_Info_Update_History.Get_Count, 2);
      -- Make sure no data was logged:
      Logger_Info_Assert.Eq (T.Log_Info_Update_History.Get (2), ((Head => 4, Count => 5, Size => 5), Current_Mode => Logger_Mode.Enabled));

   end Test_Logger_Error;

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

end Variable_Tests.Implementation;
