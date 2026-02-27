--------------------------------------------------------------------------------
-- Memory_Packetizer_Fixed_Id Tests Body
--------------------------------------------------------------------------------

with Memory_Packetizer_Types;
with Byte_Array_Pointer;
with Basic_Types;
with Basic_Assertions; use Basic_Assertions;
with Smart_Assert;
with Packet_Types;
with Sys_Time.Assertion; use Sys_Time.Assertion;
with Sys_Time.Arithmetic; use Sys_Time.Arithmetic;
with Ada.Real_Time;
with Packets_Per_Period.Assertion; use Packets_Per_Period.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with System.Storage_Elements; use System.Storage_Elements;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;

package body Memory_Packetizer_Fixed_Id_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
      Dump_Size : constant Natural := (Memory_Packetizer_Types.Memory_Dump'Object_Size - 1) / 8 + 1;
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Dump_Size * 5 + 5 * 5 - 1);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Max_Packets_Per_Time_Period => 3, Time_Period_In_Seconds => 5);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   package Packet_Id_Assert is new Smart_Assert.Discrete (Packet_Types.Packet_Id, Packet_Types.Packet_Id'Image);
   package Sequence_Count_Assert is new Smart_Assert.Discrete (Packet_Types.Sequence_Count_Mod_Type, Packet_Types.Sequence_Count_Mod_Type'Image);

   overriding procedure Test_Nominal_Packetization (Self : in out Instance) is
      use Byte_Array_Pointer;
      use Packet_Types;
      T : Component.Memory_Packetizer_Fixed_Id.Implementation.Tester.Instance_Access renames Self.Tester;
      Mem_Region_Length : constant Natural := Memory_Region.Serialization.Serialized_Length;
      Packet_Data_Length : constant Natural := Packet_Buffer_Type'Length - Mem_Region_Length;
      Bytes : aliased Basic_Types.Byte_Array := [0 .. 4 * Packet_Data_Length - Packet_Data_Length / 2 - 1 => 0];
      Dump_1 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 7, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
      Dump_2 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 28, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
   begin
      -- Fill the byte array:
      Bytes (0 .. Packet_Data_Length - 1) := [others => 9];
      Bytes (Packet_Data_Length .. 2 * Packet_Data_Length - 1) := [others => 10];
      Bytes (2 * Packet_Data_Length .. 3 * Packet_Data_Length - 1) := [others => 11];
      Bytes (3 * Packet_Data_Length .. 4 * Packet_Data_Length - Packet_Data_Length / 2 - 1) := [others => 12];

      -- Send two memory dumps:
      T.Memory_Dump_Send (Dump_1);
      T.Memory_Dump_Send (Dump_2);

      -- Empty the component queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- We would expect 8 packets to have been sent out:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Memory_Dump_Packet_History.Get_Count, 8);

      -- Check the packet ids:
      for Idx in 1 .. 4 loop
         Packet_Id_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Id, T.Packets.Get_Memory_Dump_Packet_Id);
      end loop;
      for Idx in 5 .. 8 loop
         Packet_Id_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Id, T.Packets.Get_Memory_Dump_Packet_Id);
      end loop;

      -- Check packet sequence counts:
      for Idx in 1 .. 4 loop
         Sequence_Count_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Sequence_Count, Sequence_Count_Mod_Type (Idx - 1));
      end loop;
      for Idx in 5 .. 8 loop
         Sequence_Count_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Sequence_Count, Sequence_Count_Mod_Type (Idx - 1));
      end loop;

      -- Check packet lengths:
      for Idx in 1 .. 3 loop
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Buffer_Length, Packet_Buffer_Type'Length);
      end loop;
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (4).Header.Buffer_Length, Packet_Data_Length / 2 + Memory_Region.Serialization.Serialized_Length);
      for Idx in 5 .. 7 loop
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Buffer_Length, Packet_Buffer_Type'Length);
      end loop;
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (8).Header.Buffer_Length, Packet_Data_Length / 2 + Memory_Region.Serialization.Serialized_Length);

      -- Check packet contents:
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 9]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (2).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 10]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (3).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 11]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (4).Buffer (Mem_Region_Length .. T.Packet_T_Recv_Sync_History.Get (4).Header.Buffer_Length - 1), [0 .. Packet_Data_Length / 2 - 1 => 12]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (5).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 9]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (6).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 10]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (7).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 11]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (8).Buffer (Mem_Region_Length .. T.Packet_T_Recv_Sync_History.Get (8).Header.Buffer_Length - 1), [0 .. Packet_Data_Length / 2 - 1 => 12]);

      -- Check packet address and length headers:
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (1).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address, Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (2).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (3).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (2 * Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (4).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (3 * Packet_Data_Length), Packet_Data_Length / 2));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (5).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address, Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (6).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (7).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (2 * Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (8).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (3 * Packet_Data_Length), Packet_Data_Length / 2));

      -- Check packet timing:
      declare
         First_Time : constant Sys_Time.T := T.Packet_T_Recv_Sync_History.Get (1).Header.Time;
         One_Second_Later : Sys_Time.T;
         Five_Seconds_Later : Sys_Time.T;
         Six_Seconds_Later : Sys_Time.T;
         Ten_Seconds_Later : Sys_Time.T;
         Eleven_Seconds_Later : Sys_Time.T;
         Status : Sys_Time_Status;
      begin
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (1_000), One_Second_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (5_000), Five_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (6_000), Six_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (10_000), Ten_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (11_000), Eleven_Seconds_Later);
         pragma Assert (Status = Success);

         -- The first 3 packets should be within a second of the first time.
         for Idx in 1 .. 3 loop
            Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, First_Time);
            Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, One_Second_Later);
         end loop;

         -- The 4th, 5th, and 6th packets should be at least 5 seconds after the first time.
         for Idx in 4 .. 6 loop
            Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Five_Seconds_Later);
            Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Six_Seconds_Later);
         end loop;

         -- The 7th and 8th packets should be at least 10 seconds after the first time.
         for Idx in 7 .. 8 loop
            Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Ten_Seconds_Later);
            Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Eleven_Seconds_Later);
         end loop;
      end;

      -- Fill the byte array:
      Bytes (0 .. Packet_Data_Length - 1) := [others => 7];
      Bytes (Packet_Data_Length .. 2 * Packet_Data_Length - 1) := [others => 6];
      Bytes (2 * Packet_Data_Length .. 3 * Packet_Data_Length - 1) := [others => 5];
      Bytes (3 * Packet_Data_Length .. 4 * Packet_Data_Length - Packet_Data_Length / 2 - 1) := [others => 4];

      -- Delay 5 seconds to simulate low packet load:
      declare
         use Ada.Real_Time;
         Five_Seconds_From_Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Milliseconds (5_500);
      begin
         delay until Five_Seconds_From_Now;
      end;

      -- Send two memory dumps:
      T.Memory_Dump_Send (Dump_2);
      T.Memory_Dump_Send (Dump_1);

      -- Empty the component queue:
      Natural_Assert.Eq (T.Dispatch_All, 2);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- We would expect 8 packets to have been sent out:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 16);
      Natural_Assert.Eq (T.Memory_Dump_Packet_History.Get_Count, 16);

      -- Check the packet ids:
      for Idx in 9 .. 12 loop
         Packet_Id_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Id, T.Packets.Get_Memory_Dump_Packet_Id);
      end loop;
      for Idx in 13 .. 16 loop
         Packet_Id_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Id, T.Packets.Get_Memory_Dump_Packet_Id);
      end loop;

      -- Check packet sequence counts:
      for Idx in 9 .. 12 loop
         Sequence_Count_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Sequence_Count, Sequence_Count_Mod_Type (Idx - 1));
      end loop;
      for Idx in 13 .. 16 loop
         Sequence_Count_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Sequence_Count, Sequence_Count_Mod_Type (Idx - 1));
      end loop;

      -- Check packet lengths:
      for Idx in 9 .. 11 loop
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Buffer_Length, Packet_Buffer_Type'Length);
      end loop;
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (12).Header.Buffer_Length, Packet_Data_Length / 2 + Memory_Region.Serialization.Serialized_Length);
      for Idx in 13 .. 15 loop
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Buffer_Length, Packet_Buffer_Type'Length);
      end loop;
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (16).Header.Buffer_Length, Packet_Data_Length / 2 + Memory_Region.Serialization.Serialized_Length);

      -- Check packet contents:
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (9).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 7]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (10).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 6]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (11).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 5]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (12).Buffer (Mem_Region_Length .. T.Packet_T_Recv_Sync_History.Get (4).Header.Buffer_Length - 1), [0 .. Packet_Data_Length / 2 - 1 => 4]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (13).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 7]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (14).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 6]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (15).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 5]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (16).Buffer (Mem_Region_Length .. T.Packet_T_Recv_Sync_History.Get (8).Header.Buffer_Length - 1), [0 .. Packet_Data_Length / 2 - 1 => 4]);

      -- Check packet address and length headers:
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (9).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address, Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (10).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (11).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (2 * Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (12).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (3 * Packet_Data_Length), Packet_Data_Length / 2));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (13).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address, Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (14).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (15).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (2 * Packet_Data_Length), Packet_Data_Length));
      Memory_Region_Assert.Eq (Memory_Region.Serialization.From_Byte_Array (T.Packet_T_Recv_Sync_History.Get (16).Buffer (0 .. Mem_Region_Length - 1)), (Bytes'Address + Storage_Offset (3 * Packet_Data_Length), Packet_Data_Length / 2));

      -- Check packet timing:
      declare
         First_Time : constant Sys_Time.T := T.Packet_T_Recv_Sync_History.Get (9).Header.Time;
         One_Second_Later : Sys_Time.T;
         Five_Seconds_Later : Sys_Time.T;
         Six_Seconds_Later : Sys_Time.T;
         Ten_Seconds_Later : Sys_Time.T;
         Eleven_Seconds_Later : Sys_Time.T;
         Status : Sys_Time_Status;
      begin
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (1_000), One_Second_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (5_000), Five_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (6_000), Six_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (10_000), Ten_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (11_000), Eleven_Seconds_Later);
         pragma Assert (Status = Success);

         -- The first 3 packets should be within a second of the first time.
         for Idx in 9 .. 11 loop
            Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, First_Time);
            Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, One_Second_Later);
         end loop;

         -- The 4th, 5th, and 6th packets should be at least 5 seconds after the first time.
         for Idx in 12 .. 14 loop
            Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Five_Seconds_Later);
            Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Six_Seconds_Later);
         end loop;

         -- The 7th and 8th packets should be at least 10 seconds after the first time.
         for Idx in 15 .. 16 loop
            Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Ten_Seconds_Later);
            Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Time, Eleven_Seconds_Later);
         end loop;
      end;
   end Test_Nominal_Packetization;

   overriding procedure Test_Set_Max_Packet_Rate (Self : in out Instance) is
      use Byte_Array_Pointer;
      use Packet_Types;
      T : Component.Memory_Packetizer_Fixed_Id.Implementation.Tester.Instance_Access renames Self.Tester;
      Mem_Region_Length : constant Natural := Memory_Region.Serialization.Serialized_Length;
      Packet_Data_Length : constant Natural := Packet_Buffer_Type'Length - Mem_Region_Length;
      Bytes : aliased Basic_Types.Byte_Array := [0 .. 4 * Packet_Data_Length - Packet_Data_Length / 2 - 1 => 0];
      Dump_1 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 4, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
   begin
      -- Check data product at start:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Max_Packets_Per_Time_Period_History.Get_Count, 1);
      Packets_Per_Period_Assert.Eq (T.Max_Packets_Per_Time_Period_History.Get (1), (Max_Packets => 3, Period => 5));
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Max_Packets_Per_Time_Period_History.Clear;

      -- Send some commands to change the packet rate:
      T.Command_T_Send (T.Commands.Set_Max_Packet_Rate ((Max_Packets => 15, Period => 1)));
      T.Command_T_Send (T.Commands.Set_Max_Packet_Rate ((Max_Packets => 12, Period => 17)));
      T.Command_T_Send (T.Commands.Set_Max_Packet_Rate ((Max_Packets => 1, Period => 3)));

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Empty the component queue:
      Natural_Assert.Eq (T.Dispatch_All, 3);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Max_Packet_Rate_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Max_Packet_Rate_Id, Status => Success));
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Max_Packet_Rate_Id, Status => Success));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Max_Packet_Rate_Set_History.Get_Count, 3);
      Packets_Per_Period_Assert.Eq (T.Max_Packet_Rate_Set_History.Get (1), (Max_Packets => 15, Period => 1));
      Packets_Per_Period_Assert.Eq (T.Max_Packet_Rate_Set_History.Get (2), (Max_Packets => 12, Period => 17));
      Packets_Per_Period_Assert.Eq (T.Max_Packet_Rate_Set_History.Get (3), (Max_Packets => 1, Period => 3));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Max_Packets_Per_Time_Period_History.Get_Count, 3);
      Packets_Per_Period_Assert.Eq (T.Max_Packets_Per_Time_Period_History.Get (1), (Max_Packets => 15, Period => 1));
      Packets_Per_Period_Assert.Eq (T.Max_Packets_Per_Time_Period_History.Get (2), (Max_Packets => 12, Period => 17));
      Packets_Per_Period_Assert.Eq (T.Max_Packets_Per_Time_Period_History.Get (3), (Max_Packets => 1, Period => 3));

      -- Fill the byte array:
      Bytes (0 .. Packet_Data_Length - 1) := [others => 9];
      Bytes (Packet_Data_Length .. 2 * Packet_Data_Length - 1) := [others => 10];
      Bytes (2 * Packet_Data_Length .. 3 * Packet_Data_Length - 1) := [others => 11];
      Bytes (3 * Packet_Data_Length .. 4 * Packet_Data_Length - Packet_Data_Length / 2 - 1) := [others => 12];

      -- Send two memory dumps:
      T.Memory_Dump_Send (Dump_1);

      -- Empty the component queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);

      -- We would expect 8 packets to have been sent out:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Check the packet ids:
      for Idx in 1 .. 4 loop
         Packet_Id_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Id, T.Packets.Get_Memory_Dump_Packet_Id);
      end loop;

      -- Check packet sequence counts:
      for Idx in 1 .. 4 loop
         Sequence_Count_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Sequence_Count, Sequence_Count_Mod_Type (Idx - 1));
      end loop;

      -- Check packet lengths:
      for Idx in 1 .. 3 loop
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (Idx).Header.Buffer_Length, Packet_Buffer_Type'Length);
      end loop;
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (4).Header.Buffer_Length, Packet_Data_Length / 2 + Memory_Region.Serialization.Serialized_Length);

      -- Check packet contents:
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 9]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (2).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 10]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (3).Buffer (Mem_Region_Length .. Packet_Buffer_Type'Last), [0 .. Packet_Data_Length - 1 => 11]);
      Byte_Array_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (4).Buffer (Mem_Region_Length .. T.Packet_T_Recv_Sync_History.Get (4).Header.Buffer_Length - 1), [0 .. Packet_Data_Length / 2 - 1 => 12]);

      -- Check packet timing:
      declare
         First_Time : constant Sys_Time.T := T.Packet_T_Recv_Sync_History.Get (1).Header.Time;
         One_Second_Later : Sys_Time.T;
         Three_Seconds_Later : Sys_Time.T;
         Four_Seconds_Later : Sys_Time.T;
         Six_Seconds_Later : Sys_Time.T;
         Seven_Seconds_Later : Sys_Time.T;
         Nine_Seconds_Later : Sys_Time.T;
         Ten_Seconds_Later : Sys_Time.T;
         Status : Sys_Time_Status;
      begin
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (1_000), One_Second_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (3_000), Three_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (4_000), Four_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (6_000), Six_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (7_000), Seven_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (9_000), Nine_Seconds_Later);
         pragma Assert (Status = Success);
         Status := Add (First_Time, Ada.Real_Time.Milliseconds (10_000), Ten_Seconds_Later);
         pragma Assert (Status = Success);

         -- The first packet should be within a second of the first time.
         Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (1).Header.Time, First_Time);
         Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (1).Header.Time, One_Second_Later);

         -- The second packet should be within 3 to 4 seconds:
         Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (2).Header.Time, Three_Seconds_Later);
         Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (2).Header.Time, Four_Seconds_Later);

         -- The third packet should be within 6 to 7 seconds:
         Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (3).Header.Time, Six_Seconds_Later);
         Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (3).Header.Time, Seven_Seconds_Later);

         -- The third packet should be within 9 to 10 seconds:
         Sys_Time_Assert.Ge (T.Packet_T_Recv_Sync_History.Get (4).Header.Time, Nine_Seconds_Later);
         Sys_Time_Assert.Lt (T.Packet_T_Recv_Sync_History.Get (4).Header.Time, Ten_Seconds_Later);
      end;
   end Test_Set_Max_Packet_Rate;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Memory_Packetizer_Fixed_Id.Implementation.Tester.Instance_Access renames Self.Tester;
      Bytes : constant Packets_Per_Period.Serialization.Byte_Array := [others => 0];
   begin
      -- Send an invalid command, we need to use serialization to avoid a constraint error.
      T.Command_T_Send (T.Commands.Set_Max_Packet_Rate (Packets_Per_Period.Serialization.From_Byte_Array (Bytes)));

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Empty the component queue:
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Max_Packet_Rate_Id, Status => Validation_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Max_Packet_Rate_Set_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Set_Max_Packet_Rate_Id, Errant_Field_Number => 2, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

   overriding procedure Test_Memory_Dump_Dropped (Self : in out Instance) is
      use Byte_Array_Pointer;
      use Packet_Types;
      T : Component.Memory_Packetizer_Fixed_Id.Implementation.Tester.Instance_Access renames Self.Tester;
      Packet_Buffer_Length : constant Natural := Packet_Buffer_Type'Length;
      Bytes : aliased Basic_Types.Byte_Array := [0 .. 4 * Packet_Buffer_Length - Packet_Buffer_Length / 2 - 1 => 0];
      Dump_1 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 1, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
      Dump_2 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 2, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
      Dump_3 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 3, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
      Dump_4 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 4, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
      Dump_5 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 5, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
      Dump_6 : constant Memory_Packetizer_Types.Memory_Dump := (Id => 6, Memory_Pointer => From_Address (Bytes'Address, Bytes'Length));
   begin
      -- Send memory dumps to fill up queue:
      T.Memory_Dump_Send (Dump_1);
      T.Memory_Dump_Send (Dump_2);
      T.Memory_Dump_Send (Dump_3);
      T.Memory_Dump_Send (Dump_4);

      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Turn off the assertion for dropped message detection:
      T.Expect_Memory_Dump_Send_Dropped := True;
      -- Overflow the queue:
      T.Memory_Dump_Send (Dump_5);
      -- Make sure that the tester detected a dropped message:
      Natural_Assert.Eq (T.Memory_Dump_Send_Dropped_Count, 1);
      -- Make sure no events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Dump_Request_Dropped_History.Get_Count, 1);
      Packet_Id_Assert.Eq (T.Memory_Dump_Request_Dropped_History.Get (1).Id, 5);

      -- Turn off the assertion for dropped message detection:
      T.Expect_Memory_Dump_Send_Dropped := True;
      -- Overflow the queue:
      T.Memory_Dump_Send (Dump_6);
      -- Make sure that the tester detected a dropped message:
      Natural_Assert.Eq (T.Memory_Dump_Send_Dropped_Count, 2);
      -- Make sure events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Dump_Request_Dropped_History.Get_Count, 2);
      Packet_Id_Assert.Eq (T.Memory_Dump_Request_Dropped_History.Get (2).Id, 6);
   end Test_Memory_Dump_Dropped;

end Memory_Packetizer_Fixed_Id_Tests.Implementation;
