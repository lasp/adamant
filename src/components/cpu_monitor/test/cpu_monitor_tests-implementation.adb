--------------------------------------------------------------------------------
-- Cpu_Monitor Tests Body
--------------------------------------------------------------------------------

with Task_Types;
with Ada.Interrupts.Names;
with Ada.Task_Identification;
with Interrupt_Types;
with Tick;
with Basic_Assertions; use Basic_Assertions;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Packet_Header.Assertion; use Packet_Header.Assertion;
with Packet_Types; use Packet_Types;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces;
with System;

package body Cpu_Monitor_Tests.Implementation is

   --- Task definitions:
   Task_Info_1 : aliased Task_Types.Task_Info :=
      (Number => 1, Id => Ada.Task_Identification.Null_Task_Id,
   -- The following is initialized by the component itself.
   Priority => 0, Stack_Address => System.Null_Address, Stack_Size => 0, Secondary_Stack_Address => System.Null_Address, Secondary_Stack_Size => 0, Secondary_Stack_Max_Usage => 0);

   Task_Info_2 : aliased Task_Types.Task_Info :=
      (Number => 2, Id => Ada.Task_Identification.Current_Task,
   -- The following is initialized by the component itself.
   Priority => 0, Stack_Address => System.Null_Address, Stack_Size => 0, Secondary_Stack_Address => System.Null_Address, Secondary_Stack_Size => 0, Secondary_Stack_Max_Usage => 0);

   -- List of task infos for all tasks:
   Task_List : aliased Task_Types.Task_Info_List := [0 => Task_Info_1'Access, 1 => Task_Info_2'Access];

   -- List of all interrupts used in the system:
   Interrupt_List : aliased Interrupt_Types.Interrupt_Id_List := [0 => Ada.Interrupts.Names.SIGUSR1, 1 => Ada.Interrupts.Names.SIGUSR2];

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
      Self.Tester.Component_Instance.Init (Task_List => Task_List'Access, Interrupt_List => Interrupt_List'Access, Execution_Periods => [1, 5, 10], Packet_Period => 1);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Packet_Period (Self : in out Instance) is
      T : Component.Cpu_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
      A_Tick : constant Tick.T := ((0, 0), 0);
   begin
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check data product at start:
      T.Component_Instance.Set_Up;
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Packet_Period_History.Get (1), (Value => 1));
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Packet_Period_History.Clear;

      -- Send a tick and expect a packet.
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Send a command to disable to packet sending.
      T.Command_T_Send (T.Commands.Set_Packet_Period ((Value => 0)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Packet_Period_Set_History.Get (1), (Value => 0));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Packet_Period_History.Get (1), (Value => 0));

      -- Send a tick and expect no packets.
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Send a command to set packet sending to every 3.
      T.Command_T_Send (T.Commands.Set_Packet_Period ((Value => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Success));

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Packet_Period_Set_History.Get (2), (Value => 3));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_Period_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Packet_Period_History.Get (2), (Value => 3));

      -- Send a tick and expect no packets.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 5);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);
      T.Tick_T_Send (A_Tick);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 6);
      T.Tick_T_Send (A_Tick);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 7);

      -- OK we cannot actually check the contents of a packet, but lets at least
      -- check the headers of all the packets.
      Natural_Assert.Eq (T.Cpu_Usage_Packet_History.Get_Count, 7);
      for Idx in Positive range 1 .. 7 loop
         Packet_Header_Assert.Eq (T.Cpu_Usage_Packet_History.Get (Idx).Header, (Time => (0, 0), Id => T.Packets.Get_Cpu_Usage_Packet_Id, Sequence_Count => Sequence_Count_Mod_Type (Idx - 1), Buffer_Length => 3 * (Task_List'Length + Interrupt_List'Length)));
      end loop;
   end Test_Packet_Period;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Cpu_Monitor.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Set_Packet_Period ((Value => 3));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Packet_Period_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_Period_Set_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Set_Packet_Period_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Cpu_Monitor_Tests.Implementation;
