--------------------------------------------------------------------------------
-- Memory_Copier Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Interfaces; use Interfaces;
with Basic_Types;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Memory_Manager_Enums;
with Memory_Enums;
with Memory_Copier_Tests.Implementation.Simulator;
with Invalid_Memory_Region_Length.Assertion; use Invalid_Memory_Region_Length.Assertion;
with Memory_Region_Copy.Assertion; use Memory_Region_Copy.Assertion;
with Virtual_Memory_Region_Copy.Assertion; use Virtual_Memory_Region_Copy.Assertion;
with Memory_Region_Release.Assertion; use Memory_Region_Release.Assertion;
with System.Storage_Elements; use System.Storage_Elements;

package body Memory_Copier_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Reset task state:
      Simulator.Task_Send_Response := False;
      Simulator.Task_Send_Timeout := False;
      Simulator.Task_Response := Memory_Enums.Memory_Copy_Status.Success;

      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Ticks_Until_Timeout => 3);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Make sure the simulator task is disarmed and idle before the
      -- Tester is released:
      Simulator.Disarm;
      Simulator.Wait_Idle;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Copy (Self : in out Instance) is
      T : Component.Memory_Copier.Implementation.Tester.Instance_Access renames Self.Tester;
      Dest : Basic_Types.Byte_Array (0 .. 99) := [others => 44];
      Dest2 : Basic_Types.Byte_Array (0 .. 99) := [others => 55];
   begin
      Simulator.Arm (Self'Unchecked_Access);
      -- Send command to copy region:
      T.Command_T_Send (T.Commands.Copy_Memory_Region ((Source_Address => 5, Source_Length => 6, Destination_Address => Dest'Address)));

      -- Execute the command and tell the task to respond.
      Simulator.Task_Send_Response := True;
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Simulator.Sleep (4);

      -- Make sure that proper connectors were called:
      Natural_Assert.Eq (T.Memory_Region_Request_T_Return_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ided_Memory_Region_Release_Reciprocal_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get_Count, 1);
      Memory_Region_Copy_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get (1), (Source_Region => (T.Scratch'Address + 5, Length => 6), Destination_Address => Dest'Address));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Copy_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (1), (Source_Address => 5, Source_Length => 6, Destination_Address => Dest'Address));
      Natural_Assert.Eq (T.Finished_Copy_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Finished_Copy_History.Get (1), (Source_Address => 5, Source_Length => 6, Destination_Address => Dest'Address));

      -- Check command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Success));

      -- Send command to copy region:
      T.Command_T_Send (T.Commands.Copy_Memory_Region ((Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest2'Address)));

      -- Execute the command and tell the task to respond.
      Simulator.Task_Send_Response := True;
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Simulator.Sleep (4);

      -- Make sure that proper connectors were called:
      Natural_Assert.Eq (T.Memory_Region_Request_T_Return_History.Get_Count, 2);
      Natural_Assert.Eq (T.Ided_Memory_Region_Release_Reciprocal_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get_Count, 2);
      Memory_Region_Copy_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get (2), (Source_Region => (T.Scratch'Address, Length => T.Scratch'Length), Destination_Address => Dest2'Address));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Starting_Copy_History.Get_Count, 2);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (2), (Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest2'Address));
      Natural_Assert.Eq (T.Finished_Copy_History.Get_Count, 2);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Finished_Copy_History.Get (2), (Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest2'Address));

      -- Check command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Success));

      -- Kill our helper task.
      Simulator.Disarm;
   end Test_Nominal_Copy;

   overriding procedure Test_Copy_Failure (Self : in out Instance) is
      use Memory_Enums.Memory_Copy_Status;
      T : Component.Memory_Copier.Implementation.Tester.Instance_Access renames Self.Tester;
      Dest : Basic_Types.Byte_Array (0 .. 99) := [others => 44];
   begin
      Simulator.Arm (Self'Unchecked_Access);
      -- Send command to copy region 1 byte too large:
      T.Command_T_Send (T.Commands.Copy_Memory_Region ((Source_Address => 0, Source_Length => 5, Destination_Address => Dest'Address)));

      -- Execute the command and tell the task to respond.
      Simulator.Task_Response := Failure;
      Simulator.Task_Send_Response := True;
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Simulator.Sleep (4);

      -- Make sure that proper connectors were called:
      Natural_Assert.Eq (T.Memory_Region_Request_T_Return_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ided_Memory_Region_Release_Reciprocal_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (1), (Source_Address => 0, Source_Length => 5, Destination_Address => Dest'Address));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Copy_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (1), (Source_Address => 0, Source_Length => 5, Destination_Address => Dest'Address));
      Natural_Assert.Eq (T.Copy_Failure_History.Get_Count, 1);
      Memory_Region_Release_Assert.Eq (T.Copy_Failure_History.Get (1), (Region => (Simulator.Sim_Bytes'Address, Simulator.Sim_Bytes'Length), Status => Failure));

      -- Check command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Failure));

      -- Kill our helper task.
      Simulator.Disarm;
   end Test_Copy_Failure;

   overriding procedure Test_Copy_Timeout (Self : in out Instance) is
      T : Component.Memory_Copier.Implementation.Tester.Instance_Access renames Self.Tester;
      Dest : Basic_Types.Byte_Array (0 .. 99) := [others => 44];
   begin
      Simulator.Arm (Self'Unchecked_Access);
      -- Send command to copy region 1 byte too large:
      T.Command_T_Send (T.Commands.Copy_Memory_Region ((Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest'Address)));

      -- Execute the command and tell the task to respond.
      Simulator.Task_Send_Timeout := True;
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Simulator.Sleep (4);

      -- Make sure that proper connectors were called:
      Natural_Assert.Eq (T.Memory_Region_Request_T_Return_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ided_Memory_Region_Release_Reciprocal_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (1), (Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest'Address));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Copy_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (1), (Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest'Address));
      Natural_Assert.Eq (T.Copy_Timeout_History.Get_Count, 1);

      -- Check command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Failure));

      -- Kill our helper task.
      Simulator.Disarm;
   end Test_Copy_Timeout;

   overriding procedure Test_Memory_Unavailable (Self : in out Instance) is
      T : Component.Memory_Copier.Implementation.Tester.Instance_Access renames Self.Tester;
      Dest : Basic_Types.Byte_Array (0 .. 99) := [others => 44];
   begin
      Simulator.Arm (Self'Unchecked_Access);
      -- Set scratch return status:
      T.Scratch_Return_Status := Memory_Manager_Enums.Memory_Request_Status.Failure;

      -- Send command to copy region 1 byte too large:
      T.Command_T_Send (T.Commands.Copy_Memory_Region ((Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest'Address)));

      -- Execute the command and tell the task to respond.
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure that proper connectors were called:
      Natural_Assert.Eq (T.Memory_Region_Request_T_Return_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ided_Memory_Region_Release_Reciprocal_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Copy_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (1), (Source_Address => 0, Source_Length => T.Scratch'Length, Destination_Address => Dest'Address));
      Natural_Assert.Eq (T.Memory_Region_Unavailable_History.Get_Count, 1);

      -- Check command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Failure));

      -- Kill our helper task.
      Simulator.Disarm;
   end Test_Memory_Unavailable;

   overriding procedure Test_Length_Mismatch (Self : in out Instance) is
      use Memory_Enums.Memory_Copy_Status;
      T : Component.Memory_Copier.Implementation.Tester.Instance_Access renames Self.Tester;
      Dest : Basic_Types.Byte_Array (0 .. 99) := [others => 44];
   begin
      Simulator.Arm (Self'Unchecked_Access);
      -- Send command to copy region 1 byte too large:
      T.Command_T_Send (T.Commands.Copy_Memory_Region ((Source_Address => 0, Source_Length => T.Scratch'Length + 1, Destination_Address => Dest'Address)));

      -- Execute the command and tell the task to respond.
      Simulator.Task_Send_Response := True;
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Simulator.Sleep (4);

      -- Make sure that proper connectors were called:
      Natural_Assert.Eq (T.Memory_Region_Request_T_Return_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ided_Memory_Region_Release_Reciprocal_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Copy_History.Get_Count, 1);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (1), (Source_Address => 0, Source_Length => T.Scratch'Length + 1, Destination_Address => Dest'Address));
      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Memory_Region_Length_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get (1), (Region => (T.Scratch'Address, T.Scratch'Length), Expected_Length => T.Scratch'Length + 1));

      -- Check command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Failure));

      -- Send command to copy region 1 byte too large with offset:
      T.Command_T_Send (T.Commands.Copy_Memory_Region ((Source_Address => 5, Source_Length => T.Scratch'Length - 4, Destination_Address => T'Address)));

      -- Execute the command and tell the task to respond.
      Simulator.Task_Response := Failure;
      Simulator.Task_Send_Response := True;
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Simulator.Sleep (4);

      -- Make sure that proper connectors were called:
      Natural_Assert.Eq (T.Memory_Region_Request_T_Return_History.Get_Count, 2);
      Natural_Assert.Eq (T.Ided_Memory_Region_Release_Reciprocal_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Region_Copy_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Starting_Copy_History.Get_Count, 2);
      Virtual_Memory_Region_Copy_Assert.Eq (T.Starting_Copy_History.Get (2), (Source_Address => 5, Source_Length => T.Scratch'Length - 4, Destination_Address => T'Address));
      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 2);
      Invalid_Memory_Region_Length_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get (2), (Region => (T.Scratch'Address, T.Scratch'Length), Expected_Length => T.Scratch'Length + 1));

      -- Check command response:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Failure));

      -- Kill our helper task.
      Simulator.Disarm;
   end Test_Length_Mismatch;

   overriding procedure Test_Full_Queue (Self : in out Instance) is
      T : Component.Memory_Copier.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Copy_Memory_Region ((Source_Address => 0, Source_Length => 0, Destination_Address => T'Address));
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
   end Test_Full_Queue;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Memory_Copier.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Copy_Memory_Region ((Source_Address => 0, Source_Length => 0, Destination_Address => T'Address));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 22;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Copy_Memory_Region_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Copy_Memory_Region_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 22]));

      -- This is the suite's last scenario: let the simulator task quit
      -- so the host binary can terminate.
      Simulator.Request_Quit;
   end Test_Invalid_Command;

end Memory_Copier_Tests.Implementation;
