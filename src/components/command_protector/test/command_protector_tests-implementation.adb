--------------------------------------------------------------------------------
-- Command_Protector Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Basic_Assertions; use Basic_Assertions;
with Packed_Arm_State.Assertion; use Packed_Arm_State.Assertion;
with Packed_Arm_Timeout.Assertion; use Packed_Arm_Timeout.Assertion;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Command_Protector_Enums; use Command_Protector_Enums.Armed_State;
with Command.Assertion; use Command.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Packet.Assertion; use Packet.Assertion;
with Interfaces;

package body Command_Protector_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals:
   -------------------------------------------------------------------------
   Protected_Command_Id_List : constant Component.Command_Protector.Command_Id_List := [4, 19, 77, 78];

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
      Self.Tester.Component_Instance.Init (Protected_Command_Id_List => Protected_Command_Id_List);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component:
      Self.Tester.Component_Instance.Final;

      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Initialization (Self : in out Instance) is
      T : Component.Command_Protector.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal is
      begin
         T.Component_Instance.Init (Protected_Command_Id_List => [1, 2, 3, 4]);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init failed!");
      end Init_Nominal;

      procedure Init_None is
      begin
         -- Empty list not ok.
         T.Component_Instance.Init (Protected_Command_Id_List => [1 .. 0 => 0]);
         -- Should never get here:
         Assert (False, "Index out of range did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_None;

      procedure Init_Duplicate is
      begin
         T.Component_Instance.Init (Protected_Command_Id_List => [1, 2, 3, 2]);
         -- Should never get here:
         Assert (False, "Duplicate ID did not produce exception!");
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
      Init_None;
      T.Component_Instance.Final;
      Init_Duplicate;

      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 1);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (1), (State => Unarmed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (1), (Timeout => 0));
      Natural_Assert.Eq (T.Protected_Command_Reject_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Protected_Command_Reject_Count_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Protected_Command_Forward_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Protected_Command_Forward_Count_History.Get (1), (Value => 0));
   end Test_Initialization;

   overriding procedure Test_Unprotected_Command_Accept (Self : in out Instance) is
      T : Component.Command_Protector.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := (Header => (Source_Id => 0, Id => 2, Arg_Buffer_Length => 19), Arg_Buffer => [others => 88]);
   begin
      -- Send a command not in the protected list:
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Cmd);

      -- Send a command not in the protected list:
      Cmd.Header.Id := 17;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Cmd);

      -- Send a command not in the protected list:
      Cmd.Header.Id := 1_001;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Cmd);

      -- No events or data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- OK now arm the component:
      T.Command_T_Send (T.Commands.Arm ((Timeout => 55)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (1), (Timeout => 55));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 1);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (1), (State => Armed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (1), (Timeout => 55));

      -- Send a command not in the protected list:
      Cmd.Header.Id := 18;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Cmd);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unarmed_History.Get_Count, 1);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 2);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (2), (State => Unarmed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (2), (Timeout => 0));

      -- OK now arm the component:
      T.Command_T_Send (T.Commands.Arm ((Timeout => 22)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (2), (Timeout => 22));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 3);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (3), (State => Armed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 3);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (3), (Timeout => 22));

      -- Send a command not in the protected list:
      Cmd.Header.Id := 44;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Cmd);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unarmed_History.Get_Count, 2);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 4);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (4), (State => Unarmed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 4);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (4), (Timeout => 0));
   end Test_Unprotected_Command_Accept;

   overriding procedure Test_Protected_Command_Accept (Self : in out Instance) is
      T : Component.Command_Protector.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := (Header => (Source_Id => 0, Id => 2, Arg_Buffer_Length => 19), Arg_Buffer => [others => 88]);
   begin
      -- OK now arm the component:
      T.Command_T_Send (T.Commands.Arm ((Timeout => 55)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (1), (Timeout => 55));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 1);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (1), (State => Armed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (1), (Timeout => 55));

      -- Send a command IN the protected list:
      Cmd.Header.Id := 4;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Cmd);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unarmed_History.Get_Count, 1);
      Natural_Assert.Eq (T.Accepted_Protected_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Accepted_Protected_Command_History.Get (1), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 2);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (2), (State => Unarmed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (2), (Timeout => 0));
      Natural_Assert.Eq (T.Protected_Command_Forward_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Protected_Command_Forward_Count_History.Get (1), (Value => 1));

      -- OK now arm the component:
      T.Command_T_Send (T.Commands.Arm ((Timeout => 22)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (2), (Timeout => 22));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 3);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (3), (State => Armed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 3);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (3), (Timeout => 22));

      -- Send a command not in the protected list:
      Cmd.Header.Id := 19;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Cmd);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Unarmed_History.Get_Count, 2);
      Natural_Assert.Eq (T.Accepted_Protected_Command_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Accepted_Protected_Command_History.Get (2), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 4);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (4), (State => Unarmed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 4);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (4), (Timeout => 0));
      Natural_Assert.Eq (T.Protected_Command_Forward_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Protected_Command_Forward_Count_History.Get (2), (Value => 2));
   end Test_Protected_Command_Accept;

   overriding procedure Test_Protected_Command_Reject (Self : in out Instance) is
      T : Component.Command_Protector.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := (Header => (Source_Id => 0, Id => 2, Arg_Buffer_Length => 19), Arg_Buffer => [others => 88]);
   begin
      -- OK don't arm the component:
      Cmd.Header.Id := 4;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command not to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Rejected_Protected_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Rejected_Protected_Command_History.Get (1), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Protected_Command_Reject_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Protected_Command_Reject_Count_History.Get (1), (Value => 1));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1), T.Packets.Error_Packet_Truncate (T.System_Time, Cmd));

      -- OK don't arm the component:
      Cmd.Header.Id := 77;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command not to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Rejected_Protected_Command_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Rejected_Protected_Command_History.Get (2), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Protected_Command_Reject_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Protected_Command_Reject_Count_History.Get (2), (Value => 2));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (2), T.Packets.Error_Packet_Truncate (T.System_Time, Cmd));
   end Test_Protected_Command_Reject;

   overriding procedure Test_Protected_Command_Reject_Timeout (Self : in out Instance) is
      T : Component.Command_Protector.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := (Header => (Source_Id => 0, Id => 2, Arg_Buffer_Length => 19), Arg_Buffer => [others => 88]);
   begin
      -- OK now arm the component:
      T.Command_T_Send (T.Commands.Arm ((Timeout => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (1), (Timeout => 3));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 1);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (1), (State => Armed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 1);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (1), (Timeout => 3));

      -- Send ticks:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (2), (Timeout => 2));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 3);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (3), (Timeout => 1));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 4);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (4), (Timeout => 0));
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 2);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (2), (State => Unarmed));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unarmed_Timeout_History.Get_Count, 1);

      -- OK now arm the component:
      T.Command_T_Send (T.Commands.Arm ((Timeout => 3)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 2);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (2), (Timeout => 3));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 3);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (3), (State => Armed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 5);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (5), (Timeout => 3));

      -- Send ticks:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 6);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (6), (Timeout => 2));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 7);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (7), (Timeout => 1));

      -- Send a command prior to timeout IN the protected list:
      Cmd.Header.Id := 4;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Cmd);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Unarmed_History.Get_Count, 1);
      Natural_Assert.Eq (T.Accepted_Protected_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Accepted_Protected_Command_History.Get (1), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 4);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (4), (State => Unarmed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 8);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (8), (Timeout => 0));
      Natural_Assert.Eq (T.Protected_Command_Forward_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Protected_Command_Forward_Count_History.Get (1), (Value => 1));

      -- OK now arm the component with infinite timeout:
      T.Command_T_Send (T.Commands.Arm ((Timeout => 0)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Armed_History.Get_Count, 3);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_History.Get (3), (Timeout => 0));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 15);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 5);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (5), (State => Armed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 9);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (9), (Timeout => 0));

      -- Send many ticks, we should never timeout:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 10);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (10), (Timeout => 0));
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 17);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 11);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (11), (Timeout => 0));
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 18);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 12);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (12), (Timeout => 0));
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 19);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 13);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (13), (Timeout => 0));
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 20);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 14);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (14), (Timeout => 0));
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 21);
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 15);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (15), (Timeout => 0));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);

      -- Send a command prior to timeout IN the protected list:
      Cmd.Header.Id := 4;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Cmd);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Unarmed_History.Get_Count, 2);
      Natural_Assert.Eq (T.Accepted_Protected_Command_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Accepted_Protected_Command_History.Get (2), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 24);
      Natural_Assert.Eq (T.Armed_State_History.Get_Count, 6);
      Packed_Arm_State_Assert.Eq (T.Armed_State_History.Get (6), (State => Unarmed));
      Natural_Assert.Eq (T.Armed_State_Timeout_History.Get_Count, 16);
      Packed_Arm_Timeout_Assert.Eq (T.Armed_State_Timeout_History.Get (16), (Timeout => 0));
      Natural_Assert.Eq (T.Protected_Command_Forward_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Protected_Command_Forward_Count_History.Get (2), (Value => 2));
   end Test_Protected_Command_Reject_Timeout;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Command_Protector.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Arm ((Timeout => 3));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Arm_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Arm_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Command_Protector_Tests.Implementation;
