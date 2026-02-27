--------------------------------------------------------------------------------
-- Command_Rejector Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Basic_Assertions; use Basic_Assertions;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Command_Protector_Enums; use Command_Protector_Enums.Armed_State;
with Command.Assertion; use Command.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Packet.Assertion; use Packet.Assertion;

package body Command_Rejector_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals:
   -------------------------------------------------------------------------
   Reject_Command_Id_List : constant Component.Command_Rejector.Command_Id_List := [4, 19, 77, 78];

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
      Self.Tester.Component_Instance.Init (Command_Id_Reject_List => Reject_Command_Id_List);
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
      T : Component.Command_Rejector.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal is
      begin
         T.Component_Instance.Init (Command_Id_Reject_List => [1, 2, 3, 4]);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init failed!");
      end Init_Nominal;

      procedure Init_None is
      begin
         -- Empty list not ok.
         T.Component_Instance.Init (Command_Id_Reject_List => [1 .. 0 => 0]);
         -- Should never get here:
         Assert (False, "Index out of range did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_None;

      procedure Init_Duplicate is
      begin
         T.Component_Instance.Init (Command_Id_Reject_List => [1, 2, 3, 2]);
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
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Rejected_Command_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Rejected_Command_Count_History.Get (1), (Value => 0));
   end Test_Initialization;

   overriding procedure Test_Command_Accept (Self : in out Instance) is
      T : Component.Command_Rejector.Implementation.Tester.Instance_Access renames Self.Tester;
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
   end Test_Command_Accept;

   overriding procedure Test_Command_Reject (Self : in out Instance) is
      T : Component.Command_Rejector.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := (Header => (Source_Id => 0, Id => 2, Arg_Buffer_Length => 19), Arg_Buffer => [others => 88]);
   begin
      -- OK send command in list:
      Cmd.Header.Id := 4;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command not to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Rejected_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Rejected_Command_History.Get (1), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Rejected_Command_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Rejected_Command_Count_History.Get (1), (Value => 1));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (1), T.Packets.Error_Packet_Truncate (T.System_Time, Cmd));

      -- OK send command in list:
      Cmd.Header.Id := 77;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command not to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Rejected_Command_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Rejected_Command_History.Get (2), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Rejected_Command_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Rejected_Command_Count_History.Get (2), (Value => 2));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (2), T.Packets.Error_Packet_Truncate (T.System_Time, Cmd));

      -- OK send command in list:
      Cmd.Header.Id := 78;
      T.Command_T_To_Forward_Send (Cmd);

      -- Expect command not to be forwarded:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Rejected_Command_History.Get_Count, 3);
      Command_Header_Assert.Eq (T.Rejected_Command_History.Get (3), Cmd.Header);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Rejected_Command_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Rejected_Command_Count_History.Get (3), (Value => 3));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Packet_Assert.Eq (T.Packet_T_Recv_Sync_History.Get (3), T.Packets.Error_Packet_Truncate (T.System_Time, Cmd));
   end Test_Command_Reject;

end Command_Rejector_Tests.Implementation;
