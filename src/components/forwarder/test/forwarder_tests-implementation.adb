--------------------------------------------------------------------------------
-- Forwarder Tests Body
--------------------------------------------------------------------------------

with Safe_Deallocator;
with Basic_Enums;
use Basic_Enums.Enable_Disable_Type;
with Basic_Assertions; use Basic_Assertions;
with Tick.Assertion; use Tick.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Interfaces; use Interfaces;
with Packed_Enable_Disable_Type.Assertion; use Packed_Enable_Disable_Type.Assertion;

package body Forwarder_Tests.Implementation is

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

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Startup_Forwarding_State => Enabled);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      -- Free the tester component:
      procedure Free_Tester is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Component_Tester_Package.Instance,
         Name => Component_Tester_Package.Instance_Access
      );
   begin
      -- Free component heap:
      Self.Tester.Final_Base;

      -- Delete tester:
      Free_Tester (Self.Tester);
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Init (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
   begin
      -- Nothing sent at init:
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- Make sure data product sent:
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Forwarding_State_History.Get_Count, 1);
      Packed_Enable_Disable_Type_Assert.Eq (T.Forwarding_State_History.Get (1), (State => Enabled));
   end Test_Init;

   overriding procedure Test_Enable_Disable_Forwarding (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
   begin
      -- Send some data and make sure it is forwarded:
      T.T_Send (((1, 2), 3));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 1);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (1), ((1, 2), 3));

      T.T_Send (((4, 5), 6));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 2);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (2), ((4, 5), 6));

      -- Send command to disable
      T.Command_T_Send (T.Commands.Disable_Forwarding);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Forwarding_Id, Status => Success));

      -- Check events and data products:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Forwarding_Disabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Forwarding_State_History.Get_Count, 1);
      Packed_Enable_Disable_Type_Assert.Eq (T.Forwarding_State_History.Get (1), (State => Disabled));

      -- Send some data and make sure it not forwarded:
      T.T_Send (((1, 2), 3));
      T.T_Send (((4, 5), 6));
      T.T_Send (((4, 5), 6));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 2);

      -- Send command to enable
      T.Command_T_Send (T.Commands.Enable_Forwarding);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Forwarding_Id, Status => Success));

      -- Check events and data products:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Forwarding_Enabled_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Forwarding_State_History.Get_Count, 2);
      Packed_Enable_Disable_Type_Assert.Eq (T.Forwarding_State_History.Get (2), (State => Enabled));

      -- Send some data and make sure it is forwarded:
      T.T_Send (((1, 2), 3));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (3), ((1, 2), 3));

      T.T_Send (((4, 5), 6));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 4);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (4), ((4, 5), 6));
   end Test_Enable_Disable_Forwarding;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Enable_Forwarding;
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 1;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Forwarding_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Enable_Forwarding_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 1]));
   end Test_Invalid_Command;

end Forwarder_Tests.Implementation;
