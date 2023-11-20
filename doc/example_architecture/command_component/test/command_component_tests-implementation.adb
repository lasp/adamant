--------------------------------------------------------------------------------
-- Command_Component Tests Body
--------------------------------------------------------------------------------

with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Basic_Assertions; use Basic_Assertions;

package body Command_Component_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base;

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Unit_Test (Self : in out Instance) is
   begin
      -- Send hello world command:
      Self.Tester.Command_T_Send (Self.Tester.Commands.Hello_World);

      -- Make sure a command response was sent back and that is was Success.
      Natural_Assert.Eq (Self.Tester.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (Self.Tester.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => Self.Tester.Commands.Get_Hello_World_Id,
         Status => Success
      ));

      -- Send was five sent command:
      Self.Tester.Command_T_Send (Self.Tester.Commands.Was_Five_Sent ((Value => 5)));

      -- Make sure a command response was sent back and that is was Success.
      Natural_Assert.Eq (Self.Tester.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (Self.Tester.Command_Response_T_Recv_Sync_History.Get (2), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => Self.Tester.Commands.Get_Was_Five_Sent_Id,
         Status => Success
      ));

      -- Send was five sent command:
      Self.Tester.Command_T_Send (Self.Tester.Commands.Was_Five_Sent ((Value => 6)));

      -- Make sure a command response was sent back and that is Failure.
      Natural_Assert.Eq (Self.Tester.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (Self.Tester.Command_Response_T_Recv_Sync_History.Get (3), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => Self.Tester.Commands.Get_Was_Five_Sent_Id,
         Status => Failure
      ));
   end Unit_Test;

end Command_Component_Tests.Implementation;
