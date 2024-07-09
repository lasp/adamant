--------------------------------------------------------------------------------
-- Command_Router Tests Body
--------------------------------------------------------------------------------

-- Includes
with Registration_Commands;
with Interfaces; use Interfaces;
with Packed_U32;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Basic_Assertions; use Basic_Assertions;
with Command.Assertion; use Command.Assertion;
with Command_Response.Assertion;
with Command_Router_Arg.Assertion; use Command_Router_Arg.Assertion;
with Command_Id.Assertion; use Command_Id.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Types;
with Command_Enums; use Command_Enums;
with Command_Id_Status.Assertion;
with Connector_Types;

package body Command_Router_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
      Ignore : Natural;
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => 10 * Self.Tester.Component_Instance.Get_Max_Queue_Element_Size, Command_T_Send_Count => 3, Command_Response_T_To_Forward_Send_Count => 3);
      Self.Tester.Component_Instance.Init (Max_Number_Of_Commands => 6);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- Execute component:
      Ignore := Self.Tester.Dispatch_All;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
      Self.Tester.Component_Instance.Final;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Routing (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Ignore : Natural;
   begin
      -- 5 command responses, 2 for each command registration, 1 for registration command response
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 5);

      -- No data products should have been updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);
      T.Command_T_Recv_Sync_History.Clear; -- clear registration commands from history

      -- Send Noop command and make sure the appropriate event was thrown:
      T.Command_T_To_Route_Send (T.Commands.Noop);

      -- Execute component and make sure command was received:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), T.Commands.Noop.Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (1), (Id => T.Commands.Get_Noop_Id));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Noop_Received_History.Get_Count, 1);

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Successful_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Successful_History.Get (1), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Id, Status => Success));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Command_Success_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Success_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Successful_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Successful_Command_History.Get (1), (Id => T.Commands.Get_Noop_Id));

      -- Make sure the tester did not get a command:
      Boolean_Assert.Eq (T.Command_T_Recv_Sync_History.Is_Empty, True);

      -- Send Noop Arg command and make sure the appropriate event was thrown:
      T.Command_T_To_Route_Send (T.Commands.Noop_Arg ((Value => 17)));

      -- Execute component and make sure command was received:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (2), T.Commands.Noop_Arg ((Value => 17)).Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (2), (Id => T.Commands.Get_Noop_Arg_Id));
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (2), (Value => 2));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Noop_Arg_Received_History.Get_Count, 1);
      Command_Router_Arg_Assert.Eq (T.Noop_Arg_Received_History.Get (1), (Value => 17));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Successful_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Successful_History.Get (2), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Arg_Id, Status => Success));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Command_Success_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Success_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Successful_Command_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Last_Successful_Command_History.Get (2), (Id => T.Commands.Get_Noop_Arg_Id));
      Natural_Assert.Eq (T.Noop_Arg_Last_Value_History.Get_Count, 1);
      Command_Router_Arg_Assert.Eq (T.Noop_Arg_Last_Value_History.Get (1), (Value => 17));

      -- Make sure the tester did not get a command:
      Boolean_Assert.Eq (T.Command_T_Recv_Sync_History.Is_Empty, True);

      -- OK now send the reset command and make sure all the data products are reset:
      T.Command_T_To_Route_Send (T.Commands.Reset_Data_Products);
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 3);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (3), T.Commands.Reset_Data_Products.Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 3);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (3), (Id => T.Commands.Get_Reset_Data_Products_Id));
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (3), (Value => 3));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 18);
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 4);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (4), (Id => 0));
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 4);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (4), (Value => 0));
      Natural_Assert.Eq (T.Last_Successful_Command_History.Get_Count, 3);
      Command_Id_Assert.Eq (T.Last_Successful_Command_History.Get (3), (Id => 0));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 1);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (1), (Id => 0, Status => Success));
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Noop_Arg_Last_Value_History.Get_Count, 2);
      Command_Router_Arg_Assert.Eq (T.Noop_Arg_Last_Value_History.Get (2), (Value => 0));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Successful_History.Get_Count, 3);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Successful_History.Get (3), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Reset_Data_Products_Id, Status => Success));
   end Test_Nominal_Routing;

   overriding procedure Test_Nominal_Registration (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Buffer1 : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 0];
      Buffer2 : constant Command_Types.Command_Arg_Buffer_Type := [0 => 13, 1 => 14, others => 0];
      A_Command : Command.T;
      Reg_Cmds : Registration_Commands.Instance;
      Ignore : Natural;
   begin
      -- Set the base id for reg cmds to 0. Usually we use 1, but 0 is reserved especially
      -- for this command, so lets make sure it is 0.
      Reg_Cmds.Set_Id_Base (0);

      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);

      -- Make sure automatic registration was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      A_Command := Reg_Cmds.Register_Commands ((Registration_Id => 2));
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), A_Command);
      A_Command := Reg_Cmds.Register_Commands ((Registration_Id => 3));
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), A_Command);
      T.Command_T_Recv_Sync_History.Clear; -- clear registration commands from history

      -- Register two new commands:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 2, Command_Id => 27, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      -- Execute component:
      Ignore := Self.Tester.Dispatch_All;
      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);

      -- No events or data products should have been updated with this process:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Send the first command and make sure it was sent out:
      A_Command := ((Source_Id => 0, Id => 27, Arg_Buffer_Length => 2), Arg_Buffer => Buffer1);
      T.Command_T_To_Route_Send (A_Command);
      Ignore := Self.Tester.Dispatch_All;
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), A_Command.Header);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), A_Command);

      -- Send the second command and make sure it was sent out:
      A_Command := ((Source_Id => 0, Id => 15, Arg_Buffer_Length => 2), Arg_Buffer => Buffer2);
      T.Command_T_To_Route_Send (A_Command);
      Ignore := Self.Tester.Dispatch_All;
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (2), A_Command.Header);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), A_Command);
   end Test_Nominal_Registration;

   overriding procedure Test_Routing_Errors (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Ignore : Natural;
      Buffer1 : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 0];
      A_Command : Command.T;
   begin
      T.Command_T_Recv_Sync_History.Clear; -- clear registration commands from history

      -- Test sending a command that is not registered.
      T.Command_T_To_Route_Send (((Source_Id => 0, Id => 99, Arg_Buffer_Length => 19), Arg_Buffer => [others => 0]));
      Ignore := Self.Tester.Dispatch_All;
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), (Source_Id => 0, Id => 99, Arg_Buffer_Length => 19));

      -- Make sure nothing executed:
      Boolean_Assert.Eq (T.Command_T_Recv_Sync_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Noop_Received_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Noop_Arg_Received_History.Is_Empty, True);

      -- Make sure correct event thrown:
      Natural_Assert.Eq (T.Command_Id_Not_Registered_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Id_Not_Registered_History.Get (1), (Source_Id => 0, Id => 99, Arg_Buffer_Length => 19));

      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (1), (Id => 99));
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 1);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (1), (Id => 99, Status => Id_Error));

      -- Register command to command router that doesn't actually exist in the command router.
      -- This should cause an ID error as well.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 1, Command_Id => 27, Status => Register));
      -- Execute component:
      Ignore := Self.Tester.Dispatch_All;
      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);

      -- Send the command and make sure it was sent out:
      A_Command := ((Source_Id => 0, Id => 27, Arg_Buffer_Length => 2), Arg_Buffer => Buffer1);
      T.Command_T_To_Route_Send (A_Command);
      Ignore := Self.Tester.Dispatch_All;
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (2), A_Command.Header);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (2), (Id => 27));
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (2), (Value => 2));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => 27, Errant_Field_Number => Unsigned_32'Last - 1, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 27]));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Failure_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Failure_History.Get (1), (Source_Id => 0, Registration_Id => 1, Command_Id => 27, Status => Id_Error));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 2);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (2), (Id => 27, Status => Id_Error));
   end Test_Routing_Errors;

   overriding procedure Test_Registration_Errors (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Ignore : Natural;
   begin
      T.Command_T_Recv_Sync_History.Clear; -- clear registration commands from history

      -- Make sure we start with no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);

      -- Trigger ID Conflict event:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 2, Command_Id => 27, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 27, Status => Register));
      Ignore := Self.Tester.Dispatch_All;

      -- Make sure conflict was reported:
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);
      Natural_Assert.Eq (T.Registration_Id_Conflict_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Registration_Id_Conflict_History.Get (1), (Id => 27));

      -- Trigger different Conflict event:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 60, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 60, Status => Register));
      Ignore := Self.Tester.Dispatch_All;

      -- Make sure conflict was reported:
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);
      Natural_Assert.Eq (T.Registration_Id_Conflict_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Registration_Id_Conflict_History.Get (2), (Id => 60));

      -- Two commands should have been successfully registered. Two from the command router itself
      -- and two from tests above. In this case, if I try to register another command we should
      -- expect a table full error:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 35, Status => Register));
      Ignore := Self.Tester.Dispatch_All;
      Natural_Assert.Eq (T.Router_Table_Full_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Router_Table_Full_History.Get (1), (Id => 35));
      Natural_Assert.Eq (T.Registration_Id_Conflict_History.Get_Count, 2);
   end Test_Registration_Errors;

   overriding procedure Test_Full_Queue_Errors (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Buffer : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 92];
      A_Command : constant Command.T := ((Source_Id => 2, Id => 15, Arg_Buffer_Length => Buffer'Length), Arg_Buffer => Buffer);

      procedure Fill_Queue (N : Natural := 10) is
      begin
         for Idx in 1 .. N loop
            T.Command_T_To_Route_Send (A_Command);
         end loop;
      end Fill_Queue;

      Ignore : Natural;
   begin
      -- The queue depth is 10. Lets fill up the queue with commands and not let the component execute:
      Ignore := Self.Tester.Dispatch_All; -- Empty queue
      Ignore := Self.Tester.Dispatch_All; -- Empty queue
      Fill_Queue;
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 2);

      -- Send another command and expect an event to be thrown:
      T.Expect_Command_T_To_Route_Send_Dropped := True;
      T.Command_T_To_Route_Send (A_Command);
      Natural_Assert.Eq (T.Command_T_To_Route_Send_Dropped_Count, 1);
      T.Expect_Command_T_To_Route_Send_Dropped := True;
      T.Command_T_To_Route_Send (A_Command);
      Natural_Assert.Eq (T.Command_T_To_Route_Send_Dropped_Count, 2);
      Natural_Assert.Eq (T.Incoming_Command_Dropped_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Incoming_Command_Dropped_History.Get (1), A_Command.Header);
      Command_Header_Assert.Eq (T.Incoming_Command_Dropped_History.Get (2), A_Command.Header);

      -- Also the data product for failure should be updated.
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (1), (Value => 1));
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 2);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (1), (Id => 15, Status => Dropped));
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (2), (Id => 15, Status => Dropped));

      -- If the command was dropped, a command response should still have been sent out, which should be forwarded.
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 4);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (3), (Source_Id => 2, Registration_Id => 0, Command_Id => 15, Status => Dropped));
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (4), (Source_Id => 2, Registration_Id => 0, Command_Id => 15, Status => Dropped));

      -- Send a command registration and expect an event:
      T.Expect_Command_Response_T_Send_Dropped := True;
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 2, Command_Id => 27, Status => Register));
      Natural_Assert.Eq (T.Command_Response_T_Send_Dropped_Count, 1);
      Natural_Assert.Eq (T.Command_Response_Dropped_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_Dropped_History.Get (1), (Source_Id => 2, Registration_Id => 2, Command_Id => 27, Status => Register));

      -- Send a command response with source id of zero:
      T.Expect_Command_Response_T_Send_Dropped := True;
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 2, Command_Id => 12, Status => Success));
      Natural_Assert.Eq (T.Command_Response_T_Send_Dropped_Count, 2);
      Natural_Assert.Eq (T.Command_Response_Dropped_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_Dropped_History.Get (2), (Source_Id => 0, Registration_Id => 2, Command_Id => 12, Status => Success));

      -- Send a command response with source id of not zero and expect event and synchronous forward:
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 4);
      T.Expect_Command_Response_T_Send_Dropped := True;
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 2, Command_Id => 12, Status => Success));
      Natural_Assert.Eq (T.Command_Response_T_Send_Dropped_Count, 3);
      Natural_Assert.Eq (T.Command_Response_Dropped_History.Get_Count, 3);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_Dropped_History.Get (3), (Source_Id => 2, Registration_Id => 2, Command_Id => 12, Status => Success));
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 5);
      -- Expect status to be changed to Dropped.
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (5), (Source_Id => 2, Registration_Id => 2, Command_Id => 12, Status => Dropped));

      -- Unfortunately it is really hard to test a dropped noop command at the unit test level,
      -- since the component will dequeue and enqueue the noop message in the same execution, making
      -- a full queue condition impossible to achieve with asynchronous behavior. We can test this
      -- at the integration test level if we feel it is necessary.
   end Test_Full_Queue_Errors;

   overriding procedure Test_Invalid_Argument_Length (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Noop_Command : Command.T := T.Commands.Noop;
      Noop_Arg_Command : Command.T := T.Commands.Noop_Arg ((Value => 12));
      Ignore : Natural;
   begin
      -- 3 command responses, 2 for each command registration, 1 for registration command response
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 5);

      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);
      T.Command_T_Recv_Sync_History.Clear; -- clear registration commands from history

      -- Send Noop command with an invalid length:
      Noop_Command.Header.Arg_Buffer_Length := 5;
      T.Command_T_To_Route_Send (Noop_Command);

      -- Execute component and make sure command was received:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), Noop_Command.Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (1), (Id => T.Commands.Get_Noop_Id));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      -- Make sure no noop command was received:
      Natural_Assert.Eq (T.Noop_Received_History.Get_Count, 0);
      -- Make sure an invalid command event was sent.
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Noop_Id, Errant_Field_Number => Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 5]));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Failure_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Failure_History.Get (1), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Id, Status => Length_Error));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 1);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (1), (Id => T.Commands.Get_Noop_Id, Status => Length_Error));

      -- Send Noop Arg command with invalid length:
      Noop_Arg_Command.Header.Arg_Buffer_Length := 0;
      T.Command_T_To_Route_Send (Noop_Arg_Command);

      -- Execute component:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (2), Noop_Arg_Command.Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (2), (Id => T.Commands.Get_Noop_Arg_Id));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      -- Make sure no noop arg command was received:
      Natural_Assert.Eq (T.Noop_Arg_Received_History.Get_Count, 0);
      -- Make sure an invalid command event was sent.
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 2);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (2), (Id => T.Commands.Get_Noop_Arg_Id, Errant_Field_Number => Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Failure_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Failure_History.Get (2), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Arg_Id, Status => Length_Error));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 2);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (2), (Id => T.Commands.Get_Noop_Arg_Id, Status => Length_Error));
   end Test_Invalid_Argument_Length;

   overriding procedure Test_Invalid_Argument_Value (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Noop_Arg_Command : Command.T := T.Commands.Noop_Arg ((Value => 0)); -- Out of range
      Ignore : Natural;
   begin
      -- 3 command responses, 2 for each command registration, 1 for registration command response
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 5);

      -- Send Noop Arg command with invalid value:
      Noop_Arg_Command.Arg_Buffer (Noop_Arg_Command.Arg_Buffer'First .. Noop_Arg_Command.Arg_Buffer'First + Packed_U32.Serialization.Serialized_Length - 1) := Packed_U32.Serialization.To_Byte_Array ((Value => 1_000));
      T.Command_T_To_Route_Send (Noop_Arg_Command);

      -- Execute component and make sure command was received:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), T.Commands.Noop_Arg ((Value => 0)).Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (1), (Id => T.Commands.Get_Noop_Arg_Id));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      -- Make sure no noop command was received:
      Natural_Assert.Eq (T.Noop_Arg_Received_History.Get_Count, 0);
      -- Make sure an invalid command event was sent.
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Noop_Arg_Id, Errant_Field_Number => 1, Errant_Field => [0, 0, 0, 0, 16#e8#, 16#3#, 0, 0]));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Failure_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Failure_History.Get (1), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Arg_Id, Status => Validation_Error));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 1);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (1), (Id => T.Commands.Get_Noop_Arg_Id, Status => Validation_Error));
   end Test_Invalid_Argument_Value;

   overriding procedure Test_Failed_Command (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Ignore : Natural;
   begin
      -- 3 command responses, 2 for each command registration, 1 for registration command response
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 5);

      -- Send Noop Arg command with magic number that causes a command
      -- failure.
      T.Command_T_To_Route_Send (T.Commands.Noop_Arg ((Value => 868)));

      -- Execute component and make sure command was received:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), T.Commands.Noop_Arg ((Value => 0)).Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (1), (Id => T.Commands.Get_Noop_Arg_Id));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      -- Make sure the noop command was received:
      Natural_Assert.Eq (T.Noop_Arg_Received_History.Get_Count, 1);
      Command_Router_Arg_Assert.Eq (T.Noop_Arg_Received_History.Get (1), (Value => 868));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Failure_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Failure_History.Get (1), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Arg_Id, Status => Failure));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 1);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (1), (Id => T.Commands.Get_Noop_Arg_Id, Status => Failure));
      Natural_Assert.Eq (T.Noop_Arg_Last_Value_History.Get_Count, 1);
      Command_Router_Arg_Assert.Eq (T.Noop_Arg_Last_Value_History.Get (1), (Value => 868));
   end Test_Failed_Command;

   overriding procedure Test_Synchronous_Command (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Ignore : Natural;
   begin
      -- 3 command responses, 2 for each command registration, 1 for registration command response
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 5);

      -- No data products should have been updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);
      T.Command_T_Recv_Sync_History.Clear; -- clear registration commands from history

      -- Send synchronous Noop command and make sure the appropriate event was thrown:
      T.Command_T_To_Route_Send_2 (T.Commands.Noop);

      -- Component and make sure command was received:
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), T.Commands.Noop.Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (1), (Id => T.Commands.Get_Noop_Id));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Noop_Received_History.Get_Count, 1);

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Successful_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Successful_History.Get (1), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Id, Status => Success));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Command_Success_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Success_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Successful_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Successful_Command_History.Get (1), (Id => T.Commands.Get_Noop_Id));

      -- Make sure the tester did not get a command:
      Boolean_Assert.Eq (T.Command_T_Recv_Sync_History.Is_Empty, True);

      -- Send synchronous Noop Arg command and make sure the appropriate event was thrown:
      T.Command_T_To_Route_Send_2 (T.Commands.Noop_Arg ((Value => 17)));

      -- Execute component and make sure command was received:
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (2), T.Commands.Noop_Arg ((Value => 17)).Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (2), (Id => T.Commands.Get_Noop_Arg_Id));
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (2), (Value => 2));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Noop_Arg_Received_History.Get_Count, 1);
      Command_Router_Arg_Assert.Eq (T.Noop_Arg_Received_History.Get (1), (Value => 17));

      -- Execute component and make sure response was sent:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Successful_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Successful_History.Get (2), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Arg_Id, Status => Success));
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Command_Success_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Success_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Successful_Command_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Last_Successful_Command_History.Get (2), (Id => T.Commands.Get_Noop_Arg_Id));
      Natural_Assert.Eq (T.Noop_Arg_Last_Value_History.Get_Count, 1);
      Command_Router_Arg_Assert.Eq (T.Noop_Arg_Last_Value_History.Get (1), (Value => 17));

      -- Make sure the tester did not get a command:
      Boolean_Assert.Eq (T.Command_T_Recv_Sync_History.Is_Empty, True);
   end Test_Synchronous_Command;

   overriding procedure Test_Command_Response_Forwarding (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Ignore : Natural;
   begin
      -- Make sure that the command response forwarded registration was performed at initialization:
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (1), (Source_Id => 2, Registration_Id => 0, Command_Id => 0, Status => Register_Source));
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (2), (Source_Id => 3, Registration_Id => 0, Command_Id => 0, Status => Register_Source));

      -- 3 command responses, 2 for each command registration, 1 for registration command response
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 5);

      -- No data products should have been updated:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);
      T.Command_T_Recv_Sync_History.Clear; -- clear registration commands from history

      -- Send Noop Response command and make sure the appropriate event was thrown:
      T.Command_T_To_Route_Send (T.Commands.Noop_Response);

      -- Execute component and make sure command was received:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), T.Commands.Noop_Response.Header);
      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (1), (Id => T.Commands.Get_Noop_Response_Id));

      -- Execute component and make sure command was executed:
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Noop_Response_Received_History.Get_Count, 1);

      -- Execute component and make sure response was sent. There should also be a new
      -- command put on the queue to execute a noop.
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 2);
      Natural_Assert.Eq (T.Command_Execution_Successful_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Successful_History.Get (1), (Source_Id => 0, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Response_Id, Status => Success));

      -- Make sure noop command was routed correctly.
      Boolean_Assert.Eq (T.Command_Id_Not_Registered_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Outgoing_Command_Dropped_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (2), (Source_Id => 1, Id => T.Commands.Get_Noop_Id, Arg_Buffer_Length => 0));

      -- Check data product histories:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Command_Receive_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Receive_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Received_Command_History.Get_Count, 2);
      Command_Id_Assert.Eq (T.Last_Received_Command_History.Get (2), (Id => T.Commands.Get_Noop_Id));
      Natural_Assert.Eq (T.Command_Success_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Success_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Successful_Command_History.Get_Count, 1);
      Command_Id_Assert.Eq (T.Last_Successful_Command_History.Get (1), (Id => T.Commands.Get_Noop_Response_Id));

      -- Execute component and make sure response was sent.
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Execution_Successful_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Execution_Successful_History.Get (2), (Source_Id => 1, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Id, Status => Success));

      -- Because of the self test command, the router sent the internal command. If the command forwarding
      -- worked than an additional event should have been thrown.
      Natural_Assert.Eq (T.Noop_Response_Forwarding_Success_History.Get_Count, 1);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Noop_Response_Forwarding_Success_History.Get (1), (Source_Id => 1, Registration_Id => 1, Command_Id => T.Commands.Get_Noop_Id, Status => Success));

      -- Make sure the tester did not get a command:
      Boolean_Assert.Eq (T.Command_T_Recv_Sync_History.Is_Empty, True);

      -- OK now do some spot testing. Send some command responses with invalid source IDs and make sure an
      -- error event is thrown.
      T.Command_Response_T_Send ((Source_Id => 27, Registration_Id => 2, Command_Id => 27, Status => Success));
      T.Command_Response_T_Send ((Source_Id => 4, Registration_Id => 3, Command_Id => 15, Status => Failure));
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 2);
      Natural_Assert.Eq (T.Invalid_Command_Source_Id_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Invalid_Command_Source_Id_History.Get (1), (Source_Id => 27, Registration_Id => 2, Command_Id => 27, Status => Success));
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Invalid_Command_Source_Id_History.Get (2), (Source_Id => 4, Registration_Id => 3, Command_Id => 15, Status => Failure));
      -- No response should have been forwarded.
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 2);

      -- OK now send some valid command responses, make sure they are forwarded to the correct location.
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 2, Command_Id => 27, Status => Success));
      T.Command_Response_T_Send ((Source_Id => 3, Registration_Id => 3, Command_Id => 15, Status => Failure));
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 2);
      Natural_Assert.Eq (T.Invalid_Command_Source_Id_History.Get_Count, 2);
      -- Two response should have been forwarded.
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 4);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (3), (Source_Id => 2, Registration_Id => 2, Command_Id => 27, Status => Success));
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (4), (Source_Id => 3, Registration_Id => 3, Command_Id => 15, Status => Failure));

      -- Send a command response with zero. Expect nothing to be forwarded.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 6);
      Natural_Assert.Eq (T.Invalid_Command_Source_Id_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 4);
   end Test_Command_Response_Forwarding;

   overriding procedure Test_Command_Response_Forwarding_Dropped (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- 3 command responses, 2 for each command registration, 1 for registration command response
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 5);
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2); -- Register commands

      -- Set the connector responses for command response forwarding to dropped.
      T.Connector_Command_Response_T_To_Forward_Recv_Sync_Status := Connector_Types.Message_Dropped;

      -- OK now send some command responses and expect them to be dropped.
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 2, Command_Id => 27, Status => Success));
      T.Command_Response_T_Send ((Source_Id => 3, Registration_Id => 3, Command_Id => 15, Status => Failure));
      Natural_Assert.Eq (Self.Tester.Dispatch_All, 2);
      Natural_Assert.Eq (T.Forwarded_Command_Response_Dropped_History.Get_Count, 2);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Forwarded_Command_Response_Dropped_History.Get (1), (Source_Id => 2, Registration_Id => 2, Command_Id => 27, Status => Success));
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Forwarded_Command_Response_Dropped_History.Get (2), (Source_Id => 3, Registration_Id => 3, Command_Id => 15, Status => Failure));
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 4);
   end Test_Command_Response_Forwarding_Dropped;

   overriding procedure Test_Outgoing_Command_Dropped (Self : in out Instance) is
      use Command_Response_Status;
      T : Component.Command_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Ignore : Natural;
      A_Command : Command.T;
      Buffer1 : constant Command_Types.Command_Arg_Buffer_Type := [0 => 56, 1 => 57, others => 0];
      Buffer2 : constant Command_Types.Command_Arg_Buffer_Type := [0 => 13, 1 => 14, others => 0];
   begin
      -- Register two new commands:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 2, Command_Id => 27, Status => Register));
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 3, Command_Id => 15, Status => Register));
      -- Execute component:
      Ignore := Self.Tester.Dispatch_All;
      -- Make sure there was no registration errors:
      Boolean_Assert.Eq (T.Registration_Id_Conflict_History.Is_Empty, True);
      Boolean_Assert.Eq (T.Router_Table_Full_History.Is_Empty, True);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Set the connector responses for command routing to dropped.
      T.Connector_Command_T_Recv_Sync_Status := Connector_Types.Message_Dropped;

      -- Send the first command and make sure it was sent out:
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 0);
      A_Command := ((Source_Id => 0, Id => 27, Arg_Buffer_Length => 2), Arg_Buffer => Buffer1);
      T.Command_T_To_Route_Send (A_Command);
      Ignore := Self.Tester.Dispatch_All;
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (1), A_Command.Header);
      Natural_Assert.Eq (T.Outgoing_Command_Dropped_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Outgoing_Command_Dropped_History.Get (1), A_Command.Header);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);

      -- A command response should also have been queued that will help us log a command
      -- failure.
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 1);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (1), (Id => 27, Status => Dropped));

      -- Send the second command and make sure it was sent out:
      A_Command := ((Source_Id => 2, Id => 15, Arg_Buffer_Length => 2), Arg_Buffer => Buffer2);
      T.Command_T_To_Route_Send (A_Command);
      Ignore := Self.Tester.Dispatch_All;
      Natural_Assert.Eq (T.Command_Received_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Command_Received_History.Get (2), A_Command.Header);
      Natural_Assert.Eq (T.Outgoing_Command_Dropped_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Outgoing_Command_Dropped_History.Get (2), A_Command.Header);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- A command response should also have been queued that will help us log a command
      -- failure.
      Natural_Assert.Eq (T.Command_Failure_Count_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Command_Failure_Count_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Failed_Command_History.Get_Count, 2);
      Command_Id_Status.Assertion.Command_Id_Status_Assert.Eq (T.Last_Failed_Command_History.Get (2), (Id => 15, Status => Dropped));

      -- If the command was dropped, a command response should still have been sent out since
      -- the source ID was not zero.
      Natural_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get_Count, 3);
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (3), (Source_Id => 2, Registration_Id => 3, Command_Id => 15, Status => Dropped));
      Command_Response.Assertion.Command_Response_Assert.Eq (T.Command_Response_T_To_Forward_Recv_Sync_History.Get (3), (Source_Id => 2, Registration_Id => 3, Command_Id => 15, Status => Dropped));
   end Test_Outgoing_Command_Dropped;

end Command_Router_Tests.Implementation;
