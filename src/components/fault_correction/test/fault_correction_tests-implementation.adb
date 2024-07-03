--------------------------------------------------------------------------------
-- Fault_Correction Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Test_Assembly_Fault_Responses;
with Test_Assembly_Fault_Responses_Status_Record.Assertion; use Test_Assembly_Fault_Responses_Status_Record.Assertion;
with Test_Assembly_Faults;
with Test_Assembly_Commands;
with Basic_Assertions; use Basic_Assertions;
with Sys_Time.Assertion; use Sys_Time.Assertion;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Packed_U32;
with Packed_Fault_Id.Assertion; use Packed_Fault_Id.Assertion;
with Fault_Static.Assertion; use Fault_Static.Assertion;
with Fault_Correction_Enums; use Fault_Correction_Enums.Status_Type;
with Command_Header.Assertion; use Command_Header.Assertion;
with Command.Assertion; use Command.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Interfaces; use Interfaces;
with Fault_Header.Assertion; use Fault_Header.Assertion;

package body Fault_Correction_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Fault_Response_Configurations => Test_Assembly_Fault_Responses.Fault_Response_List);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;

      -- Free component:
      Self.Tester.Component_Instance.Final;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Initialization (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_None is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Fault_Response_Configurations => [
            1 .. 0 => Test_Assembly_Fault_Responses.Component_A_Fault_1_Response
         ]);
         -- Should never get here:
         Assert (False, "Init none did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_None;

      procedure Init_Duplicate is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Fault_Response_Configurations => [
            0 => Test_Assembly_Fault_Responses.Component_A_Fault_1_Response,
            1 => Test_Assembly_Fault_Responses.Component_A_Fault_2_Response,
            2 => Test_Assembly_Fault_Responses.Component_B_Fault_1_Response,
            3 => Test_Assembly_Fault_Responses.Component_A_Fault_1_Response
         ]);
         -- Should never get here:
         Assert (False, "Init dupe dupe did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Duplicate;

      procedure Init_Too_Many is
      begin
         -- Empty list not ok.
         Self.Tester.Component_Instance.Init (Fault_Response_Configurations => [
            0 .. 5000 => Test_Assembly_Fault_Responses.Component_A_Fault_1_Response
         ]);
         -- Should never get here:
         Assert (False, "Init dupe dupe did not produce exception!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Too_Many;
   begin
      -- Make sure no events are thrown at start up:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- Check data products:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 1);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (1), (Id => 0));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 1);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (1), (0, 0));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 1);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (1))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Init tests:
      Init_None;
      T.Component_Instance.Final;
      Init_Duplicate;
      T.Component_Instance.Final;
      Init_Too_Many;
      T.Component_Instance.Final;
   end Test_Initialization;

   overriding procedure Test_Fault_Handling (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- OK send the component a fault that is enabled:
      T.Fault_T_Send ((((15, 12), Test_Assembly_Faults.Component_A_Fault_1, 0), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1).Header, (
         Source_Id => 0,
         Id => Test_Assembly_Commands.Component_A_Command_1,
         Arg_Buffer_Length => 0
      ));
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1),
         Test_Assembly_Fault_Responses.Component_A_Fault_1_Response.Command_Response);

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 1);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (1), (((15, 12), Test_Assembly_Faults.Component_A_Fault_1, 0), [others => 0]));
      Natural_Assert.Eq (T.Fault_Response_Sent_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Fault_Response_Sent_History.Get (1), (
         Source_Id => 0,
         Id => Test_Assembly_Commands.Component_A_Command_1,
         Arg_Buffer_Length => 0
      ));
      Command_Header_Assert.Eq (T.Fault_Response_Sent_History.Get (1),
         Test_Assembly_Fault_Responses.Component_A_Fault_1_Response.Command_Response.Header);

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 1);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (1), (Id => Test_Assembly_Faults.Component_A_Fault_1));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 1);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (1), (15, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 1);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (1))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Fault_Detected,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- OK send the component the same fault again:
      T.Fault_T_Send ((((15, 12), Test_Assembly_Faults.Component_A_Fault_1, 0), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2),
         Test_Assembly_Fault_Responses.Component_A_Fault_1_Response.Command_Response);

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 2);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (2), (((15, 12), Test_Assembly_Faults.Component_A_Fault_1, 0), [others => 0]));
      Natural_Assert.Eq (T.Fault_Response_Sent_History.Get_Count, 2);
      Command_Header_Assert.Eq (T.Fault_Response_Sent_History.Get (2),
         Test_Assembly_Fault_Responses.Component_A_Fault_1_Response.Command_Response.Header);

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 2);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (2), (Id => Test_Assembly_Faults.Component_A_Fault_1));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 2);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (2), (15, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 1); -- no change

      -- OK send the component a latching fault:
      T.Fault_T_Send ((((16, 12), Test_Assembly_Faults.Component_A_Fault_2, 0), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3),
         Test_Assembly_Fault_Responses.Component_A_Fault_2_Response.Command_Response);

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 3);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (3), (((16, 12), Test_Assembly_Faults.Component_A_Fault_2, 0), [others => 0]));
      Natural_Assert.Eq (T.Fault_Response_Sent_History.Get_Count, 3);
      Command_Header_Assert.Eq (T.Fault_Response_Sent_History.Get (3),
         Test_Assembly_Fault_Responses.Component_A_Fault_2_Response.Command_Response.Header);

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 11);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (3), (Value => 3));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 3);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (3), (Id => Test_Assembly_Faults.Component_A_Fault_2));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 3);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (3), (16, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 2);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (2))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Fault_Detected,
            Component_A_Fault_2_Status => Fault_Latched,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- OK send the component same latching fault again:
      T.Fault_T_Send ((((17, 12), Test_Assembly_Faults.Component_A_Fault_2, 1), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3); -- No change, latched

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 4);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (4), (((17, 12), Test_Assembly_Faults.Component_A_Fault_2, 1), [0 => 55, others => 0]));
      Natural_Assert.Eq (T.Fault_Response_Sent_History.Get_Count, 3); -- No change, latched

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 14);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 4);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (4), (Value => 4));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 4);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (4), (Id => Test_Assembly_Faults.Component_A_Fault_2));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 4);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (4), (17, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 2); -- No change, latched

      -- OK send a disabled fault:
      T.Fault_T_Send ((((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3); -- No change, disabled

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 5);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (5), (((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [0 => 55, others => 0]));
      Natural_Assert.Eq (T.Fault_Response_Sent_History.Get_Count, 3); -- No change, disabled

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 17);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 5);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (5), (Value => 5));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 5);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (5), (Id => Test_Assembly_Faults.Component_B_Fault_1));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 5);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (5), (17, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 2); -- No change, disabled
   end Test_Fault_Handling;

   overriding procedure Test_Enable_Disable_Fault_Response (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- OK send a disabled fault:
      T.Fault_T_Send ((((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0); -- No change, disabled

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 1);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (1), (((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [0 => 55, others => 0]));
      Natural_Assert.Eq (T.Fault_Response_Sent_History.Get_Count, 0); -- No change, disabled

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (1), (Value => 1));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 1);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (1), (Id => Test_Assembly_Faults.Component_B_Fault_1));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 1);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (1), (17, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 0); -- No change, disabled

      -- Send enable command:
      T.Command_T_Send (T.Commands.Enable_Fault_Response ((Id => Test_Assembly_Faults.Component_B_Fault_1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Fault_Response_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Fault_Response_Enabled_History.Get_Count, 1);
      Packed_Fault_Id_Assert.Eq (T.Fault_Response_Enabled_History.Get (1), (Id => Test_Assembly_Faults.Component_B_Fault_1));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 1);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (1))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Nominal,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- OK send fault again:
      T.Fault_T_Send ((((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1),
         Test_Assembly_Fault_Responses.Component_B_Fault_1_Response.Command_Response);

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 2);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (2), (((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [0 => 55, others => 0]));
      Natural_Assert.Eq (T.Fault_Response_Sent_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Fault_Response_Sent_History.Get (1),
         Test_Assembly_Fault_Responses.Component_B_Fault_1_Response.Command_Response.Header);

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (2), (Value => 2));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 2);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (2), (Id => Test_Assembly_Faults.Component_B_Fault_1));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 2);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (2), (17, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 2);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (2))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Fault_Detected,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Send disable command:
      T.Command_T_Send (T.Commands.Disable_Fault_Response ((Id => Test_Assembly_Faults.Component_B_Fault_1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Fault_Response_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Fault_Response_Disabled_History.Get_Count, 1);
      Packed_Fault_Id_Assert.Eq (T.Fault_Response_Disabled_History.Get (1), (Id => Test_Assembly_Faults.Component_B_Fault_1));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 3);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (3))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- OK send fault again:
      T.Fault_T_Send ((((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure response was sent out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1); -- no change

      -- Make sure we get some events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Fault_Received_History.Get_Count, 3);
      Fault_Static_Assert.Eq (T.Fault_Received_History.Get (3), (((17, 12), Test_Assembly_Faults.Component_B_Fault_1, 1), [0 => 55, others => 0]));

      -- Make sure we get some data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (3), (Value => 3));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 3);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (3), (Id => Test_Assembly_Faults.Component_B_Fault_1));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 3);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (3), (17, 12));
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 3); -- no change

      -- Send enable command to enable already enabled:
      T.Command_T_Send (T.Commands.Enable_Fault_Response ((Id => Test_Assembly_Faults.Component_A_Fault_1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Fault_Response_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Fault_Response_Enabled_History.Get_Count, 2);
      Packed_Fault_Id_Assert.Eq (T.Fault_Response_Enabled_History.Get (2), (Id => Test_Assembly_Faults.Component_A_Fault_1));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 4);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (4))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );
   end Test_Enable_Disable_Fault_Response;

   overriding procedure Test_Clear_Fault_Response (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- OK send a few faults:
      T.Fault_T_Send ((((15, 12), Test_Assembly_Faults.Component_A_Fault_1, 0), [others => 55]));
      T.Fault_T_Send ((((16, 12), Test_Assembly_Faults.Component_A_Fault_2, 0), [others => 55]));
      T.Fault_T_Send ((((16, 12), Test_Assembly_Faults.Component_B_Fault_3, 0), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 3);

      -- Check latest state data product:
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 3);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (3))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Fault_Detected,
            Component_A_Fault_2_Status => Fault_Latched,
            Component_B_Fault_3_Status => Fault_Latched,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Send command to clear a fault:
      T.Command_T_Send (T.Commands.Clear_Fault_Response ((Id => Test_Assembly_Faults.Component_A_Fault_2)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Fault_Response_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Fault_Response_Cleared_History.Get_Count, 1);
      Packed_Fault_Id_Assert.Eq (T.Fault_Response_Cleared_History.Get (1), (Id => Test_Assembly_Faults.Component_A_Fault_2));

      -- Check state data product:
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 4);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (4))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Fault_Detected,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Fault_Latched,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Send command to clear a non_latching fault:
      T.Command_T_Send (T.Commands.Clear_Fault_Response ((Id => Test_Assembly_Faults.Component_A_Fault_1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Fault_Response_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Fault_Response_Cleared_History.Get_Count, 2);
      Packed_Fault_Id_Assert.Eq (T.Fault_Response_Cleared_History.Get (2), (Id => Test_Assembly_Faults.Component_A_Fault_1));

      -- Check state data product:
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 5);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (5))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Fault_Latched,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Send command to clear a disabled fault response:
      T.Command_T_Send (T.Commands.Clear_Fault_Response ((Id => Test_Assembly_Faults.Component_C_Fault_1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Fault_Response_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Fault_Response_Cleared_History.Get_Count, 3);
      Packed_Fault_Id_Assert.Eq (T.Fault_Response_Cleared_History.Get (3), (Id => Test_Assembly_Faults.Component_C_Fault_1));

      -- Check state data product:
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 6);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (6))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Fault_Latched,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Send command to clear an enabled fault response:
      T.Command_T_Send (T.Commands.Clear_Fault_Response ((Id => Test_Assembly_Faults.Component_A_Fault_1)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Fault_Response_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Fault_Response_Cleared_History.Get_Count, 4);
      Packed_Fault_Id_Assert.Eq (T.Fault_Response_Cleared_History.Get (4), (Id => Test_Assembly_Faults.Component_A_Fault_1));

      -- Check state data product:
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 7);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (7))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Fault_Latched,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Load up a few more faults:
      T.Fault_T_Send ((((15, 12), Test_Assembly_Faults.Component_A_Fault_1, 0), [others => 55]));
      T.Fault_T_Send ((((16, 12), Test_Assembly_Faults.Component_A_Fault_2, 0), [others => 55]));
      T.Fault_T_Send ((((16, 12), Test_Assembly_Faults.Component_B_Fault_3, 0), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 3);

      -- Check latest state data product:
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 9);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (9))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Fault_Detected,
            Component_A_Fault_2_Status => Fault_Latched,
            Component_B_Fault_3_Status => Fault_Latched,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );

      -- Send command to clear all fault responses:
      T.Command_T_Send (T.Commands.Clear_All_Fault_Responses);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_All_Fault_Responses_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.All_Fault_Responses_Cleared_History.Get_Count, 1);

      -- Check state data product:
      Natural_Assert.Eq (T.Fault_Response_Statuses_History.Get_Count, 10);
      Test_Assembly_Fault_Responses_Status_Record_Assert.Eq (
         Test_Assembly_Fault_Responses_Status_Record.Serialization.From_Byte_Array (
            Packed_U32.Serialization.To_Byte_Array (T.Fault_Response_Statuses_History.Get (10))(0 .. 1)
         ),
         (
            Component_A_Fault_1_Status => Nominal,
            Component_A_Fault_2_Status => Nominal,
            Component_B_Fault_3_Status => Nominal,
            Component_B_Fault_1_Status => Disabled,
            Component_C_Fault_1_Status => Disabled,
            Reserved_0 => 0,
            Reserved_1 => 0,
            Reserved_2 => 0
         )
      );
   end Test_Clear_Fault_Response;

   overriding procedure Test_Reset_Data_Products (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- First call the test_fault_handling test to seed the component with some data product data.
      Self.Test_Fault_Handling;

      -- Now send the reset command.
      T.Command_T_Send (T.Commands.Reset_Data_Products);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Reset_Data_Products_Id, Status => Success));

      -- Check data products.
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 20);
      Natural_Assert.Eq (T.Fault_Counter_History.Get_Count, 6);
      Packed_U16_Assert.Eq (T.Fault_Counter_History.Get (6), (Value => 0));
      Natural_Assert.Eq (T.Last_Fault_Id_Received_History.Get_Count, 6);
      Packed_Fault_Id_Assert.Eq (T.Last_Fault_Id_Received_History.Get (6), (Id => 0));
      Natural_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get_Count, 6);
      Sys_Time_Assert.Eq (T.Time_Of_Last_Fault_Received_History.Get (6), (0, 0));
   end Test_Reset_Data_Products;

   overriding procedure Test_Unrecognized_Fault_Id (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send enable command:
      T.Command_T_Send (T.Commands.Enable_Fault_Response ((Id => 99)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Fault_Response_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Unrecognized_Fault_Id_History.Get_Count, 1);
      Packed_Fault_Id_Assert.Eq (T.Unrecognized_Fault_Id_History.Get (1), (Id => 99));

      -- Send disable command:
      T.Command_T_Send (T.Commands.Disable_Fault_Response ((Id => 99)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Fault_Response_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unrecognized_Fault_Id_History.Get_Count, 2);
      Packed_Fault_Id_Assert.Eq (T.Unrecognized_Fault_Id_History.Get (2), (Id => 99));

      -- Send disable command:
      T.Command_T_Send (T.Commands.Clear_Fault_Response ((Id => 99)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Clear_Fault_Response_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unrecognized_Fault_Id_History.Get_Count, 3);
      Packed_Fault_Id_Assert.Eq (T.Unrecognized_Fault_Id_History.Get (3), (Id => 99));

      -- Send unrecognized fault:
      T.Fault_T_Send ((((15, 12), 99, 0), [others => 55]));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unrecognized_Fault_Id_History.Get_Count, 4);
      Packed_Fault_Id_Assert.Eq (T.Unrecognized_Fault_Id_History.Get (4), (Id => 99));
   end Test_Unrecognized_Fault_Id;

   overriding procedure Test_Full_Queue (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
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

      -- OK, let's try to send a fault:
      T.Expect_Fault_T_Send_Dropped := True;
      T.Fault_T_Send ((((15, 12), 99, 0), [others => 55]));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Fault_Dropped_History.Get_Count, 1);
      Fault_Header_Assert.Eq (T.Fault_Dropped_History.Get (1), ((15, 12), 99, 0));
   end Test_Full_Queue;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Fault_Correction.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Disable_Fault_Response ((Id => 99));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Fault_Response_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Disable_Fault_Response_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

end Fault_Correction_Tests.Implementation;
