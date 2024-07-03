--------------------------------------------------------------------------------
-- Precision_Time_Protocol_Master Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Interfaces; use Interfaces;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Ptp_Time_Message.Assertion; use Ptp_Time_Message.Assertion;
with Unexpected_Ptp_Transaction_Count.Assertion; use Unexpected_Ptp_Transaction_Count.Assertion;
with Ptp_Enums; use Ptp_Enums.Ptp_Message_Type;
with Ptp_State.Assertion; use Ptp_State.Assertion;
use Ptp_State;

package body Precision_Time_Protocol_Master_Tests.Implementation is

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
      Self.Tester.Component_Instance.Init (Sync_Period => 3, Enabled_State => Ptp_State.Enabled);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Time_Sync (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;

      -- Test data products on startup:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Delay_Request_Messages_Received_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Delay_Request_Messages_Received_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Unexpected_Messages_Received_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Unexpected_Messages_Received_History.Get (1), (Value => 0));
      Natural_Assert.Eq (T.Precision_Time_Protocol_State_History.Get_Count, 1);
      Ptp_State_Assert.Eq (T.Precision_Time_Protocol_State_History.Get (1), (State => Enabled));

      -- Send the component ticks, and make sure ptp messages sent at appropriate time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (1), (Message_Type => Sync, Transaction_Count => 1, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (2), (Value => 1));

      -- Send the component ticks, and make sure ptp messages sent at appropriate time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 2);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (2), (Message_Type => Sync, Transaction_Count => 2, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (3), (Value => 2));

      -- Send the component ticks, and make sure ptp messages sent at appropriate time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 3);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (3), (Message_Type => Sync, Transaction_Count => 3, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 4);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (4), (Value => 3));

      -- No events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Time_Sync;

   overriding procedure Test_Follow_Up (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- A follow up should be sent every time we call the follow up connector:
      T.Follow_Up_Sys_Time_T_Send (((99, 100)));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (1), (Message_Type => Follow_Up, Transaction_Count => 0, Time_Stamp => (99, 100)));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get (1), (Value => 1));

      -- A follow up should be sent every time we call the follow up connector:
      T.Follow_Up_Sys_Time_T_Send (((101, 102)));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 2);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (2), (Message_Type => Follow_Up, Transaction_Count => 0, Time_Stamp => (101, 102)));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get (2), (Value => 2));

      --
      -- Send some ticks to make sure sequence count gets incremented in the follow up
      --

      -- Send the component ticks, and make sure ptp messages sent at appropriate time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 3);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (3), (Message_Type => Sync, Transaction_Count => 1, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (1), (Value => 1));

      -- Send the component ticks, and make sure ptp messages sent at appropriate time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 4);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (4), (Message_Type => Sync, Transaction_Count => 2, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (2), (Value => 2));

      --
      -- Ok test sending another follow up
      --

      -- A follow up should be sent every time we call the follow up connector:
      T.Follow_Up_Sys_Time_T_Send (((109, 119)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 5);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (5), (Message_Type => Follow_Up, Transaction_Count => 2, Time_Stamp => (109, 119)));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Follow_Up_Messages_Sent_History.Get (3), (Value => 3));

      -- No events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Follow_Up;

   overriding procedure Test_Delay_Request (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      --
      -- We need to send a tick first for the nominal case. Otherwise the component is not expecting
      -- any delay request message, because it hasn't sent a sync yet.
      --
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (1), (Message_Type => Sync, Transaction_Count => 1, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (1), (Value => 1));

      -- A delay response should be sent every time we call the delay request connector:
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (1, 3), Message => (Message_Type => Delay_Request, Transaction_Count => 1, Time_Stamp => (99, 100))));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 2);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (2), (Message_Type => Delay_Response, Transaction_Count => 1, Time_Stamp => (1, 3)));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Delay_Request_Messages_Received_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Delay_Request_Messages_Received_History.Get (1), (Value => 1));

      -- A delay response should be sent every time we call the delay request connector:
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (0, 0), Message => (Message_Type => Delay_Request, Transaction_Count => 1, Time_Stamp => (99, 100))));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 3);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (3), (Message_Type => Delay_Response, Transaction_Count => 1, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Delay_Request_Messages_Received_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Delay_Request_Messages_Received_History.Get (2), (Value => 2));

      --
      -- Send some ticks to make sure sequence count gets incremented in the follow up
      --

      -- Send the component ticks, and make sure ptp messages sent at appropriate time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 4);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (4), (Message_Type => Sync, Transaction_Count => 2, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (2), (Value => 2));

      --
      -- Ok test sending another delay request
      --

      -- A delay response should be sent every time we call the delay request connector:
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (0, 0), Message => (Message_Type => Delay_Request, Transaction_Count => 2, Time_Stamp => (99, 100))));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 5);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (5), (Message_Type => Delay_Response, Transaction_Count => 2, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Delay_Request_Messages_Received_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Delay_Request_Messages_Received_History.Get (3), (Value => 3));

      -- No events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Delay_Request;

   overriding procedure Test_Unexpected_Message_Received (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- A delay response with sequence count zero should be rejected on startup, since we haven't sent a
      -- sync message yet.
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (0, 0), Message => (Message_Type => Delay_Request, Transaction_Count => 1, Time_Stamp => (99, 100))));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Unexpected_Messages_Received_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Unexpected_Messages_Received_History.Get (1), (Value => 1));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Unexpected_Transaction_Count_History.Get_Count, 1);
      Unexpected_Ptp_Transaction_Count_Assert.Eq (T.Unexpected_Transaction_Count_History.Get (1), (Message => (Message_Type => Delay_Request, Transaction_Count => 1, Time_Stamp => (99, 100)), Expected_Transaction_Count => 0));

      -- Try another:
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (0, 0), Message => (Message_Type => Delay_Request, Transaction_Count => 99, Time_Stamp => (99, 77))));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unexpected_Messages_Received_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Unexpected_Messages_Received_History.Get (2), (Value => 2));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unexpected_Transaction_Count_History.Get_Count, 2);
      Unexpected_Ptp_Transaction_Count_Assert.Eq (T.Unexpected_Transaction_Count_History.Get (2), (Message => (Message_Type => Delay_Request, Transaction_Count => 99, Time_Stamp => (99, 77)), Expected_Transaction_Count => 0));

      -- Send a message with a type other than delay request:
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (0, 0), Message => (Message_Type => Sync, Transaction_Count => 99, Time_Stamp => (99, 77))));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unexpected_Messages_Received_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Unexpected_Messages_Received_History.Get (3), (Value => 3));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unexpected_Message_Type_History.Get_Count, 1);
      Ptp_Time_Message_Assert.Eq (T.Unexpected_Message_Type_History.Get (1), (Message_Type => Sync, Transaction_Count => 99, Time_Stamp => (99, 77)));

      -- Try another
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (0, 0), Message => (Message_Type => Follow_Up, Transaction_Count => 99, Time_Stamp => (99, 77))));
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unexpected_Messages_Received_History.Get_Count, 4);
      Packed_U16_Assert.Eq (T.Unexpected_Messages_Received_History.Get (4), (Value => 4));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unexpected_Message_Type_History.Get_Count, 2);
      Ptp_Time_Message_Assert.Eq (T.Unexpected_Message_Type_History.Get (2), (Message_Type => Follow_Up, Transaction_Count => 99, Time_Stamp => (99, 77)));
   end Test_Unexpected_Message_Received;

   -- This test ensures that the enable/disable commands work as intended.
   overriding procedure Test_Enable_Disable (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Disable:
      T.Command_T_Send (T.Commands.Disable_Precision_Time_Protocol);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Disable_Precision_Time_Protocol_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ptp_Disabled_History.Get_Count, 1);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Precision_Time_Protocol_State_History.Get_Count, 1);
      Ptp_State_Assert.Eq (T.Precision_Time_Protocol_State_History.Get (1), (State => Disabled));

      -- No syncing messages should be sent:
      for Idx in 0 .. 100 loop
         T.Tick_T_Send (((0, 0), 0));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      end loop;

      -- Set period to one:
      Self.Tester.Component_Instance.Init (Sync_Period => 1, Enabled_State => Ptp_State.Enabled);

      -- Enable
      T.Command_T_Send (T.Commands.Enable_Precision_Time_Protocol);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Precision_Time_Protocol_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Ptp_Enabled_History.Get_Count, 1);

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Precision_Time_Protocol_State_History.Get_Count, 2);
      Ptp_State_Assert.Eq (T.Precision_Time_Protocol_State_History.Get (2), (State => Enabled));

      -- Syncing messages should be sent every tick:
      for Idx in 1 .. 5 loop
         T.Tick_T_Send (((0, 0), 0));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, Idx);
         Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (Idx), (Message_Type => Sync, Transaction_Count => Interfaces.Unsigned_16 (Idx), Time_Stamp => T.System_Time));
      end loop;
   end Test_Enable_Disable;

   overriding procedure Test_Sync_Once (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Init with syncing disabled:
      Self.Tester.Component_Instance.Init (Sync_Period => 1, Enabled_State => Ptp_State.Disabled);

      -- No syncing messages should be sent:
      for Idx in 0 .. 100 loop
         T.Tick_T_Send (((0, 0), 0));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      end loop;

      -- Send sync once command:
      T.Command_T_Send (T.Commands.Sync_Once);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Sync_Once_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Syncing_Once_History.Get_Count, 1);

      -- Send tick
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (1), (Message_Type => Sync, Transaction_Count => 1, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (1), (Value => 1));

      -- No syncing messages should be sent:
      for Idx in 0 .. 100 loop
         T.Tick_T_Send (((0, 0), 0));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 1);
      end loop;

      -- Send sync once command:
      T.Command_T_Send (T.Commands.Sync_Once);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Sync_Once_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Syncing_Once_History.Get_Count, 2);

      -- Send tick
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 2);
      Ptp_Time_Message_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get (2), (Message_Type => Sync, Transaction_Count => 2, Time_Stamp => T.System_Time));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Transaction_Number_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Transaction_Number_History.Get (2), (Value => 2));

      -- No syncing messages should be sent:
      for Idx in 0 .. 100 loop
         T.Tick_T_Send (((0, 0), 0));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 2);
      end loop;
   end Test_Sync_Once;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Enable_Precision_Time_Protocol;
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 1;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Enable_Precision_Time_Protocol_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ptp_Enabled_History.Get_Count, 0);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Enable_Precision_Time_Protocol_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 1]));
   end Test_Invalid_Command;

   overriding procedure Test_Queue_Overflow (Self : in out Instance) is
      T : Component.Precision_Time_Protocol_Master.Implementation.Tester.Instance_Access renames Self.Tester;
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
      Natural_Assert.Eq (T.Queue_Overflowed_History.Get_Count, 1);

      -- OK the next ptp message should overflow the queue.
      T.Expect_Ptp_Time_Message_Receive_T_Send_Dropped := True;
      T.Ptp_Time_Message_Receive_T_Send ((Receive_Time => (0, 0), Message => (Message_Type => Delay_Request, Transaction_Count => 1, Time_Stamp => (99, 100))));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Queue_Overflowed_History.Get_Count, 2);

      -- OK the next tick should overflow the queue.
      T.Expect_Tick_T_Send_Dropped := True;
      T.Tick_T_Send (((0, 0), 0));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Queue_Overflowed_History.Get_Count, 3);

      -- OK the next follow up should overflow the queue.
      T.Expect_Follow_Up_Sys_Time_T_Send_Dropped := True;
      T.Follow_Up_Sys_Time_T_Send (((99, 100)));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Queue_Overflowed_History.Get_Count, 4);

      -- Make sure not data products or ptp messages sent:
      Natural_Assert.Eq (T.Ptp_Time_Message_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Dispatch should pop 3 items:
      Natural_Assert.Eq (T.Dispatch_All, 3);
   end Test_Queue_Overflow;

end Precision_Time_Protocol_Master_Tests.Implementation;
