--------------------------------------------------------------------------------
-- Limiter Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Tick.Assertion; use Tick.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Parameter_Enums.Assertion; use Parameter_Enums;
use Parameter_Enums.Parameter_Operation_Type;
use Parameter_Enums.Parameter_Update_Status;
use Parameter_Enums.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Invalid_Parameter_Info.Assertion; use Invalid_Parameter_Info.Assertion;
with Interfaces; use Interfaces;
with Parameter.Assertion; use Parameter.Assertion;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Safe_Deallocator;

package body Limiter_Tests.Implementation is

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
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 4);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Max_Sends_Per_Tick => 3);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      -- Free the tester component:
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Component_Tester_Package.Instance, Name => Component_Tester_Package.Instance_Access);
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
      Free_If_Testing (Self.Tester);
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Limiting (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 0);
   begin
      -- Send some ticks, expect no sends since no data was enqueued.
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 0);

      -- Enqueue one piece of data, make sure one send occurs.
      T.T_Send (((0, 0), 1));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 25);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 1);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (1), ((0, 0), 1));

      -- Enqueue two pieces of data, make sure two sends occur.
      T.T_Send (((0, 0), 2));
      T.T_Send (((0, 0), 3));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 1);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 50);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (2), ((0, 0), 2));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (3), ((0, 0), 3));

      -- Enqueue three pieces of data, make sure three sends occur.
      T.T_Send (((0, 0), 4));
      T.T_Send (((0, 0), 5));
      T.T_Send (((0, 0), 6));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 75);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 6);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (4), ((0, 0), 4));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (5), ((0, 0), 5));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (6), ((0, 0), 6));

      -- Enqueue four pieces of data, make sure three sends occur.
      T.T_Send (((0, 0), 7));
      T.T_Send (((0, 0), 8));
      T.T_Send (((0, 0), 9));
      T.T_Send (((0, 0), 10));
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 6);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 100);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 9);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (7), ((0, 0), 7));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (8), ((0, 0), 8));
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (9), ((0, 0), 9));

      -- Send another tick and the remaining item on the queue
      -- should be sent out.
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 10);
      Tick_Assert.Eq (T.T_Recv_Sync_History.Get (10), ((0, 0), 10));

      -- The next tick should produce nothing.
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 10);

      -- Check queue usage:
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 0);
   end Test_Nominal_Limiting;

   overriding procedure Test_Change_Rate_Command (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 0);
   begin
      -- Check data product at start:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (1), (Value => 3));
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Max_Packet_Sends_Per_Tick_History.Clear;

      -- Send a command change sends per tick to zero:
      T.Command_T_Send (T.Commands.Sends_Per_Tick ((Value => 0)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Sends_Per_Tick_Id, Status => Success));

      -- Check event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Max_Send_Per_Tick_Set_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Max_Send_Per_Tick_Set_History.Get (1), (Value => 0));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (1), (Value => 0));

      -- Send some ticks and data, expect no sends, since sends are disabled.
      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);
      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);
      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);
      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 100);

      -- Send a command change sends per tick to 2:
      T.Command_T_Send (T.Commands.Sends_Per_Tick ((Value => 2)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Sends_Per_Tick_Id, Status => Success));

      -- Check event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Max_Send_Per_Tick_Set_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Max_Send_Per_Tick_Set_History.Get (2), (Value => 2));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (2), (Value => 2));

      -- Send tick, expect two items to be sent.
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 2);

      -- Send a command change sends per tick to 1:
      T.Command_T_Send (T.Commands.Sends_Per_Tick ((Value => 1)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Sends_Per_Tick_Id, Status => Success));

      -- Check event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Max_Send_Per_Tick_Set_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Max_Send_Per_Tick_Set_History.Get (3), (Value => 1));

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (3), (Value => 1));

      -- Drain the queue.
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 4);
   end Test_Change_Rate_Command;

   overriding procedure Test_Change_Rate_Parameter (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      The_Tick : constant Tick.T := ((0, 0), 0);
      Param : Parameter.T;
   begin
      -- Check data product at start:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (1), (Value => 3));
      T.Data_Product_T_Recv_Sync_History.Clear;
      T.Max_Packet_Sends_Per_Tick_History.Clear;

      -- Set the parameter to change sends per tick to zero
      Parameter_Update_Status_Assert.Eq (T.Stage_Parameter (T.Parameters.Max_Sends_Per_Tick ((Value => 0))), Success);
      Parameter_Update_Status_Assert.Eq (T.Fetch_Parameter (T.Parameters.Get_Max_Sends_Per_Tick_Id, Param), Success);
      Parameter_Update_Status_Assert.Eq (T.Update_Parameters, Success);
      Parameter_Assert.Eq (Param, T.Parameters.Max_Sends_Per_Tick ((Value => 0)));

      -- Send some ticks and data, expect no sends, since sends are disabled.
      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (1), (Value => 0));

      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);
      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);
      T.T_Send (((0, 0), 1));
      T.Tick_T_Send (The_Tick);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 0);
      Byte_Assert.Eq (T.Component_Instance.Get_Queue_Current_Percent_Used, 100);

      -- Set the parameter to change sends per tick to two
      Parameter_Update_Status_Assert.Eq (T.Stage_Parameter (T.Parameters.Max_Sends_Per_Tick ((Value => 2))), Success);
      Parameter_Update_Status_Assert.Eq (T.Fetch_Parameter (T.Parameters.Get_Max_Sends_Per_Tick_Id, Param), Success);
      Parameter_Update_Status_Assert.Eq (T.Update_Parameters, Success);
      Parameter_Assert.Eq (Param, T.Parameters.Max_Sends_Per_Tick ((Value => 2)));

      -- Send tick, expect two items to be sent.
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 2);

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (2), (Value => 2));

      -- Set the parameter to change sends per tick to 1
      Parameter_Update_Status_Assert.Eq (T.Stage_Parameter (T.Parameters.Max_Sends_Per_Tick ((Value => 1))), Success);
      Parameter_Update_Status_Assert.Eq (T.Fetch_Parameter (T.Parameters.Get_Max_Sends_Per_Tick_Id, Param), Success);
      Parameter_Update_Status_Assert.Eq (T.Update_Parameters, Success);
      Parameter_Assert.Eq (Param, T.Parameters.Max_Sends_Per_Tick ((Value => 1)));

      -- Drain the queue.
      T.Tick_T_Send (The_Tick);

      -- Check data product:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Max_Packet_Sends_Per_Tick_History.Get (3), (Value => 1));

      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 4);
      T.Tick_T_Send (The_Tick);
      Natural_Assert.Eq (T.T_Recv_Sync_History.Get_Count, 4);
   end Test_Change_Rate_Parameter;

   overriding procedure Test_Queue_Overflow (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
   begin
      -- Four ticks should fill the queue.
      T.T_Send (((0, 0), 1));
      T.T_Send (((0, 0), 1));
      T.T_Send (((0, 0), 1));
      T.T_Send (((0, 0), 1));

      -- OK the next command should overflow the queue.
      T.Expect_T_Send_Dropped := True;
      T.T_Send (((0, 0), 1));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Dropped_History.Get_Count, 1);
   end Test_Queue_Overflow;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Sends_Per_Tick ((Value => 0));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Sends_Per_Tick_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Sends_Per_Tick_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

   overriding procedure Test_Invalid_Parameter (Self : in out Instance) is
      T : Component_Tester_Package.Instance_Access renames Self.Tester;
      Param : Parameter.T := T.Parameters.Max_Sends_Per_Tick ((Value => 0));
   begin
      -- Make the parameter invalid by modifying its length.
      Param.Header.Buffer_Length := 0;

      -- Send bad parameter and expect bad response:
      Parameter_Update_Status_Assert.Eq (T.Stage_Parameter (Param), Length_Error);
      Parameter_Update_Status_Assert.Eq (T.Fetch_Parameter (T.Parameters.Get_Max_Sends_Per_Tick_Id, Param), Success);
      Parameter_Assert.Neq (Param, T.Parameters.Max_Sends_Per_Tick ((Value => 0)));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Parameter_Received_History.Get_Count, 1);
      Invalid_Parameter_Info_Assert.Eq (T.Invalid_Parameter_Received_History.Get (1), (Id => T.Parameters.Get_Max_Sends_Per_Tick_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));

      -- Make the parameter invalid by setting a crazy id;
      Param.Header.Id := 1_001;

      -- Send bad command and expect bad response:
      Parameter_Update_Status_Assert.Eq (T.Stage_Parameter (Param), Id_Error);
      Parameter_Update_Status_Assert.Eq (T.Fetch_Parameter (T.Parameters.Get_Max_Sends_Per_Tick_Id, Param), Success);
      Parameter_Assert.Neq (Param, T.Parameters.Max_Sends_Per_Tick ((Value => 0)));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Parameter_Received_History.Get_Count, 2);
      Invalid_Parameter_Info_Assert.Eq (T.Invalid_Parameter_Received_History.Get (2), (Id => 1_001, Errant_Field_Number => Interfaces.Unsigned_32'Last - 1, Errant_Field => [0, 0, 0, 0, 0, 0, 16#03#, 16#E9#]));
   end Test_Invalid_Parameter;

end Limiter_Tests.Implementation;
