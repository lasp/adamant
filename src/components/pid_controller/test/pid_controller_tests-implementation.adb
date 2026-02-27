--------------------------------------------------------------------------------
-- Pid_Controller Tests Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Basic_Assertions; use Basic_Assertions;
with Command;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Interfaces; use Interfaces;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Invalid_Parameter_Info.Assertion; use Invalid_Parameter_Info.Assertion;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Packed_F32.Assertion; use Packed_F32.Assertion;
with Parameter;
with Parameter_Enums.Assertion;
use Parameter_Enums.Parameter_Update_Status;
use Parameter_Enums.Assertion;
with Pid_Diagnostic_Subpacket;
with Packet_Types;

package body Pid_Controller_Tests.Implementation is

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
      --Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

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

   overriding procedure Test_Diagnostic_Packet (Self : in out Instance) is
      use Packet_Types;
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
      Subpacket_Duration : Natural;
      Extra_Packets : constant Natural := 2;
      Packet_Buffer_Length : constant Natural := Packet_Buffer_Type'Length;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Diagnostic Packet:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- The packet is sent on some period of the control, but we do not necessarily care about the control values yet.
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, True));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure that the first time with a count, we have the packet setup with that count.
      T.Command_T_Send (T.Commands.Start_Diagnostics ((Duration => 2)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Get_Diagnostic_Subpacket_Count, 2);

      -- Induce the packet to send
      -- Note: This works due to the parameters. Values of gains here are just 0 for pass through values
      T.Control_Input_U_Send (((0, 0), 1.0, 1.0, 1.0, False));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get_Count, 0);
      T.Control_Input_U_Send (((0, 0), 2.0, 2.0, 2.0, False));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get_Count, 1);

      -- Check that we got the count in the buffer and that we got the expected patterns in the two subpackets
      -- Using a floating point converter we know that 1.0 should be 0x3F800000 or 63, 128, 0, 0
      -- Using a floating point converter we know that 2.0 should be 0x40000000 or 64, 0, 0, 0
      Byte_Array_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get (1).Buffer (0 .. 3), [0, 0, 0, 2]);
      Byte_Array_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get (1).Buffer (4 .. 15), [63, 128, 0, 0, 63, 128, 0, 0, 63, 128, 0, 0]); --subpacket from the first control input
      Byte_Array_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get (1).Buffer (16 .. 27), [64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0]); --subpacket from the second control input

      -- Start a new packet and this time let's make sure we fill the packet and send.
      -- Need to find out how many subpackets we should issue to get at least one full packet sent plus a couple more
      Subpacket_Duration := Natural (Packet_Buffer_Length / Pid_Diagnostic_Subpacket.Max_Serialized_Length) + Extra_Packets;

      T.Command_T_Send (T.Commands.Start_Diagnostics ((Duration => Subpacket_Duration)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (Self.Tester.Get_Diagnostic_Subpacket_Count, Subpacket_Duration);

      -- Induce the packet to send
      while Subpacket_Duration > Extra_Packets loop
         Subpacket_Duration := @ - 1;

         T.Control_Input_U_Send (((0, 0), 1.0, 1.0, 1.0, False));
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
         Natural_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get_Count, 1);

      end loop;

      -- Check that on this control cycle we got the full packet
      T.Control_Input_U_Send (((0, 0), 1.0, 1.0, 1.0, False));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get_Count, 2);
      Byte_Array_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get (2).Buffer (0 .. 3), [0, 0, 0, 103]);

      -- Now make sure there was one more packet with just a couple sub packets
      T.Control_Input_U_Send (((0, 0), 1.0, 1.0, 1.0, False));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get_Count, 3);
      Byte_Array_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get (3).Buffer (0 .. 3), [0, 0, 0, 2]);

      -- Make sure no others are issued
      T.Control_Input_U_Send (((0, 0), 1.0, 1.0, 1.0, False));
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pid_Controller_Diagnostic_Packet_History.Get_Count, 3);

      --

   end Test_Diagnostic_Packet;

   overriding procedure Test_Update_Data_Products (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
   begin

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- Send in some values that should make it into the data products
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, True));

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 1);

      -- Make sure that the values make sense for what was sent in
      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (1), (Value => 1.0));
      Short_Float_Assert.Eq (T.Pid_Error_Mean_History.Get (1).Value, 1.0, Epsilon => 0.001); -- only one sample so far
      Short_Float_Assert.Eq (T.Pid_Error_Variance_History.Get (1).Value, 0.0, Epsilon => 0.001);
      Packed_F32_Assert.Eq (T.Pid_Error_Max_History.Get (1), (Value => 1.0));

      -- Fill the entire statistics array
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 0.0, 2.0, 0.0, False));

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 2);

      -- Now check that the values are what is expected after 4 control cycles
      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (2), (Value => 2.0));
      Short_Float_Assert.Eq (T.Pid_Error_Mean_History.Get (2).Value, 1.25, Epsilon => 0.001);
      Packed_F32_Assert.Eq (T.Pid_Error_Max_History.Get (2), (Value => 2.0));

      -- Perform a new control and make sure the values reset
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, True));
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, False));

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 3);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 3);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 3);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 3);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 3);

      -- Make sure that the values are 0 until we have a full set of data
      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (1), (Value => 1.0));
      Short_Float_Assert.Eq (T.Pid_Error_Mean_History.Get (1).Value, 1.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (T.Pid_Error_Variance_History.Get (1).Value, 0.0, Epsilon => 0.001);
      Packed_F32_Assert.Eq (T.Pid_Error_Max_History.Get (1), (Value => 1.0));

   end Test_Update_Data_Products;

   overriding procedure Test_Pid_Controller (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
      Status : Parameter_Enums.Parameter_Update_Status.E;
      Param : Parameter.T;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing PID Controller:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- Set our gain parameters to something simple to test the controller
      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.P_Gain ((Value => 1.0)));
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_P_Gain_Id, Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);

      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.I_Gain ((Value => 100.0)));
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_I_Gain_Id, Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);

      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.D_Gain ((Value => 3.0)));
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_D_Gain_Id, Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);

      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.N_Filter ((Value => 4.0)));
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_N_Filter_Id, Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);

      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.I_Max_Limit ((Value => 1.0)));
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_I_Max_Limit_Id, Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Status := Self.Tester.Stage_Parameter (Self.Tester.Parameters.I_Min_Limit ((Value => -1.0)));
      Parameter_Update_Status_Assert.Eq (Status, Success);
      Status := Self.Tester.Fetch_Parameter (Self.Tester.Parameters.Get_I_Min_Limit_Id, Param);
      pragma Unreferenced (Param);
      Parameter_Update_Status_Assert.Eq (Status, Success);

      Parameter_Update_Status_Assert.Eq (Self.Tester.Update_Parameters, Success);

      -- Send in some values that are simple enough to calculate by hand and test the controller
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, True));

      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (1), (Value => 1.0));
      Short_Float_Assert.Eq (T.Control_Output_U_Recv_Sync_History.Get (1).Output_Value, 13.0, Epsilon => 0.001);

      -- Now let's test the limiting on the integral term
      T.Control_Input_U_Send (((0, 0), 1.0, 5.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 1.0, 5.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 1.0, 5.0, 0.0, False));

      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (2), (Value => 4.0));
      Short_Float_Assert.Eq (T.Control_Output_U_Recv_Sync_History.Get (4).Output_Value, 48.794, Epsilon => 0.001);

      -- Now let's test the limiting on the integral term in the negative direction
      T.Control_Input_U_Send (((0, 0), 10.0, 1.0, 0.0, True));
      T.Control_Input_U_Send (((0, 0), 10.0, 1.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 10.0, 1.0, 0.0, False));

      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (3), (Value => -9.0));
      Short_Float_Assert.Eq (T.Control_Output_U_Recv_Sync_History.Get (7).Output_Value, -109.53, Epsilon => 0.01);

   end Test_Pid_Controller;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Start_Diagnostics ((Duration => 100));
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Invalid Command:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      --Natural_Assert.eq(t.dispatch_all, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Start_Diagnostics_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Start_Diagnostics_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

   overriding procedure Test_Invalid_Parameter (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
      Param : Parameter.T := T.Parameters.P_Gain ((Value => 0.0));
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Invalid Parameter:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- Make the parameter invalid by modifying its length.
      Param.Header.Buffer_Length := 0;

      -- Send bad parameter and expect bad response:
      Parameter_Update_Status_Assert.Eq (T.Stage_Parameter (Param), Length_Error);

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Parameter_Received_History.Get_Count, 1);
      Invalid_Parameter_Info_Assert.Eq (T.Invalid_Parameter_Received_History.Get (1), (Id => T.Parameters.Get_P_Gain_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));

      -- Make the parameter invalid by setting a crazy id;
      Param.Header.Id := 1_001;

      -- Send bad command and expect bad response:
      Parameter_Update_Status_Assert.Eq (T.Stage_Parameter (Param), Id_Error);

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Parameter_Received_History.Get_Count, 2);
      Invalid_Parameter_Info_Assert.Eq (T.Invalid_Parameter_Received_History.Get (2), (Id => 1_001, Errant_Field_Number => Interfaces.Unsigned_32'Last - 1, Errant_Field => [0, 0, 0, 0, 0, 0, 16#03#, 16#E9#]));
   end Test_Invalid_Parameter;

   overriding procedure Test_Database_Update_Period (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Database Update Period:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- Set the database update period should be set to 3 on startup. Let's make sure that is true:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 8);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 2);

      -- Send command to set period to zero, and make sure nothing comes out:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Set_Database_Update_Period ((Value => 0)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Database_Update_Period_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Database_Update_Period_Set_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Database_Update_Period_Set_History.Get (1), (Value => 0));

      -- Nothing should be produced:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 2);

      -- Send command to set period to 2, and make sure something comes out every other:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      T.Command_T_Send (T.Commands.Set_Database_Update_Period ((Value => 2)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Database_Update_Period_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Database_Update_Period_Set_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Database_Update_Period_Set_History.Get (2), (Value => 2));

      -- Updates should be produced every 2 calls:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 16);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 24);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 24);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 32);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 32);
      T.Control_Input_U_Send (((0, 0), 0.0, 0.0, 0.0, False));
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 40);

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 5);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 5);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 5);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 5);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 5);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 5);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 5);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 5);
   end Test_Database_Update_Period;

   overriding procedure Test_Start_Diagnostics_Command (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Set Diagnostic Subpackets:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- Ensure the count is first initialized to 0 which would mean off and no packets by default.
      Natural_Assert.Eq (Self.Tester.Get_Diagnostic_Subpacket_Count, 0);

      -- Send command to set the number of subpackets:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Start_Diagnostics ((Duration => 20)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Start_Diagnostics_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Diagnostics_Started_History.Get_Count, 1);
      Natural_Assert.Eq (Self.Tester.Get_Diagnostic_Subpacket_Count, 20);

      -- Send command to set the number of packets to one. Make sure the counter updates and a packet is sent due to there only being one:
      T.Command_T_Send (T.Commands.Start_Diagnostics ((Duration => 0)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Start_Diagnostics_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Diagnostics_Started_History.Get_Count, 2);
      Natural_Assert.Eq (Self.Tester.Get_Diagnostic_Subpacket_Count, 0);

   end Test_Start_Diagnostics_Command;

   overriding procedure Test_Set_Controller_Statistic_Duration_Command (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      Put_Line ("");
      Put_Line ("----------------------------------");
      Put_Line ("Testing Set Controller Statistics Duration:");
      Put_Line ("----------------------------------");

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 10, Moving_Average_Init_Samples => 5);

      -- Send command to set the number of subpackets:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Set_Controller_Statistic_Duration ((Value => 2)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Controller_Statistic_Duration_Id, Status => Success));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Set_Controller_Statistics_Duration_History.Get_Count, 1);
      Natural_Assert.Eq (T.Set_Controller_Statistics_Duration_Too_Large_History.Get_Count, 0);

      T.Command_T_Send (T.Commands.Set_Controller_Statistic_Duration ((Value => 20)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Controller_Statistic_Duration_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Set_Controller_Statistics_Duration_History.Get_Count, 1);
      Natural_Assert.Eq (T.Set_Controller_Statistics_Duration_Too_Large_History.Get_Count, 1);

   end Test_Set_Controller_Statistic_Duration_Command;

   overriding procedure Test_Moving_Average_Unused (Self : in out Instance) is
      T : Component.Pid_Controller.Implementation.Tester.Instance_Access renames Self.Tester;
   begin

      Self.Tester.Component_Instance.Init (Control_Frequency => 100.0, Database_Update_Period => 3, Moving_Average_Max_Samples => 0, Moving_Average_Init_Samples => 5);

      -- Send in some values that should make it into the data products
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, True));

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 1);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 1);

      -- Make sure that the values make sense for what was sent in
      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (1), (Value => 1.0));
      Short_Float_Assert.Eq (T.Pid_Error_Mean_History.Get (1).Value, 0.0, Epsilon => 0.001); -- only one sample so far
      Short_Float_Assert.Eq (T.Pid_Error_Variance_History.Get (1).Value, 0.0, Epsilon => 0.001);
      Packed_F32_Assert.Eq (T.Pid_Error_Max_History.Get (1), (Value => 0.0));

      -- Fill the entire statistics array
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 1.0, 2.0, 0.0, False));
      T.Control_Input_U_Send (((0, 0), 0.0, 2.0, 0.0, False));

      -- Make sure the correct data products were sent out:
      Natural_Assert.Eq (T.P_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.I_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.D_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Ff_Output_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Mean_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Variance_History.Get_Count, 2);
      Natural_Assert.Eq (T.Pid_Error_Max_History.Get_Count, 2);

      -- Now check that the values are 0 after a full set of cycles. There should be no statistics being performed here.
      Packed_F32_Assert.Eq (T.Pid_Error_History.Get (2), (Value => 2.0));
      Short_Float_Assert.Eq (T.Pid_Error_Mean_History.Get (2).Value, 0.0, Epsilon => 0.001);
      Short_Float_Assert.Eq (T.Pid_Error_Variance_History.Get (1).Value, 0.0, Epsilon => 0.001);
      Packed_F32_Assert.Eq (T.Pid_Error_Max_History.Get (2), (Value => 0.0));

      -- The other ones to test are reset, which should have been covered with the first run of the controller
      -- and we need to test the command to make sure nothing happens there.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      T.Command_T_Send (T.Commands.Set_Controller_Statistic_Duration ((Value => 4)));
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Controller_Statistic_Duration_Id, Status => Failure));
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Set_Controller_Statistics_Duration_History.Get_Count, 0);
      Natural_Assert.Eq (T.Set_Controller_Statistics_Duration_Too_Large_History.Get_Count, 1);

   end Test_Moving_Average_Unused;

end Pid_Controller_Tests.Implementation;
