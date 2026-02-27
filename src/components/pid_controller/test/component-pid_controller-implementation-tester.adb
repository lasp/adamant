--------------------------------------------------------------------------------
-- Pid_Controller Component Tester Body
--------------------------------------------------------------------------------

-- Includes:
with Parameter;

package body Component.Pid_Controller.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Control_Output_U_Recv_Sync_History.Init (Depth => 500);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 500);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 500);
      Self.Sys_Time_T_Return_History.Init (Depth => 500);
      Self.Event_T_Recv_Sync_History.Init (Depth => 500);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 500);
      -- Event histories:
      Self.Invalid_Command_Received_History.Init (Depth => 500);
      Self.Invalid_Parameter_Received_History.Init (Depth => 500);
      Self.Database_Update_Period_Set_History.Init (Depth => 500);
      Self.Diagnostics_Started_History.Init (Depth => 500);
      Self.Set_Controller_Statistics_Duration_History.Init (Depth => 500);
      Self.Set_Controller_Statistics_Duration_Too_Large_History.Init (Depth => 500);
      -- Data product histories:
      Self.P_Output_History.Init (Depth => 500);
      Self.I_Output_History.Init (Depth => 500);
      Self.D_Output_History.Init (Depth => 500);
      Self.Ff_Output_History.Init (Depth => 500);
      Self.Pid_Error_History.Init (Depth => 500);
      Self.Pid_Error_Mean_History.Init (Depth => 500);
      Self.Pid_Error_Variance_History.Init (Depth => 500);
      Self.Pid_Error_Max_History.Init (Depth => 500);
      -- Packet histories:
      Self.Pid_Controller_Diagnostic_Packet_History.Init (Depth => 500);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Control_Output_U_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Invalid_Command_Received_History.Destroy;
      Self.Invalid_Parameter_Received_History.Destroy;
      Self.Database_Update_Period_Set_History.Destroy;
      Self.Diagnostics_Started_History.Destroy;
      Self.Set_Controller_Statistics_Duration_History.Destroy;
      Self.Set_Controller_Statistics_Duration_Too_Large_History.Destroy;
      -- Data product histories:
      Self.P_Output_History.Destroy;
      Self.I_Output_History.Destroy;
      Self.D_Output_History.Destroy;
      Self.Ff_Output_History.Destroy;
      Self.Pid_Error_History.Destroy;
      Self.Pid_Error_Mean_History.Destroy;
      Self.Pid_Error_Variance_History.Destroy;
      Self.Pid_Error_Max_History.Destroy;
      -- Packet histories:
      Self.Pid_Controller_Diagnostic_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Control_Output_U_Send (To_Component => Self'Unchecked_Access, Hook => Self.Control_Output_U_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Attach_Control_Input_U_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Control_Input_U_Recv_Sync_Access);
      Self.Attach_Parameter_Update_T_Provide (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Parameter_Update_T_Modify_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The connector for sending the calculated PID controller output.
   overriding procedure Control_Output_U_Recv_Sync (Self : in out Instance; Arg : in Control_Output.U) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Control_Output_U_Recv_Sync_History.Push (Arg);
   end Control_Output_U_Recv_Sync;

   -- Packet for sending diagnostic packets.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- The Event connector
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- Invalid parameter update
   overriding procedure Invalid_Parameter_Received (Self : in out Instance; Arg : in Invalid_Parameter_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Parameter_Received_History.Push (Arg);
   end Invalid_Parameter_Received;

   -- The event to indicate that the database update period was commanded.
   overriding procedure Database_Update_Period_Set (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Database_Update_Period_Set_History.Push (Arg);
   end Database_Update_Period_Set;

   -- This event indicates that the diagnostic packet request command has been sent.
   overriding procedure Diagnostics_Started (Self : in out Instance; Arg : in Packed_Natural_Duration.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Diagnostics_Started_History.Push (Arg);
   end Diagnostics_Started;

   -- This event indicates that the command to change the duration that statistics are collected was received and changed.
   overriding procedure Set_Controller_Statistics_Duration (Self : in out Instance; Arg : in Packed_Positive.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Set_Controller_Statistics_Duration_History.Push (Arg);
   end Set_Controller_Statistics_Duration;

   -- This event indicates that the command to change the duration that statistics are collected was received but failed to change the length.
   overriding procedure Set_Controller_Statistics_Duration_Too_Large (Self : in out Instance; Arg : in Packed_Positive.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Set_Controller_Statistics_Duration_Too_Large_History.Push (Arg);
   end Set_Controller_Statistics_Duration_Too_Large;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the pid controller component.
   -- The output proportional value of the last control cycle used to help determine how to get to the desired location.
   overriding procedure P_Output (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.P_Output_History.Push (Arg);
   end P_Output;

   -- The output integrator value of the last control cycle used to help smoothly get to the desired location as well as determine overshoot and settling time.
   overriding procedure I_Output (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.I_Output_History.Push (Arg);
   end I_Output;

   -- The output derivative value of the last control cycle which determines how fast the controller reaches its desired location.
   overriding procedure D_Output (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.D_Output_History.Push (Arg);
   end D_Output;

   -- The output of the last feed forward value used in the controller to overcome sources of friction.
   overriding procedure Ff_Output (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ff_Output_History.Push (Arg);
   end Ff_Output;

   -- The output of the last control cycle error calculated by the controller.
   overriding procedure Pid_Error (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pid_Error_History.Push (Arg);
   end Pid_Error;

   -- The mean value of error seen in the controller over a desired, and set data length.
   overriding procedure Pid_Error_Mean (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pid_Error_Mean_History.Push (Arg);
   end Pid_Error_Mean;

   -- The variance of the error seen in the controller over a desired, and set data length.
   overriding procedure Pid_Error_Variance (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pid_Error_Variance_History.Push (Arg);
   end Pid_Error_Variance;

   -- The max error seen in the controller over a desired, and set data length.
   overriding procedure Pid_Error_Max (Self : in out Instance; Arg : in Packed_F32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pid_Error_Max_History.Push (Arg);
   end Pid_Error_Max;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the PID controller component.
   -- The diagnostic packet that is issued based on the number of samples set by command. Samples are taken at the control rate. Includes error, reference, and current.
   overriding procedure Pid_Controller_Diagnostic_Packet (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Pid_Controller_Diagnostic_Packet_History.Push (Arg);
   end Pid_Controller_Diagnostic_Packet;

   -----------------------------------------------
   -- Special primitives for aiding in the staging,
   -- fetching, and updating of parameters
   -----------------------------------------------
   not overriding function Stage_Parameter (Self : in out Instance; Par : in Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (Table_Id => 1, Operation => Stage, Status => Success, Param => Par);
   begin
      Self.Parameter_Update_T_Provide (Param_Update);
      return Param_Update.Status;
   end Stage_Parameter;

   not overriding function Fetch_Parameter (Self : in out Instance; Id : in Parameter_Types.Parameter_Id; Par : out Parameter.T) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (Table_Id => 1, Operation => Fetch, Status => Success, Param => (Header => (Id => Id, Buffer_Length => 0), Buffer => [others => 0]));
   begin
      -- Set the ID to fetch:
      Param_Update.Param.Header.Id := Id;
      Self.Parameter_Update_T_Provide (Param_Update);
      Par := Param_Update.Param;
      return Param_Update.Status;
   end Fetch_Parameter;

   not overriding function Update_Parameters (Self : in out Instance) return Parameter_Update_Status.E is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      Param_Update : Parameter_Update.T := (Table_Id => 1, Operation => Update, Status => Success, Param => ((0, 0), [others => 0]));
   begin
      Self.Parameter_Update_T_Provide (Param_Update);
      return Param_Update.Status;
   end Update_Parameters;

   -----------------------------------------------
   -- Custom Functions for White Box Testing:
   -----------------------------------------------

   not overriding function Get_Diagnostic_Subpacket_Count (Self : in Instance) return Integer is
   begin
      return Self.Component_Instance.Diagnostic_Counter.Get_Count;
   end Get_Diagnostic_Subpacket_Count;

end Component.Pid_Controller.Implementation.Tester;
