--------------------------------------------------------------------------------
-- Pid_Controller Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Pid_Controller_Reciprocal;
with Sys_Time;
with Printable_History;
with Control_Output.Representation;
with Packet.Representation;
with Command_Response.Representation;
with Sys_Time.Representation;
with Event.Representation;
with Data_Product.Representation;
with Event;
with Invalid_Command_Info.Representation;
with Invalid_Parameter_Info.Representation;
with Packed_U16.Representation;
with Packed_Natural_Duration.Representation;
with Packed_Positive.Representation;
with Data_Product;
with Packed_F32.Representation;

-- This component is a generic component for PID control that uses proportional, integral, and derivative gains. The component input is the measured and commanded positions which is used to find an error, as well as a feed-forward value to overcome friction and jitter. The component uses the error with the PID gains that are set by the user in the parameter table to perform the correct control for the particular system. Any one of the gains can be set to 0 to turn off that particular term. The component also has the ability to limit the integral term to prevent wind-up of that term and potential kickback in the physical system. There are also optional statistics for the mean, variance, and max of the error which is disabled by setting the Moving_Average_Max_Samples initialization parameter to 0. Lastly, the component also has the ability to produce diagnostics over a particular amount of time set by command, which contains the error and reference positions.
package Component.Pid_Controller.Implementation.Tester is

   use Component.Pid_Controller_Reciprocal;
   -- Invoker connector history packages:
   package Control_Output_U_Recv_Sync_History_Package is new Printable_History (Control_Output.U, Control_Output.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);

   -- Event history packages:
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Invalid_Parameter_Received_History_Package is new Printable_History (Invalid_Parameter_Info.T, Invalid_Parameter_Info.Representation.Image);
   package Database_Update_Period_Set_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Diagnostics_Started_History_Package is new Printable_History (Packed_Natural_Duration.T, Packed_Natural_Duration.Representation.Image);
   package Set_Controller_Statistics_Duration_History_Package is new Printable_History (Packed_Positive.T, Packed_Positive.Representation.Image);
   package Set_Controller_Statistics_Duration_Too_Large_History_Package is new Printable_History (Packed_Positive.T, Packed_Positive.Representation.Image);

   -- Data product history packages:
   package P_Output_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package I_Output_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package D_Output_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package Ff_Output_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package Pid_Error_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package Pid_Error_Mean_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package Pid_Error_Variance_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);
   package Pid_Error_Max_History_Package is new Printable_History (Packed_F32.T, Packed_F32.Representation.Image);

   -- Packet history packages:
   package Pid_Controller_Diagnostic_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Pid_Controller_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Pid_Controller.Implementation.Instance;
      -- Connector histories:
      Control_Output_U_Recv_Sync_History : Control_Output_U_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Invalid_Parameter_Received_History : Invalid_Parameter_Received_History_Package.Instance;
      Database_Update_Period_Set_History : Database_Update_Period_Set_History_Package.Instance;
      Diagnostics_Started_History : Diagnostics_Started_History_Package.Instance;
      Set_Controller_Statistics_Duration_History : Set_Controller_Statistics_Duration_History_Package.Instance;
      Set_Controller_Statistics_Duration_Too_Large_History : Set_Controller_Statistics_Duration_Too_Large_History_Package.Instance;
      -- Data product histories:
      P_Output_History : P_Output_History_Package.Instance;
      I_Output_History : I_Output_History_Package.Instance;
      D_Output_History : D_Output_History_Package.Instance;
      Ff_Output_History : Ff_Output_History_Package.Instance;
      Pid_Error_History : Pid_Error_History_Package.Instance;
      Pid_Error_Mean_History : Pid_Error_Mean_History_Package.Instance;
      Pid_Error_Variance_History : Pid_Error_Variance_History_Package.Instance;
      Pid_Error_Max_History : Pid_Error_Max_History_Package.Instance;
      -- Packet histories:
      Pid_Controller_Diagnostic_Packet_History : Pid_Controller_Diagnostic_Packet_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The connector for sending the calculated PID controller output.
   overriding procedure Control_Output_U_Recv_Sync (Self : in out Instance; Arg : in Control_Output.U);
   -- Packet for sending diagnostic packets.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- The Event connector
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- Invalid parameter update
   overriding procedure Invalid_Parameter_Received (Self : in out Instance; Arg : in Invalid_Parameter_Info.T);
   -- The event to indicate that the database update period was commanded.
   overriding procedure Database_Update_Period_Set (Self : in out Instance; Arg : in Packed_U16.T);
   -- This event indicates that the diagnostic packet request command has been sent.
   overriding procedure Diagnostics_Started (Self : in out Instance; Arg : in Packed_Natural_Duration.T);
   -- This event indicates that the command to change the duration that statistics are collected was received and changed.
   overriding procedure Set_Controller_Statistics_Duration (Self : in out Instance; Arg : in Packed_Positive.T);
   -- This event indicates that the command to change the duration that statistics are collected was received but failed to change the length.
   overriding procedure Set_Controller_Statistics_Duration_Too_Large (Self : in out Instance; Arg : in Packed_Positive.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the pid controller component.
   -- The output proportional value of the last control cycle used to help determine how to get to the desired location.
   overriding procedure P_Output (Self : in out Instance; Arg : in Packed_F32.T);
   -- The output integrator value of the last control cycle used to help smoothly get to the desired location as well as determine overshoot and settling time.
   overriding procedure I_Output (Self : in out Instance; Arg : in Packed_F32.T);
   -- The output derivative value of the last control cycle which determines how fast the controller reaches its desired location.
   overriding procedure D_Output (Self : in out Instance; Arg : in Packed_F32.T);
   -- The output of the last feed forward value used in the controller to overcome sources of friction.
   overriding procedure Ff_Output (Self : in out Instance; Arg : in Packed_F32.T);
   -- The output of the last control cycle error calculated by the controller.
   overriding procedure Pid_Error (Self : in out Instance; Arg : in Packed_F32.T);
   -- The mean value of error seen in the controller over a desired, and set data length.
   overriding procedure Pid_Error_Mean (Self : in out Instance; Arg : in Packed_F32.T);
   -- The variance of the error seen in the controller over a desired, and set data length.
   overriding procedure Pid_Error_Variance (Self : in out Instance; Arg : in Packed_F32.T);
   -- The max error seen in the controller over a desired, and set data length.
   overriding procedure Pid_Error_Max (Self : in out Instance; Arg : in Packed_F32.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the PID controller component.
   -- The diagnostic packet that is issued based on the number of samples set by command. Samples are taken at the control rate. Includes error, reference, and current.
   overriding procedure Pid_Controller_Diagnostic_Packet (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for aiding in the staging,
   -- fetching, and updating of parameters
   -----------------------------------------------
   -- Stage a parameter value within the component
   not overriding function Stage_Parameter (Self : in out Instance; Par : in Parameter.T) return Parameter_Update_Status.E;
   -- Fetch the value of a parameter with the component
   not overriding function Fetch_Parameter (Self : in out Instance; Id : in Parameter_Types.Parameter_Id; Par : out Parameter.T) return Parameter_Update_Status.E;
   -- Tell the component it is OK to atomically update all of its
   -- working parameter values with the staged values.
   not overriding function Update_Parameters (Self : in out Instance) return Parameter_Update_Status.E;

   -----------------------------------------------
   -- Custom Functions for White Box Testing:
   -----------------------------------------------
   not overriding function Get_Diagnostic_Subpacket_Count (Self : in Instance) return Integer;

end Component.Pid_Controller.Implementation.Tester;
