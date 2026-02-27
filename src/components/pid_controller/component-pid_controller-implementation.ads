--------------------------------------------------------------------------------
-- Pid_Controller Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Control_Input;
with Parameter;
with Command;
with Protected_Variables;
with Moving_Average;

-- This component is the PID controller
package Component.Pid_Controller.Implementation is

   -- The component class instance record:
   type Instance is new Pid_Controller.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Control_Frequency : Short_Float - The frequency in Hz at which the PID controller is being driven. This determines the time step for the PID controller to use in the algorithm.
   -- Database_Update_Period : Unsigned_16 - The period in which to update the data products
   -- Moving_Average_Max_Samples : Natural - The number of diagnostic samples to keep to perform the mean, variance, and max
   -- Moving_Average_Init_Samples : Integer - The number of samples to initialize the object with. Must be less than the max, and is optional to set to the max with -1
   --
   overriding procedure Init (Self : in out Instance; Control_Frequency : in Short_Float; Database_Update_Period : in Unsigned_16; Moving_Average_Max_Samples : in Natural; Moving_Average_Init_Samples : in Integer := -1);
private

   -- Protected objects for protecting data that can be modified by synchronous command.
   package Protected_Signed_16_Counter is new Protected_Variables.Generic_Protected_Counter_Decrement (Integer);
   package Protected_Unsigned_16_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (Interfaces.Unsigned_16);
   package Moving_Average_Variable is new Moving_Average (Short_Float);

   protected type Protected_Moving_Average is
      -- Procedures requiring full mutual exclusion:
      procedure Init (Sample_Storage_Size : in Positive; Sample_Calculation_Size : in Integer);
      procedure Destroy;
      procedure Calculate_Mean_Variance_Max (New_Sample : in Short_Float; Mean : out Short_Float; Variance : out Short_Float; Max : out Short_Float);
      procedure Reset;
      procedure Change_Sample_Calculation_Size (New_Sample_Length : in Positive; Status : out Moving_Average_Variable.Size_Status);
   private
      -- Protected moving average data structure:
      Ma_Stats : Moving_Average_Variable.Instance;
   end Protected_Moving_Average;

   -- The component class instance record:
   type Instance is new Pid_Controller.Base_Instance with record
      -- Control timestep, determined by frequency
      Time_Step : Short_Float := 1.0;

      -- Database control variables
      Database_Counter : Protected_Unsigned_16_Counter.Counter; -- How many times have we processed resolver data?

      -- Previous integral and derivative values
      Control_Error_Prev : Short_Float := 0.0;
      Control_Out_Prev_I : Short_Float := 0.0;
      Control_Out_Prev_D : Short_Float := 0.0;

      -- Diagnostic packet variables
      Diagnostic_Counter : Protected_Signed_16_Counter.Counter;
      Diagnostic_Packet : Packet.T;
      Diagnostic_Subpacket_Count : Natural := 0;

      -- Statistics array and values:
      Ma_Stats : Protected_Moving_Average;
      Use_Ma_Stats : Boolean := False;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The connector for receiving the control command.
   overriding procedure Control_Input_U_Recv_Sync (Self : in out Instance; Arg : in Control_Input.U);
   -- The parameter update connector. This does not need to be connected if the parameter for this component will not be used.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Control_Output_U_Send message is dropped due to a full queue.
   overriding procedure Control_Output_U_Send_Dropped (Self : in out Instance; Arg : in Control_Output.U) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the PID Controller component.
   -- Set the PID controller diagnostic packet's duration to capture samples.
   overriding function Start_Diagnostics (Self : in out Instance; Arg : in Packed_Natural_Duration.T) return Command_Execution_Status.E;
   -- Change the database update period, in units of the resolver acquisition period.
   overriding function Set_Database_Update_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;
   -- Resets and changes the duration that the rolling statistics of the controller are measured, up to a max value set at compile time.
   overriding function Set_Controller_Statistic_Duration (Self : in out Instance; Arg : in Packed_Positive.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

   -----------------------------------------------
   -- Parameter primitives:
   -----------------------------------------------
   -- Description:
   --    The set of parameters for the gains in the pid controller

   -- Invalid parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);
   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be implemented if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   overriding procedure Update_Parameters_Action (Self : in out Instance) is null;
   -- This function is called when the parameter operation type is "Validate". The default implementation of this
   -- subprogram in the implementation package is a function that returns "Valid". However, this function can, and should be
   -- overridden if something special needs to happen to further validate a parameter. Examples of this might be validation of
   -- certain parameters beyond individual type ranges, or performing other special functionality that only needs to be
   -- performed after parameters have been validated. Note that range checking is performed during staging, and does not need
   -- to be implemented here.
   overriding function Validate_Parameters (
      Self : in out Instance;
      P_Gain : in Packed_F32.U;
      I_Gain : in Packed_F32.U;
      D_Gain : in Packed_F32.U;
      N_Filter : in Packed_F32.U;
      I_Min_Limit : in Packed_F32.U;
      I_Max_Limit : in Packed_F32.U
   ) return Parameter_Validation_Status.E is (Parameter_Validation_Status.Valid);

end Component.Pid_Controller.Implementation;
