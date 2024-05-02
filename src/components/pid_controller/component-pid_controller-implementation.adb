--------------------------------------------------------------------------------
-- Pid_Controller Component Implementation Body
--------------------------------------------------------------------------------

with Packet_Types;
with Pid_Diagnostic_Subpacket;
with Packed_Natural;

package body Component.Pid_Controller.Implementation is

   ---------------------------------------
   -- Protected Moving_Average Wrapper:
   ---------------------------------------
   protected body Protected_Moving_Average is

      procedure Init (Sample_Storage_Size : in Positive; Sample_Calculation_Size : in Integer) is
      begin
         Ma_Stats.Init (Sample_Storage_Size => Sample_Storage_Size, Sample_Calculation_Size => Sample_Calculation_Size);
      end Init;

      procedure Destroy is
      begin
         Ma_Stats.Destroy;
      end Destroy;

      procedure Calculate_Mean_Variance_Max (New_Sample : in Short_Float; Mean : out Short_Float; Variance : out Short_Float; Max : out Short_Float) is
      begin
         Ma_Stats.Calculate_Mean_Variance_Max (New_Sample, Mean, Variance, Max);
      end Calculate_Mean_Variance_Max;

      procedure Reset is
      begin
         Ma_Stats.Reset;
      end Reset;

      procedure Change_Sample_Calculation_Size (New_Sample_Length : in Positive; Status : out Moving_Average_Variable.Size_Status) is
      begin
         Status := Ma_Stats.Change_Sample_Calculation_Size (New_Sample_Length);
      end Change_Sample_Calculation_Size;

   end Protected_Moving_Average;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- control_Frequency : Short_Float - The frequency in Hz at which the PID controller is being driven. This determines the time step for the PID controller to use in the algorithm.
   -- database_Update_Period : Unsigned_16 - The period in which to update the data products
   -- moving_Average_Max_Samples : Natural - The number of diagnostic samples to keep to perform the mean, variance, and max for the maximum duration
   -- moving_Average_Init_Samples: Integer - The number of samples to initialize the object with. Must be less than the max, and is optional to set to the max with -1
   --
   overriding procedure Init (Self : in out Instance; Control_Frequency : in Short_Float; Database_Update_Period : in Unsigned_16; Moving_Average_Max_Samples : in Natural; Moving_Average_Init_Samples : in Integer := -1) is
   begin
      -- Set the control time step (period):
      pragma Assert (Control_Frequency > 0.0, "The control frequency must be a positive floating point value.");
      Self.Time_Step := 1.0 / Control_Frequency;

      -- Set the database update period:
      Self.Database_Counter.Set_Period_And_Reset_Count (Database_Update_Period);
      -- Set the diagnostic counter and the statistic variables
      Self.Diagnostic_Counter.Set_Count (0);

      -- Setup the moving average object if the max size is set to something other than 0
      if Moving_Average_Max_Samples > 0 then
         Self.Ma_Stats.Init (Sample_Storage_Size => Moving_Average_Max_Samples, Sample_Calculation_Size => Moving_Average_Init_Samples);
         Self.Use_Ma_Stats := True;
      else
         Self.Use_Ma_Stats := False;
      end if;

      -- Set the packet data to a clean packet
      Self.Diagnostic_Packet := (Header => (Time => (0, 0), Id => Self.Packets.Get_Pid_Controller_Diagnostic_Packet_Id, Sequence_Count => 0, Buffer_Length => 0), Buffer => (others => 0));

   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The connector for receiving the control command.
   overriding procedure Control_Input_U_Recv_Sync (Self : in out Instance; Arg : in Control_Input.U) is
   begin
      -- Update the parameters for the control first
      Self.Update_Parameters;

      -- If this is the first iteration of a new control command then we want to reset the internal state of our
      -- accumulated I, D and Error to zero. This prevents an old control history from affecting this new control
      -- run.
      if Arg.First_Iteration then
         Self.Control_Error_Prev := 0.0;
         Self.Control_Out_Prev_I := 0.0;
         Self.Control_Out_Prev_D := 0.0;

         -- Reset our statistics on a new control
         Self.Ma_Stats.Reset;
      end if;

      --
      -- Use the updated parameters to calculate the control angle we desire
      --
      declare
         -- Pull out the timestamp from the input data for ease of use:
         Timestamp : constant Sys_Time.T := Sys_Time.Pack (Arg.Time);
         -- Create locals for the statistics
         Mean_Error : Short_Float := 0.0;
         Variance_Error : Short_Float := 0.0;
         Max_Error : Short_Float := 0.0;
         -- Calculate the current error:
         Pid_Control_Error : constant Short_Float := Arg.Commanded_Value - Arg.Measured_Value;
         -- Proportional control
         Pid_Proportional_Output : constant Short_Float := Self.P_Gain.Value * Pid_Control_Error;
         -- Integral control
         Pid_Integral_Output : Short_Float := Self.Control_Out_Prev_I + Self.I_Gain.Value * Self.Time_Step * Self.Control_Error_Prev;
         -- Derivataive control
         Pid_Derivative_Output : constant Short_Float := Self.Control_Out_Prev_D * (1.0 - (Self.N_Filter.Value * Self.Time_Step)) + (Pid_Control_Error - Self.Control_Error_Prev) * Self.D_Gain.Value * Self.N_Filter.Value;
         -- Total control output
         Pid_Control_Output : Short_Float;
      begin
         -- Limit integral wind up based on our integral limit parameters:
         if Pid_Integral_Output > Self.I_Max_Limit.Value then
            Pid_Integral_Output := Self.I_Max_Limit.Value;
         elsif Pid_Integral_Output < Self.I_Min_Limit.Value then
            Pid_Integral_Output := Self.I_Min_Limit.Value;
         end if;

         -- Set the control output after we limit the integral term
         Pid_Control_Output := Pid_Proportional_Output + Pid_Integral_Output + Pid_Derivative_Output + Arg.Feed_Forward_Value;

         -- Output the control
         Self.Control_Output_U_Send_If_Connected ((Time => Arg.Time, Output_Value => Pid_Control_Output, Error => Pid_Control_Error));

         -- Update the previous values here
         Self.Control_Error_Prev := Pid_Control_Error;
         Self.Control_Out_Prev_D := Pid_Derivative_Output;
         Self.Control_Out_Prev_I := Pid_Integral_Output;

         -- Calculate the new statistics for this cycle if the object was initialized
         if Self.Use_Ma_Stats then
            Self.Ma_Stats.Calculate_Mean_Variance_Max (Pid_Control_Error, Mean_Error, Variance_Error, Max_Error);
         end if;

         -- Update the database if it is time to:
         if Self.Database_Counter.Is_Count_At_Period and then Self.Is_Data_Product_T_Send_Connected then
            -- Send the data products
            Self.Data_Product_T_Send (Self.Data_Products.P_Output (Timestamp, ((Value => Pid_Proportional_Output))));
            Self.Data_Product_T_Send (Self.Data_Products.I_Output (Timestamp, ((Value => Pid_Integral_Output))));
            Self.Data_Product_T_Send (Self.Data_Products.D_Output (Timestamp, ((Value => Pid_Derivative_Output))));
            Self.Data_Product_T_Send (Self.Data_Products.Ff_Output (Timestamp, ((Value => Arg.Feed_Forward_Value))));

            -- Finish filling in the statistcs
            Self.Data_Product_T_Send (Self.Data_Products.Pid_Error (Timestamp, ((Value => Pid_Control_Error))));
            Self.Data_Product_T_Send (Self.Data_Products.Pid_Error_Mean (Timestamp, ((Value => Mean_Error))));
            Self.Data_Product_T_Send (Self.Data_Products.Pid_Error_Variance (Timestamp, ((Value => Variance_Error))));
            Self.Data_Product_T_Send (Self.Data_Products.Pid_Error_Max (Timestamp, ((Value => Max_Error))));
         end if;

         -- Increment the database counter:
         Self.Database_Counter.Increment_Count;

         -- Diagnostic packet - only save if the number of commanded samples is not finished
         declare
            use Pid_Diagnostic_Subpacket;
            -- The current diagnostic count:
            Diag_Count : constant Integer := Self.Diagnostic_Counter.Get_Count;
            -- The current index in the packet we are filling:
            Diagnostic_Packet_Index : Natural := (Self.Diagnostic_Subpacket_Count * Pid_Diagnostic_Subpacket.Max_Serialized_Length) + Packed_Natural.Max_Serialized_Length;

            -- Helper procedure which send out the diagnostic packet and resets variables to allow the filling
            -- of the next packet:
            procedure Send_Packet is
               use Packet_Types;
            begin
               -- Set the number of subpackets in this packet:
               Self.Diagnostic_Packet.Buffer (Self.Diagnostic_Packet.Buffer'First .. Self.Diagnostic_Packet.Buffer'First + Packed_Natural.Max_Serialized_Length - 1) := Packed_Natural.Serialization.To_Byte_Array ((Value => Self.Diagnostic_Subpacket_Count));

               -- Set buffer length of packet based on current diagnostic subpacket count:
               Self.Diagnostic_Packet.Header.Buffer_Length := (Self.Diagnostic_Subpacket_Count * Pid_Diagnostic_Subpacket.Max_Serialized_Length) + Packed_Natural.Max_Serialized_Length;

               -- Send out the packet:
               Self.Packet_T_Send_If_Connected (Self.Diagnostic_Packet);

               -- Increment sequence count:
               Self.Diagnostic_Packet.Header.Sequence_Count := Self.Diagnostic_Packet.Header.Sequence_Count + 1;

               -- Reset diagnostic subpacket counter:
               Self.Diagnostic_Subpacket_Count := 0;

               -- Reset diagnostic packet index:
               Diagnostic_Packet_Index := Packed_Natural.Max_Serialized_Length;
            end Send_Packet;
         begin
            if Diag_Count > 0 then
               -- If there is not enough room in the current packet, then send the packet and start a new one.
               if (Diagnostic_Packet_Index + Pid_Diagnostic_Subpacket.Max_Serialized_Length) > Self.Diagnostic_Packet.Buffer'Length then
                  Send_Packet;
               end if;

               -- Determine if we are starting a new packet
               if Self.Diagnostic_Subpacket_Count = 0 then
                  -- Set timestamp:
                  Self.Diagnostic_Packet.Header.Time := Timestamp;
               end if;

               -- Fill the packet buffer with diagnostic data from this cycle:
               Self.Diagnostic_Packet.Buffer (Diagnostic_Packet_Index .. Diagnostic_Packet_Index + Pid_Diagnostic_Subpacket.Max_Serialized_Length - 1) :=
                  Pid_Diagnostic_Subpacket.Serialization.To_Byte_Array ((Measured_Angle => Arg.Measured_Value, Reference_Angle => Arg.Commanded_Value, Current_Out_Angle => Pid_Control_Output));

               -- Increment the diagnostic subpacket count:
               Self.Diagnostic_Subpacket_Count := Self.Diagnostic_Subpacket_Count + 1;

               -- Decrement the count. If it is 0 here (is. was equal to 1) we will not return here so we need to send what we have of the packet.
               Self.Diagnostic_Counter.Decrement_Count (1);
               if Diag_Count = 1 then
                  Send_Packet;
               end if;
            end if;
         end;
      end;
   end Control_Input_U_Recv_Sync;

   -- The parameter update connector. This does not need to be connected if the parameter for this component will not be used.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T) is
   begin
      -- Process the parameter update, staging or fetching parameters as requested.
      Self.Process_Parameter_Update (Arg);
   end Parameter_Update_T_Modify;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the PID Controller component.
   -- Set the PID controller diagnostic packet's duration to capture samples.
   overriding function Start_Diagnostics (Self : in out Instance; Arg : in Packed_Natural_Duration.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Set the diagnostic subpacket count
      Self.Diagnostic_Counter.Set_Count (Integer (Arg.Duration));
      -- Throw informational event:
      Self.Event_T_Send_If_Connected (Self.Events.Diagnostics_Started (Self.Sys_Time_T_Get, Arg));
      return Success;
   end Start_Diagnostics;

   -- Change the database update period, in units of the resolver acquisition period.
   overriding function Set_Database_Update_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- Set the database update period:
      Self.Database_Counter.Set_Period_And_Reset_Count (Arg.Value);
      -- Throw informational event:
      Self.Event_T_Send_If_Connected (Self.Events.Database_Update_Period_Set (Self.Sys_Time_T_Get, Arg));
      return Success;
   end Set_Database_Update_Period;

   -- Resets and changes the duration that the rolling statistics of the controller are measured, up to a max value set at compile time.
   overriding function Set_Controller_Statistic_Duration (Self : in out Instance; Arg : in Packed_Positive.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      use Moving_Average_Variable;
      Status : Moving_Average_Variable.Size_Status;
   begin
      -- Set the length of the counter based on the command
      Self.Ma_Stats.Change_Sample_Calculation_Size (Arg.Value, Status);

      -- Check status
      case Status is
         when Success =>
            -- Throw informational event:
            Self.Event_T_Send_If_Connected (Self.Events.Set_Controller_Statistics_Duration (Self.Sys_Time_T_Get, Arg));
            return Success;
         when Too_Large =>
            -- Throw error event:
            Self.Event_T_Send_If_Connected (Self.Events.Set_Controller_Statistics_Duration_Too_Large (Self.Sys_Time_T_Get, Arg));
            return Failure;
      end case;
   end Set_Controller_Statistic_Duration;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

   -----------------------------------------------
   -- Parameter handlers:
   -----------------------------------------------
   -- Description:
   --    The set of parameters for the gains in the pid controller
   -- Invalid Parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Throw event:
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Parameter_Received (Self.Sys_Time_T_Get, (Id => Par.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Parameter;

end Component.Pid_Controller.Implementation;
