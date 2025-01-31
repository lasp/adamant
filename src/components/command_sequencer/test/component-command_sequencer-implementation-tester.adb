--------------------------------------------------------------------------------
-- Command_Sequencer Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Command_Sequencer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Sequence_Load_Return_T_Recv_Sync_History.Init (Depth => 999);
      Self.Command_T_Recv_Sync_History.Init (Depth => 999);
      Self.Data_Product_Fetch_T_Service_History.Init (Depth => 999);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 999);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 999);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 999);
      Self.Event_T_Recv_Sync_History.Init (Depth => 999);
      Self.Sys_Time_T_Return_History.Init (Depth => 999);
      -- Event histories:
      Self.Starting_Sequence_History.Init (Depth => 999);
      Self.Finished_Sequence_History.Init (Depth => 999);
      Self.Summary_Packet_Period_Set_History.Init (Depth => 999);
      Self.Details_Packet_Sent_History.Init (Depth => 999);
      Self.Invalid_Engine_Id_History.Init (Depth => 999);
      Self.Invalid_Sequence_Crc_History.Init (Depth => 999);
      Self.Invalid_Sequence_Length_History.Init (Depth => 999);
      Self.Invalid_Sequence_Id_History.Init (Depth => 999);
      Self.Load_To_Uninitialized_Engine_History.Init (Depth => 999);
      Self.Load_To_Invalid_Engine_Id_History.Init (Depth => 999);
      Self.No_Engine_Available_History.Init (Depth => 999);
      Self.No_Engine_Available_For_Load_History.Init (Depth => 999);
      Self.Engine_In_Use_History.Init (Depth => 999);
      Self.Sequence_Load_Error_History.Init (Depth => 999);
      Self.Killed_All_Engines_History.Init (Depth => 999);
      Self.Killed_Engine_History.Init (Depth => 999);
      Self.Dropped_Command_History.Init (Depth => 999);
      Self.Dropped_Command_Response_History.Init (Depth => 999);
      Self.Dropped_Tick_History.Init (Depth => 999);
      Self.Dropped_Sequence_Load_History.Init (Depth => 999);
      Self.Invalid_Command_Received_History.Init (Depth => 999);
      Self.Unexpected_Command_Response_History.Init (Depth => 999);
      Self.Unexpected_Register_Source_History.Init (Depth => 999);
      Self.Sequence_Execution_Error_History.Init (Depth => 999);
      Self.Sequence_Timeout_Error_History.Init (Depth => 999);
      Self.Unexpected_Command_Response_Id_History.Init (Depth => 999);
      Self.Sequence_Command_Failure_History.Init (Depth => 999);
      Self.Engine_Id_Out_Of_Range_Error_History.Init (Depth => 999);
      Self.Engine_Unavailable_For_Load_History.Init (Depth => 999);
      Self.Data_Product_Id_Out_Of_Range_Error_History.Init (Depth => 999);
      Self.Data_Product_Extraction_Error_History.Init (Depth => 999);
      Self.Execute_Recursion_Limit_Exceeded_History.Init (Depth => 999);
      Self.Invalid_Engine_Kill_Range_History.Init (Depth => 999);
      Self.Engines_Killed_History.Init (Depth => 999);
      Self.Print_History.Init (Depth => 999);
      Self.Loaded_Engine_Arguments_History.Init (Depth => 999);
      Self.Unable_To_Load_Engine_Arguments_History.Init (Depth => 999);
      Self.Unhandled_Telemetry_Type_History.Init (Depth => 999);
      -- Data product histories:
      Self.Summary_Packet_Period_History.Init (Depth => 999);
      -- Packet histories:
      Self.Summary_Packet_History.Init (Depth => 999);
      Self.Details_Packet_History.Init (Depth => 999);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Sequence_Load_Return_T_Recv_Sync_History.Destroy;
      Self.Command_T_Recv_Sync_History.Destroy;
      Self.Data_Product_Fetch_T_Service_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Starting_Sequence_History.Destroy;
      Self.Finished_Sequence_History.Destroy;
      Self.Summary_Packet_Period_Set_History.Destroy;
      Self.Details_Packet_Sent_History.Destroy;
      Self.Invalid_Engine_Id_History.Destroy;
      Self.Invalid_Sequence_Crc_History.Destroy;
      Self.Invalid_Sequence_Length_History.Destroy;
      Self.Invalid_Sequence_Id_History.Destroy;
      Self.Load_To_Uninitialized_Engine_History.Destroy;
      Self.Load_To_Invalid_Engine_Id_History.Destroy;
      Self.No_Engine_Available_History.Destroy;
      Self.No_Engine_Available_For_Load_History.Destroy;
      Self.Engine_In_Use_History.Destroy;
      Self.Sequence_Load_Error_History.Destroy;
      Self.Killed_All_Engines_History.Destroy;
      Self.Killed_Engine_History.Destroy;
      Self.Dropped_Command_History.Destroy;
      Self.Dropped_Command_Response_History.Destroy;
      Self.Dropped_Tick_History.Destroy;
      Self.Dropped_Sequence_Load_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Unexpected_Command_Response_History.Destroy;
      Self.Unexpected_Register_Source_History.Destroy;
      Self.Sequence_Execution_Error_History.Destroy;
      Self.Sequence_Timeout_Error_History.Destroy;
      Self.Unexpected_Command_Response_Id_History.Destroy;
      Self.Sequence_Command_Failure_History.Destroy;
      Self.Engine_Id_Out_Of_Range_Error_History.Destroy;
      Self.Engine_Unavailable_For_Load_History.Destroy;
      Self.Data_Product_Id_Out_Of_Range_Error_History.Destroy;
      Self.Data_Product_Extraction_Error_History.Destroy;
      Self.Execute_Recursion_Limit_Exceeded_History.Destroy;
      Self.Invalid_Engine_Kill_Range_History.Destroy;
      Self.Engines_Killed_History.Destroy;
      Self.Print_History.Destroy;
      Self.Loaded_Engine_Arguments_History.Destroy;
      Self.Unable_To_Load_Engine_Arguments_History.Destroy;
      Self.Unhandled_Telemetry_Type_History.Destroy;
      -- Data product histories:
      Self.Summary_Packet_Period_History.Destroy;
      -- Packet histories:
      Self.Summary_Packet_History.Destroy;
      Self.Details_Packet_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Sequence_Load_Return_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Sequence_Load_Return_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_Fetch_T_Request (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_Fetch_T_Service_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Async_Access);
      Self.Attach_Command_Response_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_Response_T_Recv_Async_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Sequence_Load_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Sequence_Load_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send the return status from a sequence load operation.
   overriding procedure Sequence_Load_Return_T_Recv_Sync (Self : in out Instance; Arg : in Sequence_Load_Return.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Load_Return_T_Recv_Sync_History.Push (Arg);
   end Sequence_Load_Return_T_Recv_Sync;

   -- The command send connector. Commands originating from sequences are sent out of this connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_T_Recv_Sync_History.Push (Arg);
   end Command_T_Recv_Sync;

   -- Fetch a data product item from the database. This is used to check telemetry during conditionals in a sequence.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T is
      To_Return : Data_Product_Return.T := (
         The_Status => Self.Data_Product_Fetch_Return_Status,
         The_Data_Product => (
            Header => (
               Time => Self.System_Time,
               Id => Arg.Id,
               Buffer_Length => 0
            ),
            Buffer => [others => 0]
         )
      );
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Fetch_T_Service_History.Push (Arg);

      -- Add data into the data product buffer to return:
      case Arg.Id is
         when 1 =>
            To_Return.The_Data_Product.Header.Buffer_Length := 2;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Packed_U16.Serialization.To_Byte_Array (Self.Component_A_Data_Product_1);
         when 2 =>
            To_Return.The_Data_Product.Header.Buffer_Length := 4;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Packed_F32.Serialization.To_Byte_Array (Self.Component_A_Data_Product_2);
         when 3 =>
            To_Return.The_Data_Product.Header.Buffer_Length := Sys_Time.Size_In_Bytes;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Self.Component_A_Data_Product_3);
         when 4 =>
            To_Return.The_Data_Product.Header.Buffer_Length := 2;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Packed_I16.Serialization.To_Byte_Array (Self.Component_A_Data_Product_4);
         when 5 =>
            To_Return.The_Data_Product.Header.Buffer_Length := 2;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Packed_U16.Serialization.To_Byte_Array (Self.Component_B_Data_Product_1);
         when 6 =>
            To_Return.The_Data_Product.Header.Buffer_Length := 4;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Packed_F32.Serialization.To_Byte_Array (Self.Component_B_Data_Product_2);
         when 7 =>
            To_Return.The_Data_Product.Header.Buffer_Length := Sys_Time.Size_In_Bytes;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Sys_Time.Serialization.To_Byte_Array (Self.Component_B_Data_Product_3);
         when 8 =>
            To_Return.The_Data_Product.Header.Buffer_Length := 2;
            To_Return.The_Data_Product.Buffer (0 .. To_Return.The_Data_Product.Header.Buffer_Length - 1) := Packed_I16.Serialization.To_Byte_Array (Self.Component_B_Data_Product_4);
         when others => null;
      end case;

      -- Override the buffer length if necessary to simulate error.
      if Self.Data_Product_Fetch_Length_Override then
         To_Return.The_Data_Product.Header.Buffer_Length := Self.Data_Product_Fetch_Length_Override_Value;
      end if;

      -- Override the timestamp if necessary to simulate error.
      if Self.Data_Product_Fetch_Time_Override then
         To_Return.The_Data_Product.Header.Time := Self.Data_Product_Fetch_Time_Override_Value;
      end if;

      return To_Return;
   end Data_Product_Fetch_T_Service;

   -- This connector is used to register the components commands with the command router component.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Packets are sent out of this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      if not Self.Expect_Tick_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Tick_T_Send was called!");
      else
         Self.Tick_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Tick_T_Send_Dropped := False;
      end if;
   end Tick_T_Send_Dropped;

   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is
      Ignore : Command_Response.T renames Arg;
   begin
      if not Self.Expect_Command_Response_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_Response_T_Send was called!");
      else
         Self.Command_Response_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_Response_T_Send_Dropped := False;
      end if;
   end Command_Response_T_Send_Dropped;

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      if not Self.Expect_Command_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_T_Send was called!");
      else
         Self.Command_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_T_Send_Dropped := False;
      end if;
   end Command_T_Send_Dropped;

   -- This procedure is called when a Sequence_Load_T_Send message is dropped due to a full queue.
   overriding procedure Sequence_Load_T_Send_Dropped (Self : in out Instance; Arg : in Sequence_Load.T) is
      Ignore : Sequence_Load.T renames Arg;
   begin
      if not Self.Expect_Sequence_Load_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Sequence_Load_T_Send was called!");
      else
         Self.Sequence_Load_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Sequence_Load_T_Send_Dropped := False;
      end if;
   end Sequence_Load_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Starting a sequence with the following information.
   overriding procedure Starting_Sequence (Self : in out Instance; Arg : in Sequence_Load_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Starting_Sequence_History.Push (Arg);
   end Starting_Sequence;

   -- The sequence engine as finished its execution of the parent sequence.
   overriding procedure Finished_Sequence (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Finished_Sequence_History.Push (Arg);
   end Finished_Sequence;

   -- A command was received to change the packet period of the summary packet.
   overriding procedure Summary_Packet_Period_Set (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Summary_Packet_Period_Set_History.Push (Arg);
   end Summary_Packet_Period_Set;

   -- The sequencer engine details packet was sent for the request engine.
   overriding procedure Details_Packet_Sent (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Details_Packet_Sent_History.Push (Arg);
   end Details_Packet_Sent;

   -- The operation could not be completed because the engine ID provided is invalid.
   overriding procedure Invalid_Engine_Id (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Engine_Id_History.Push (Arg);
   end Invalid_Engine_Id;

   -- The sequence could not be run due to a bad CRC.
   overriding procedure Invalid_Sequence_Crc (Self : in out Instance; Arg : in Sequence_Crc_Error.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Sequence_Crc_History.Push (Arg);
   end Invalid_Sequence_Crc;

   -- The sequence could not be run due to a bad length.
   overriding procedure Invalid_Sequence_Length (Self : in out Instance; Arg : in Sequence_Length_Error.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Sequence_Length_History.Push (Arg);
   end Invalid_Sequence_Length;

   -- The sequence could not be run due to unexpected ID.
   overriding procedure Invalid_Sequence_Id (Self : in out Instance; Arg : in Sequence_Id_Error.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Sequence_Id_History.Push (Arg);
   end Invalid_Sequence_Id;

   -- The sequence could not be run because the engine has not yet been initialized.
   overriding procedure Load_To_Uninitialized_Engine (Self : in out Instance; Arg : in Sequence_Load.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Load_To_Uninitialized_Engine_History.Push (Arg);
   end Load_To_Uninitialized_Engine;

   -- The sequence could not be run due to unexpected engine id.
   overriding procedure Load_To_Invalid_Engine_Id (Self : in out Instance; Arg : in Sequence_Load.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Load_To_Invalid_Engine_Id_History.Push (Arg);
   end Load_To_Invalid_Engine_Id;

   -- No engine is available to take a sequence load.
   overriding procedure No_Engine_Available (Self : in out Instance; Arg : in Sequence_Load.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.No_Engine_Available_History.Push (Arg);
   end No_Engine_Available;

   -- No engine is available to take a sequence load from another currently running sequence.
   overriding procedure No_Engine_Available_For_Load (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.No_Engine_Available_For_Load_History.Push (Arg);
   end No_Engine_Available_For_Load;

   -- The sequence could not be run because the current engine is in use.
   overriding procedure Engine_In_Use (Self : in out Instance; Arg : in Sequence_In_Use_Error.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Engine_In_Use_History.Push (Arg);
   end Engine_In_Use;

   -- A sequence could not be loaded due to an internal sequence runner error.
   overriding procedure Sequence_Load_Error (Self : in out Instance; Arg : in Sequence_Load_Error_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Load_Error_History.Push (Arg);
   end Sequence_Load_Error;

   -- A command was executed to kill all running sequences.
   overriding procedure Killed_All_Engines (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Killed_All_Engines_History.Push (Arg);
   end Killed_All_Engines;

   -- A command was executed to kill a sequence running in a specific engine
   overriding procedure Killed_Engine (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Killed_Engine_History.Push (Arg);
   end Killed_Engine;

   -- A command was dropped due to a full queue.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Command_History.Push (Arg);
   end Dropped_Command;

   -- A command response was dropped due to a full queue.
   overriding procedure Dropped_Command_Response (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Command_Response_History.Push (Arg);
   end Dropped_Command_Response;

   -- A tick was dropped due to a full queue.
   overriding procedure Dropped_Tick (Self : in out Instance; Arg : in Tick.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Tick_History.Push (Arg);
   end Dropped_Tick;

   -- A sequence load was dropped due to a full queue.
   overriding procedure Dropped_Sequence_Load (Self : in out Instance; Arg : in Sequence_Load.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Sequence_Load_History.Push (Arg);
   end Dropped_Sequence_Load;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command response was found with an unrecognized source ID.
   overriding procedure Unexpected_Command_Response (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Command_Response_History.Push (Arg);
   end Unexpected_Command_Response;

   -- An extra source registration was received, but all engines have a source ID already.
   overriding procedure Unexpected_Register_Source (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Register_Source_History.Push (Arg);
   end Unexpected_Register_Source;

   -- An error occurred while executing a sequence.
   overriding procedure Sequence_Execution_Error (Self : in out Instance; Arg : in Engine_Error_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Execution_Error_History.Push (Arg);
   end Sequence_Execution_Error;

   -- A sequence timed out waiting on a command response of subsequence load.
   overriding procedure Sequence_Timeout_Error (Self : in out Instance; Arg : in Engine_Error_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Timeout_Error_History.Push (Arg);
   end Sequence_Timeout_Error;

   -- A command response was received with an unexpected command ID.
   overriding procedure Unexpected_Command_Response_Id (Self : in out Instance; Arg : in Unexpected_Command_Response_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Command_Response_Id_History.Push (Arg);
   end Unexpected_Command_Response_Id;

   -- A command from a sequence failed to execute successfully.
   overriding procedure Sequence_Command_Failure (Self : in out Instance; Arg : in Command_Fail_Error_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sequence_Command_Failure_History.Push (Arg);
   end Sequence_Command_Failure;

   -- During a sequence load from an engine, the destination engine ID was found to be out of range.
   overriding procedure Engine_Id_Out_Of_Range_Error (Self : in out Instance; Arg : in Engine_Id_Out_Of_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Engine_Id_Out_Of_Range_Error_History.Push (Arg);
   end Engine_Id_Out_Of_Range_Error;

   -- During a sequence load from an engine, the destination engine ID was found to be unavailable.
   overriding procedure Engine_Unavailable_For_Load (Self : in out Instance; Arg : in Engine_Id_Out_Of_Range.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Engine_Unavailable_For_Load_History.Push (Arg);
   end Engine_Unavailable_For_Load;

   -- A data product was fetched with an ID that was not recognized.
   overriding procedure Data_Product_Id_Out_Of_Range_Error (Self : in out Instance; Arg : in Engine_Error_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Id_Out_Of_Range_Error_History.Push (Arg);
   end Data_Product_Id_Out_Of_Range_Error;

   -- Data could not be parsed out of the fetched data product.
   overriding procedure Data_Product_Extraction_Error (Self : in out Instance; Arg : in Engine_Error_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Extraction_Error_History.Push (Arg);
   end Data_Product_Extraction_Error;

   -- The recursion limit was exceeded in a call to execute. This likely means a malformed sequence was executed.
   overriding procedure Execute_Recursion_Limit_Exceeded (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Execute_Recursion_Limit_Exceeded_History.Push (Arg);
   end Execute_Recursion_Limit_Exceeded;

   -- The engine kill range is invalid and cannot be executed.
   overriding procedure Invalid_Engine_Kill_Range (Self : in out Instance; Arg : in Packed_Engine_Kill_Params.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Engine_Kill_Range_History.Push (Arg);
   end Invalid_Engine_Kill_Range;

   -- The provided engines were killed by another engine.
   overriding procedure Engines_Killed (Self : in out Instance; Arg : in Packed_Engine_Kill_Params.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Engines_Killed_History.Push (Arg);
   end Engines_Killed;

   -- A sequence is sending the following print statement.
   overriding procedure Print (Self : in out Instance; Arg : in Seq_Print_Event_Record.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Print_History.Push (Arg);
   end Print;

   -- Arguments were successfully loaded into an engine by command.
   overriding procedure Loaded_Engine_Arguments (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Loaded_Engine_Arguments_History.Push (Arg);
   end Loaded_Engine_Arguments;

   -- Arguments were not successfully loaded into an engine because the engine is currently busy.
   overriding procedure Unable_To_Load_Engine_Arguments (Self : in out Instance; Arg : in Unexpected_Engine_State.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unable_To_Load_Engine_Arguments_History.Push (Arg);
   end Unable_To_Load_Engine_Arguments;

   -- The telemetry type specified in the sequence is not handled by this implementation.
   overriding procedure Unhandled_Telemetry_Type (Self : in out Instance; Arg : in Engine_Error_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unhandled_Telemetry_Type_History.Push (Arg);
   end Unhandled_Telemetry_Type;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Command Sequencer component.
   -- The current packet period for the summary packet.
   overriding procedure Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Summary_Packet_Period_History.Push (Arg);
   end Summary_Packet_Period;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Command Sequencer.
   -- This packet contains a brief summary of what sequences are running in the sequencer's engines. Its type is determined dynamically based on the number of engines declared in the component.
   overriding procedure Summary_Packet (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Summary_Packet_History.Push (Arg);
   end Summary_Packet;

   -- This packet contains all useful information about the current state a single engine within the sequence. Its type is determined dynamically based on the stack size allocated to the engines.
   overriding procedure Details_Packet (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Details_Packet_History.Push (Arg);
   end Details_Packet;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

   -----------------------------------------------
   -- Custom white-box subprograms:
   -----------------------------------------------
   not overriding function Get_Engine_State (Self : in out Instance; Engine_Id : in Seq_Types.Sequence_Engine_Id) return Seq_Enums.Seq_Engine_State.E is
   begin
      return Self.Component_Instance.Seq_Engines.all (Engine_Id).Get_Engine_State;
   end Get_Engine_State;

end Component.Command_Sequencer.Implementation.Tester;
