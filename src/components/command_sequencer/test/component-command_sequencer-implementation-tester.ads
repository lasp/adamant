--------------------------------------------------------------------------------
-- Command_Sequencer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Command_Sequencer_Reciprocal;
with Sys_Time;
with Printable_History;
with Sequence_Load_Return.Representation;
with Command.Representation;
with Data_Product_Return.Representation;
with Data_Product_Fetch.Representation;
with Command_Response.Representation;
with Packet.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Sequence_Load_Info.Representation;
with Packed_Sequence_Engine_Id.Representation;
with Packed_U16.Representation;
with Sequence_Crc_Error.Representation;
with Sequence_Length_Error.Representation;
with Sequence_Id_Error.Representation;
with Sequence_Load.Representation;
with Sequence_In_Use_Error.Representation;
with Sequence_Load_Error_Info.Representation;
with Command_Header.Representation;
with Tick.Representation;
with Invalid_Command_Info.Representation;
with Engine_Error_Type.Representation;
with Unexpected_Command_Response_Info.Representation;
with Command_Fail_Error_Type.Representation;
with Engine_Id_Out_Of_Range.Representation;
with Packed_Engine_Kill_Params.Representation;
with Data_Product_Enums; use Data_Product_Enums.Fetch_Status;
with Packed_F32;
with Packed_I16;
with Data_Product;
with Seq_Enums;
with Unexpected_Engine_State.Representation;
with Seq_Print_Event_Record.Representation;

-- The Command Sequencer component executes command sequences with a configurable number of engines. The sequence engines execute sequences in the LASP Awesome Sequence Engine Language (LASEL) compiled by the LASP SEQ tool. Documentation on LASEL is included in this component's doc/ directory.
--
-- This component runs a configurable number of sequence engines using a single Adamant task. The task runs each engine in priority order, where lower numbered engines take precedence over higher numbered engines. Each engine contains a configurable-sized stack that allows sequences to call subsequences. This component adheres to the property that commands are only executed after previous commands have completed (i.e. a command response has been received). In this way the sequences are largely event driven, waiting on the execution of previous commands to finish prior to executing subsequent ones. A periodic tick is supplied to the component to provide timing control for sequences that need to execute relative or absolute waits, or check until a telemetry condition has been met before proceeding.
--
-- The sequence engine and LASEL interpreter is located in the seq/ directory.
package Component.Command_Sequencer.Implementation.Tester is

   use Component.Command_Sequencer_Reciprocal;
   -- Invoker connector history packages:
   package Sequence_Load_Return_T_Recv_Sync_History_Package is new Printable_History (Sequence_Load_Return.T, Sequence_Load_Return.Representation.Image);
   package Command_T_Recv_Sync_History_Package is new Printable_History (Command.T, Command.Representation.Image);
   package Data_Product_Fetch_T_Service_History_Package is new Printable_History (Data_Product_Fetch.T, Data_Product_Fetch.Representation.Image);
   package Data_Product_Fetch_T_Service_Return_History_Package is new Printable_History (Data_Product_Return.T, Data_Product_Return.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Starting_Sequence_History_Package is new Printable_History (Sequence_Load_Info.T, Sequence_Load_Info.Representation.Image);
   package Finished_Sequence_History_Package is new Printable_History (Packed_Sequence_Engine_Id.T, Packed_Sequence_Engine_Id.Representation.Image);
   package Summary_Packet_Period_Set_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Details_Packet_Sent_History_Package is new Printable_History (Packed_Sequence_Engine_Id.T, Packed_Sequence_Engine_Id.Representation.Image);
   package Invalid_Engine_Id_History_Package is new Printable_History (Packed_Sequence_Engine_Id.T, Packed_Sequence_Engine_Id.Representation.Image);
   package Invalid_Sequence_Crc_History_Package is new Printable_History (Sequence_Crc_Error.T, Sequence_Crc_Error.Representation.Image);
   package Invalid_Sequence_Length_History_Package is new Printable_History (Sequence_Length_Error.T, Sequence_Length_Error.Representation.Image);
   package Invalid_Sequence_Id_History_Package is new Printable_History (Sequence_Id_Error.T, Sequence_Id_Error.Representation.Image);
   package Load_To_Uninitialized_Engine_History_Package is new Printable_History (Sequence_Load.T, Sequence_Load.Representation.Image);
   package Load_To_Invalid_Engine_Id_History_Package is new Printable_History (Sequence_Load.T, Sequence_Load.Representation.Image);
   package No_Engine_Available_History_Package is new Printable_History (Sequence_Load.T, Sequence_Load.Representation.Image);
   package No_Engine_Available_For_Load_History_Package is new Printable_History (Packed_Sequence_Engine_Id.T, Packed_Sequence_Engine_Id.Representation.Image);
   package Engine_In_Use_History_Package is new Printable_History (Sequence_In_Use_Error.T, Sequence_In_Use_Error.Representation.Image);
   package Sequence_Load_Error_History_Package is new Printable_History (Sequence_Load_Error_Info.T, Sequence_Load_Error_Info.Representation.Image);
   package Killed_All_Engines_History_Package is new Printable_History (Natural, Natural'Image);
   package Killed_Engine_History_Package is new Printable_History (Packed_Sequence_Engine_Id.T, Packed_Sequence_Engine_Id.Representation.Image);
   package Dropped_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Dropped_Command_Response_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Dropped_Tick_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);
   package Dropped_Sequence_Load_History_Package is new Printable_History (Sequence_Load.T, Sequence_Load.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Unexpected_Command_Response_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Unexpected_Register_Source_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Sequence_Execution_Error_History_Package is new Printable_History (Engine_Error_Type.T, Engine_Error_Type.Representation.Image);
   package Sequence_Timeout_Error_History_Package is new Printable_History (Engine_Error_Type.T, Engine_Error_Type.Representation.Image);
   package Unexpected_Command_Response_Id_History_Package is new Printable_History (Unexpected_Command_Response_Info.T, Unexpected_Command_Response_Info.Representation.Image);
   package Sequence_Command_Failure_History_Package is new Printable_History (Command_Fail_Error_Type.T, Command_Fail_Error_Type.Representation.Image);
   package Engine_Id_Out_Of_Range_Error_History_Package is new Printable_History (Engine_Id_Out_Of_Range.T, Engine_Id_Out_Of_Range.Representation.Image);
   package Engine_Unavailable_For_Load_History_Package is new Printable_History (Engine_Id_Out_Of_Range.T, Engine_Id_Out_Of_Range.Representation.Image);
   package Data_Product_Id_Out_Of_Range_Error_History_Package is new Printable_History (Engine_Error_Type.T, Engine_Error_Type.Representation.Image);
   package Data_Product_Extraction_Error_History_Package is new Printable_History (Engine_Error_Type.T, Engine_Error_Type.Representation.Image);
   package Execute_Recursion_Limit_Exceeded_History_Package is new Printable_History (Packed_Sequence_Engine_Id.T, Packed_Sequence_Engine_Id.Representation.Image);
   package Invalid_Engine_Kill_Range_History_Package is new Printable_History (Packed_Engine_Kill_Params.T, Packed_Engine_Kill_Params.Representation.Image);
   package Engines_Killed_History_Package is new Printable_History (Packed_Engine_Kill_Params.T, Packed_Engine_Kill_Params.Representation.Image);
   package Print_History_Package is new Printable_History (Seq_Print_Event_Record.T, Seq_Print_Event_Record.Representation.Image);
   package Loaded_Engine_Arguments_History_Package is new Printable_History (Packed_Sequence_Engine_Id.T, Packed_Sequence_Engine_Id.Representation.Image);
   package Unable_To_Load_Engine_Arguments_History_Package is new Printable_History (Unexpected_Engine_State.T, Unexpected_Engine_State.Representation.Image);
   package Unhandled_Telemetry_Type_History_Package is new Printable_History (Engine_Error_Type.T, Engine_Error_Type.Representation.Image);

   -- Data product history packages:
   package Summary_Packet_Period_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Summary_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Details_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Command_Sequencer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Command_Sequencer.Implementation.Instance;
      -- Connector histories:
      Sequence_Load_Return_T_Recv_Sync_History : Sequence_Load_Return_T_Recv_Sync_History_Package.Instance;
      Command_T_Recv_Sync_History : Command_T_Recv_Sync_History_Package.Instance;
      Data_Product_Fetch_T_Service_History : Data_Product_Fetch_T_Service_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Starting_Sequence_History : Starting_Sequence_History_Package.Instance;
      Finished_Sequence_History : Finished_Sequence_History_Package.Instance;
      Summary_Packet_Period_Set_History : Summary_Packet_Period_Set_History_Package.Instance;
      Details_Packet_Sent_History : Details_Packet_Sent_History_Package.Instance;
      Invalid_Engine_Id_History : Invalid_Engine_Id_History_Package.Instance;
      Invalid_Sequence_Crc_History : Invalid_Sequence_Crc_History_Package.Instance;
      Invalid_Sequence_Length_History : Invalid_Sequence_Length_History_Package.Instance;
      Invalid_Sequence_Id_History : Invalid_Sequence_Id_History_Package.Instance;
      Load_To_Uninitialized_Engine_History : Load_To_Uninitialized_Engine_History_Package.Instance;
      Load_To_Invalid_Engine_Id_History : Load_To_Invalid_Engine_Id_History_Package.Instance;
      No_Engine_Available_History : No_Engine_Available_History_Package.Instance;
      No_Engine_Available_For_Load_History : No_Engine_Available_For_Load_History_Package.Instance;
      Engine_In_Use_History : Engine_In_Use_History_Package.Instance;
      Sequence_Load_Error_History : Sequence_Load_Error_History_Package.Instance;
      Killed_All_Engines_History : Killed_All_Engines_History_Package.Instance;
      Killed_Engine_History : Killed_Engine_History_Package.Instance;
      Dropped_Command_History : Dropped_Command_History_Package.Instance;
      Dropped_Command_Response_History : Dropped_Command_Response_History_Package.Instance;
      Dropped_Tick_History : Dropped_Tick_History_Package.Instance;
      Dropped_Sequence_Load_History : Dropped_Sequence_Load_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Unexpected_Command_Response_History : Unexpected_Command_Response_History_Package.Instance;
      Unexpected_Register_Source_History : Unexpected_Register_Source_History_Package.Instance;
      Sequence_Execution_Error_History : Sequence_Execution_Error_History_Package.Instance;
      Sequence_Timeout_Error_History : Sequence_Timeout_Error_History_Package.Instance;
      Unexpected_Command_Response_Id_History : Unexpected_Command_Response_Id_History_Package.Instance;
      Sequence_Command_Failure_History : Sequence_Command_Failure_History_Package.Instance;
      Engine_Id_Out_Of_Range_Error_History : Engine_Id_Out_Of_Range_Error_History_Package.Instance;
      Engine_Unavailable_For_Load_History : Engine_Unavailable_For_Load_History_Package.Instance;
      Data_Product_Id_Out_Of_Range_Error_History : Data_Product_Id_Out_Of_Range_Error_History_Package.Instance;
      Data_Product_Extraction_Error_History : Data_Product_Extraction_Error_History_Package.Instance;
      Execute_Recursion_Limit_Exceeded_History : Execute_Recursion_Limit_Exceeded_History_Package.Instance;
      Invalid_Engine_Kill_Range_History : Invalid_Engine_Kill_Range_History_Package.Instance;
      Engines_Killed_History : Engines_Killed_History_Package.Instance;
      Print_History : Print_History_Package.Instance;
      Loaded_Engine_Arguments_History : Loaded_Engine_Arguments_History_Package.Instance;
      Unable_To_Load_Engine_Arguments_History : Unable_To_Load_Engine_Arguments_History_Package.Instance;
      Unhandled_Telemetry_Type_History : Unhandled_Telemetry_Type_History_Package.Instance;
      -- Data product histories:
      Summary_Packet_Period_History : Summary_Packet_Period_History_Package.Instance;
      -- Packet histories:
      Summary_Packet_History : Summary_Packet_History_Package.Instance;
      Details_Packet_History : Details_Packet_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Tick_T_Send_Dropped : Boolean := False;
      Tick_T_Send_Dropped_Count : Natural := 0;
      Expect_Command_Response_T_Send_Dropped : Boolean := False;
      Command_Response_T_Send_Dropped_Count : Natural := 0;
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Sequence_Load_T_Send_Dropped : Boolean := False;
      Sequence_Load_T_Send_Dropped_Count : Natural := 0;
      -- Custom stuff for simulating data product fetches:
      Data_Product_Fetch_Return_Status : Data_Product_Enums.Fetch_Status.E := Success;
      Data_Product_Fetch_Length_Override : Boolean := False;
      Data_Product_Fetch_Length_Override_Value : Data_Product_Types.Data_Product_Buffer_Length_Type := 0;
      Data_Product_Fetch_Time_Override : Boolean := False;
      Data_Product_Fetch_Time_Override_Value : Sys_Time.T := (0, 0);
      Component_A_Data_Product_1 : Packed_U16.T := (Value => 0);
      Component_A_Data_Product_2 : Packed_F32.T := (Value => 0.0);
      Component_A_Data_Product_3 : Sys_Time.T := (0, 0);
      Component_A_Data_Product_4 : Packed_I16.T := (Value => 0);
      Component_B_Data_Product_1 : Packed_U16.T := (Value => 0);
      Component_B_Data_Product_2 : Packed_F32.T := (Value => 0.0);
      Component_B_Data_Product_3 : Sys_Time.T := (0, 0);
      Component_B_Data_Product_4 : Packed_I16.T := (Value => 0);
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send the return status from a sequence load operation.
   overriding procedure Sequence_Load_Return_T_Recv_Sync (Self : in out Instance; Arg : in Sequence_Load_Return.T);
   -- The command send connector. Commands originating from sequences are sent out of this connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- Fetch a data product item from the database. This is used to check telemetry during conditionals in a sequence.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T;
   -- This connector is used to register the components commands with the command router component.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Packets are sent out of this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Tick_T_Send message is dropped due to a full queue.
   overriding procedure Tick_T_Send_Dropped (Self : in out Instance; Arg : in Tick.T);

   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T);

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Sequence_Load_T_Send message is dropped due to a full queue.
   overriding procedure Sequence_Load_T_Send_Dropped (Self : in out Instance; Arg : in Sequence_Load.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Starting a sequence with the following information.
   overriding procedure Starting_Sequence (Self : in out Instance; Arg : in Sequence_Load_Info.T);
   -- The sequence engine has finished its execution of the parent sequence.
   overriding procedure Finished_Sequence (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T);
   -- A command was received to change the packet period of the summary packet.
   overriding procedure Summary_Packet_Period_Set (Self : in out Instance; Arg : in Packed_U16.T);
   -- The sequencer engine details packet was sent for the request engine.
   overriding procedure Details_Packet_Sent (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T);
   -- The operation could not be completed because the engine ID provided is invalid.
   overriding procedure Invalid_Engine_Id (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T);
   -- The sequence could not be run due to a bad CRC.
   overriding procedure Invalid_Sequence_Crc (Self : in out Instance; Arg : in Sequence_Crc_Error.T);
   -- The sequence could not be run due to a bad length.
   overriding procedure Invalid_Sequence_Length (Self : in out Instance; Arg : in Sequence_Length_Error.T);
   -- The sequence could not be run due to unexpected ID.
   overriding procedure Invalid_Sequence_Id (Self : in out Instance; Arg : in Sequence_Id_Error.T);
   -- The sequence could not be run because the engine has not yet been initialized.
   overriding procedure Load_To_Uninitialized_Engine (Self : in out Instance; Arg : in Sequence_Load.T);
   -- The sequence could not be run due to unexpected engine id.
   overriding procedure Load_To_Invalid_Engine_Id (Self : in out Instance; Arg : in Sequence_Load.T);
   -- No engine is available to take a sequence load.
   overriding procedure No_Engine_Available (Self : in out Instance; Arg : in Sequence_Load.T);
   -- No engine is available to take a sequence load from another currently running sequence.
   overriding procedure No_Engine_Available_For_Load (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T);
   -- The sequence could not be run because the current engine is in use.
   overriding procedure Engine_In_Use (Self : in out Instance; Arg : in Sequence_In_Use_Error.T);
   -- A sequence could not be loaded due to an internal sequence runner error.
   overriding procedure Sequence_Load_Error (Self : in out Instance; Arg : in Sequence_Load_Error_Info.T);
   -- A command was executed to kill all running sequences.
   overriding procedure Killed_All_Engines (Self : in out Instance);
   -- A command was executed to kill a sequence running in a specific engine
   overriding procedure Killed_Engine (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T);
   -- A command was dropped due to a full queue.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T);
   -- A command response was dropped due to a full queue.
   overriding procedure Dropped_Command_Response (Self : in out Instance; Arg : in Command_Response.T);
   -- A tick was dropped due to a full queue.
   overriding procedure Dropped_Tick (Self : in out Instance; Arg : in Tick.T);
   -- A sequence load was dropped due to a full queue.
   overriding procedure Dropped_Sequence_Load (Self : in out Instance; Arg : in Sequence_Load.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command response was found with an unrecognized source ID.
   overriding procedure Unexpected_Command_Response (Self : in out Instance; Arg : in Command_Response.T);
   -- An extra source registration was received, but all engines have a source ID already.
   overriding procedure Unexpected_Register_Source (Self : in out Instance; Arg : in Command_Response.T);
   -- An error occurred while executing a sequence.
   overriding procedure Sequence_Execution_Error (Self : in out Instance; Arg : in Engine_Error_Type.T);
   -- A sequence timed out waiting on a command response or subsequence load.
   overriding procedure Sequence_Timeout_Error (Self : in out Instance; Arg : in Engine_Error_Type.T);
   -- A command response was received with an unexpected command ID.
   overriding procedure Unexpected_Command_Response_Id (Self : in out Instance; Arg : in Unexpected_Command_Response_Info.T);
   -- A command from a sequence failed to execute successfully.
   overriding procedure Sequence_Command_Failure (Self : in out Instance; Arg : in Command_Fail_Error_Type.T);
   -- During a sequence load from an engine, the destination engine ID was found to be out of range.
   overriding procedure Engine_Id_Out_Of_Range_Error (Self : in out Instance; Arg : in Engine_Id_Out_Of_Range.T);
   -- During a sequence load from an engine, the destination engine ID was found to be unavailable.
   overriding procedure Engine_Unavailable_For_Load (Self : in out Instance; Arg : in Engine_Id_Out_Of_Range.T);
   -- A data product was fetched with an ID that was not recognized.
   overriding procedure Data_Product_Id_Out_Of_Range_Error (Self : in out Instance; Arg : in Engine_Error_Type.T);
   -- Data could not be parsed out of the fetched data product.
   overriding procedure Data_Product_Extraction_Error (Self : in out Instance; Arg : in Engine_Error_Type.T);
   -- The recursion limit was exceeded in a call to execute. This likely means a malformed sequence was executed.
   overriding procedure Execute_Recursion_Limit_Exceeded (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T);
   -- The engine kill range is invalid and cannot be executed.
   overriding procedure Invalid_Engine_Kill_Range (Self : in out Instance; Arg : in Packed_Engine_Kill_Params.T);
   -- The provided engines were killed by another engine.
   overriding procedure Engines_Killed (Self : in out Instance; Arg : in Packed_Engine_Kill_Params.T);
   -- A sequence is sending the following print statement.
   overriding procedure Print (Self : in out Instance; Arg : in Seq_Print_Event_Record.T);
   -- Arguments were successfully loaded into an engine by command.
   overriding procedure Loaded_Engine_Arguments (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T);
   -- Arguments were not successfully loaded into an engine because the engine is currently busy.
   overriding procedure Unable_To_Load_Engine_Arguments (Self : in out Instance; Arg : in Unexpected_Engine_State.T);
   -- The telemetry type specified in the sequence is not handled by this implementation.
   overriding procedure Unhandled_Telemetry_Type (Self : in out Instance; Arg : in Engine_Error_Type.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Command Sequencer component.
   -- The current packet period for the summary packet.
   overriding procedure Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Command Sequencer.
   -- This packet contains a brief summary of what sequences are running in the sequencer's engines. Its type is determined dynamically based on the number of engines declared in the component.
   overriding procedure Summary_Packet (Self : in out Instance; Arg : in Packet.T);
   -- This packet contains all useful information about the current state a single engine within the sequence. Its type is determined dynamically based on the stack size allocated to the engines.
   overriding procedure Details_Packet (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

   -----------------------------------------------
   -- Custom white-box subprograms:
   -----------------------------------------------
   not overriding function Get_Engine_State (Self : in out Instance; Engine_Id : in Seq_Types.Sequence_Engine_Id) return Seq_Enums.Seq_Engine_State.E;

end Component.Command_Sequencer.Implementation.Tester;
