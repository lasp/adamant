--------------------------------------------------------------------------------
-- Command_Sequencer Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Command_Response;
with Command;
with Sequence_Load;
with Seq;
with Seq_Types;

-- The Command Sequencer component executes command sequences with a configurable number of engines. The sequence engines execute sequences in the LASP Awesome Sequence Engine Language (LASEL) compiled by the LASP SEQ tool. Documentation on LASEL is included in this component's doc/ directory.
--
-- This component runs a configurable number of sequence engines using a single Adamant task. The task runs each engine in priority order, where lower numbered engines take precedence over higher numbered engines. Each engine contains a configurable-sized stack that allows sequences to call subsequences. This component adheres to the property that commands are only executed after previous commands have completed (i.e. a command response has been received). In this way the sequences are largely event driven, waiting on the execution of previous commands to finish prior to executing subsequent ones. A periodic tick is supplied to the component to provide timing control for sequences that need to execute relative or absolute waits, or check until a telemetry condition has been met before proceeding.
--
-- The sequence engine and LASEL interpreter is located in the seq/ directory.
package Component.Command_Sequencer.Implementation is

   -- The component class instance record:
   type Instance is new Command_Sequencer.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The initialization subprogram creates a sequencer with the desired number of engines and internal stack sizes.
   --
   -- Init Parameters:
   -- Num_Engines : Seq_Types.Num_Engines_Type - The number of engines allocated in the sequencer. This determines the number of sequences the component can run in parallel.
   -- Stack_Size : Seq_Types.Stack_Depth_Type - The size of the stack allocated for each engine in entries. Each stack entry contains a single running sequence, and additional stack entries can be used for subsequence calls. A value of 5 here would allow a sequence to call subsequences up to 5 levels deep.
   -- Create_Sequence_Load_Command_Function : Create_Sequence_Load_Command_Access - When a sequence loads or spans or calls another sequence, the command sequencer will call this function to formulate the correct sequence load command for the assembly. Since the specifics of sequence loading often varies on a mission by mission basis, this function allows the encoding of that mission specific behavior by the user.
   -- Packet_Period : Interfaces.Unsigned_16 - The initial packet rate for the sequencer summary packet in ticks. A value of 0 disabled the packet.
   -- Continue_On_Command_Failure : Boolean - If set to True, then the sequence engines will continue to execute even if a sent command fails. If set to False, then the engines will halt with an error status if a sent command fails.
   -- Timeout_Limit : Natural - The number of ticks to wait before timing out sequencer operations such as waiting on a command response or subsequence load. If a timeout of this type occurs the engine will transition to an error state. A value of zero disables these timeouts.
   -- Instruction_Limit : Positive - The maximum number of sequence instructions we allow the sequence to execute without hitting a pausing action such as sending a command, waiting on telemetry, or waiting for a relative or absolute time. The purpose of this parameter is to prevent a sequence from entering an infinite execution loop which would cause the entire component task to hang indefinitely. You should set the value to some maximum number of instructions that you never expect any of your compiled sequences to hit.
   --
   overriding procedure Init (Self : in out Instance; Num_Engines : in Seq_Types.Num_Engines_Type; Stack_Size : in Seq_Types.Stack_Depth_Type; Create_Sequence_Load_Command_Function : in not null Create_Sequence_Load_Command_Access; Packet_Period : in Interfaces.Unsigned_16; Continue_On_Command_Failure : in Boolean; Timeout_Limit : in Natural; Instruction_Limit : in Positive);
   not overriding procedure Final (Self : in out Instance);

private

   -- Create a type for an array of sequence engines.
   type Seq_Engine_Array is array (Seq_Types.Sequence_Engine_Id range <>) of Seq.Engine;
   type Seq_Engine_Array_Access is access all Seq_Engine_Array;

   -- Create a type for auxiliary data for each sequence engine. This holds information
   -- about each sequence engine that is not held within the engine type itself.
   type Engine_Aux_Data_Type is record
      -- Timeout counter - this helps the component timeout while waiting on a specific
      -- event that never occurs.
      Timeout_Counter : Natural := 0;
      -- Create a command error count which increments when a sent command fails.
      Command_Error_Counter : Interfaces.Unsigned_16 := 0;
   end record;
   type Engine_Aux_Data_Array is array (Seq_Types.Sequence_Engine_Id range <>) of Engine_Aux_Data_Type;
   type Engine_Aux_Data_Array_Access is access all Engine_Aux_Data_Array;

   -- The component class instance record:
   type Instance is new Command_Sequencer.Base_Instance with record
      -- The sequence engines and timeout counters:
      Seq_Engines : Seq_Engine_Array_Access := null;
      Engine_Aux_Data : Engine_Aux_Data_Array_Access := null;
      -- Maximum number of instructions that can be executed without a pausing action. Prevents
      -- infinite loops within a running sequence.
      Instruction_Limit : Positive := Positive'Last;
      -- Maximum number of ticks to spend waiting on command responses and subsequence loads. A
      -- timeout results in a transition of the engine to an error state.
      Timeout_Limit : Natural := Natural'First;
      -- Configuration which determines whether or not an engine continues to execute after a
      -- command fails or halts with an error.
      Continue_On_Command_Failure : Boolean := False;
      -- Counter for issuing packets at the correct time:
      Packet_Counter : Interfaces.Unsigned_16 := Interfaces.Unsigned_16'First;
      Packet_Period : Interfaces.Unsigned_16 := 1;
      -- Function access to the mission specific function which returns the sequence load command
      -- for the assembly.
      Create_Sequence_Load_Command_Function : Create_Sequence_Load_Command_Access := null;
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
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The schedule invokee connector. This is used to detect sequence timeout errors, meter out the checking of telemetry for sequence conditionals, and determine when to resume a sequence after a relative or absolute wait.
   overriding procedure Tick_T_Recv_Async (Self : in out Instance; Arg : in Tick.T);
   -- This procedure is called when a Tick_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Tick_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Tick.T);
   -- Command responses from sent commands are received on this connector, allowed subsequent commands in a sequence to be sent out.
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T);
   -- This procedure is called when a Command_Response_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T);
   -- The command receive connector. Commands received on this connector are executed by the sequencer itself, i.e. halting a sequence.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- This connector is used to load a sequence into the command sequencer via memory region. Sequences are not copied to this component's memory, they are run directly from the address provided in the given sequence load memory region.
   overriding procedure Sequence_Load_T_Recv_Async (Self : in out Instance; Arg : in Sequence_Load.T);
   -- This procedure is called when a Sequence_Load_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Sequence_Load_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Sequence_Load.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Sequence_Load_Return_T_Send message is dropped due to a full queue.
   overriding procedure Sequence_Load_Return_T_Send_Dropped (Self : in out Instance; Arg : in Sequence_Load_Return.T) is null;
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Command Sequencer.
   -- This command halts all currently running engines.
   overriding function Kill_All_Engines (Self : in out Instance) return Command_Execution_Status.E;
   -- This command halts an engine with the provided engine number.
   overriding function Kill_Engine (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) return Command_Execution_Status.E;
   -- Set the period of the sequence summary packet. A period of zero disables the sending of the packet.
   overriding function Set_Summary_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;
   -- The sequence details packet for a particular engine is issued when this command is received.
   overriding function Issue_Details_Packet (Self : in out Instance; Arg : in Packed_Sequence_Engine_Id.T) return Command_Execution_Status.E;
   -- If a sequence requires arguments to be run correctly at the parent level, this command can be used to set the arguments into the engine prior to loading the sequence. This command will only be executed if there is no other sequence loaded in this engine. Arguments can only be set for a sequence that is going to be loaded into the parent stack position. If this command is not run prior to running a sequence in an engine, then the arguments will default to values of zero. If a sequence does not require some or all of the 16 arguments, then those arguments will never be read, and thus do not need to be set by this command.
   overriding function Set_Engine_Arguments (Self : in out Instance; Arg : in Packed_Variable_Array.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Command_Sequencer.Implementation;
