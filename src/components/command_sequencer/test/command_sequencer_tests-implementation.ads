--------------------------------------------------------------------------------
-- Command_Sequencer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Command Sequencer component.
package Command_Sequencer_Tests.Implementation is
   -- Test data and state:
   type Instance is new Command_Sequencer_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests loading and running a simple sequence.
   overriding procedure Test_Nominal_Load_And_Run_Sequence (Self : in out Instance);
   -- This unit test tests loading and running a sequence that loads a subsequence.
   overriding procedure Test_Nominal_Subsequence_Load (Self : in out Instance);
   -- This unit test tests loading and running a sequence that loads a sequence in another engine.
   overriding procedure Test_Nominal_Sequence_Spawn (Self : in out Instance);
   -- This unit test tests loading and running a sequence that loads a sequence into the same engine.
   overriding procedure Test_Nominal_Sequence_Replace (Self : in out Instance);
   -- This unit test tests loading and running a sequence that executes a conditional on a data product.
   overriding procedure Test_Nominal_Sequence_Telemetry_Compare (Self : in out Instance);
   -- This unit test tests loading and running a sequence that executes a conditional on a data product with the waitnewvalue keyword.
   overriding procedure Test_Nominal_Sequence_Wait_New_Value (Self : in out Instance);
   -- This unit test tests loading and running a sequence that grabs a data product and sets it as a local variable.
   overriding procedure Test_Nominal_Fetch_Data_Product (Self : in out Instance);
   -- This unit test tests loading and running a sequence that executes a conditional on a data product that is malformed.
   overriding procedure Test_Sequence_Telemetry_Compare_Error (Self : in out Instance);
   -- This unit test tests a few corner cases related to telemetry comparisons and makes sure they behave as intended.
   overriding procedure Test_Sequence_Telemetry_Compare_Corner_Cases (Self : in out Instance);
   -- This unit test tests loading and running a sequence that loads a sequence into another engine that does not exist.
   overriding procedure Test_Sequence_Spawn_Invalid_Engine (Self : in out Instance);
   -- This unit test tests loading and running a sequence that loads a sequence into an engine that is currently busy.
   overriding procedure Test_Sequence_Spawn_Unavailable_Engine (Self : in out Instance);
   -- This unit test tests loading and running a sequence that loads a sequence into any engine when no engines are available.
   overriding procedure Test_Sequence_Spawn_Any_Unavailable (Self : in out Instance);
   -- This unit test tests loading and running a sequence that executes a conditional on a data product that is not available or has an unknown ID.
   overriding procedure Test_Data_Product_Fetch_Error (Self : in out Instance);
   -- This unit test tests loading and running a simple sequence with relative and absolute waits.
   overriding procedure Test_Relative_And_Absolute_Wait_Sequence (Self : in out Instance);
   -- This unit test tests all the error conditions associated with a bad sequence load.
   overriding procedure Test_Sequence_Load_Error (Self : in out Instance);
   -- This unit test tests how the component responds to a sequence that has a failed command.
   overriding procedure Test_Sequence_Execution_Error (Self : in out Instance);
   -- This unit test tests sequence command and subsequence load timeouts.
   overriding procedure Test_Sequence_Timeouts (Self : in out Instance);
   -- This unit test tests issuing the details packet by command.
   overriding procedure Test_Issue_Details_Packet (Self : in out Instance);
   -- This unit test tests changing the summary packet period by command.
   overriding procedure Test_Set_Summary_Packet_Period (Self : in out Instance);
   -- This unit test exercises commands sent to invalid engine IDs and makes sure they do not execute.
   overriding procedure Test_Command_Invalid_Engine (Self : in out Instance);
   -- This unit test exercises that an invalid command throws the appropriate event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- This unit test exercises that a queue overflow results in the appropriate event.
   overriding procedure Test_Queue_Overflow (Self : in out Instance);
   -- This unit test tests how the component responds to a sequence that is corrupted and fails internally.
   overriding procedure Test_Sequence_Internal_Execution_Error (Self : in out Instance);
   -- This unit test tests the kill engine command.
   overriding procedure Test_Kill_Engine_Command (Self : in out Instance);
   -- This unit test tests the kill engine sequence opcode.
   overriding procedure Test_Kill_Opcode (Self : in out Instance);
   -- This unit test tests the sequence recursion error, where the execute recursion limit is exceeded.
   overriding procedure Test_Recursion_Error (Self : in out Instance);
   -- This unit test tests the sequencer print opcode which produces an event from the command sequencer.
   overriding procedure Test_Print (Self : in out Instance);
   -- This unit test tests the set engine arguments command.
   overriding procedure Test_Set_Engine_Arguments (Self : in out Instance);
   -- This unit test tests the return value feature.
   overriding procedure Test_Return_Val (Self : in out Instance);
   -- This unit test tests what happens when a floating point sequence variable overflows.
   overriding procedure Test_Bad_Float (Self : in out Instance);
   -- This unit test tests to make sure commands with complex arguments are formed correctly by the sequencer.
   overriding procedure Test_Complex_Command (Self : in out Instance);
   -- This unit test tests a signed integer corner case that needs to be handled.
   overriding procedure Test_Signed_Integer_Handling (Self : in out Instance);
   -- This unit test tests a telemetry fetch when the telemetry item never becomes available.
   overriding procedure Test_Set_Telemetry_Timeout (Self : in out Instance);
   -- This unit test tests when a sub sequence load fails due to timeout.
   overriding procedure Test_Sub_Seq_Load_Timeout (Self : in out Instance);

   -- Test data and state:
   type Instance is new Command_Sequencer_Tests.Base_Instance with record
      null;
   end record;
end Command_Sequencer_Tests.Implementation;
