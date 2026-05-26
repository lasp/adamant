--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Simple Command Sequencer component
package Simple_Command_Sequencer_Tests.Implementation is

   -- Test data and state:
   type Instance is new Simple_Command_Sequencer_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   overriding procedure Test_Start_Sequence (Self : in out Instance);
   overriding procedure Test_Dynamic_Sequence (Self : in out Instance);
   overriding procedure Test_Invalid_Sequence_Id (Self : in out Instance);
   overriding procedure Test_No_Frame_Available (Self : in out Instance);
   overriding procedure Test_Command_Failure_Aborts_Sequence (Self : in out Instance);
   overriding procedure Test_Command_Failure_Continues_Sequence (Self : in out Instance);
   overriding procedure Test_Concurrent_Sequences (Self : in out Instance);
   overriding procedure Test_Spurious_Command_Response (Self : in out Instance);
   overriding procedure Test_Tick_Does_Not_Wake_Waiting_For_Resp (Self : in out Instance);
   overriding procedure Test_Dropped_Command (Self : in out Instance);
   overriding procedure Test_Dropped_Command_Response (Self : in out Instance);
   overriding procedure Test_Dropped_Tick (Self : in out Instance);
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   overriding procedure Test_No_Wait_Sequence (Self : in out Instance);
   overriding procedure Test_Frame_Reuse_After_Completion (Self : in out Instance);
   overriding procedure Test_Out_Of_Range_Sleep (Self : in out Instance);
   overriding procedure Test_Timeout (Self : in out Instance);
   overriding procedure Test_Out_Of_Range_Timeout (Self : in out Instance);
   overriding procedure Test_Extra_Sequence_Id (Self : in out Instance);
   overriding procedure Test_Kill_All_Sequences (Self : in out Instance);
   overriding procedure Test_Set_Summary_Packet_Period (Self : in out Instance);
   overriding procedure Test_Synthesized_Sequence_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Simple_Command_Sequencer_Tests.Base_Instance with record
      null;
   end record;
end Simple_Command_Sequencer_Tests.Implementation;
