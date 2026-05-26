--------------------------------------------------------------------------------
-- Simple_Command_Sequencer Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Serializer_Types; use Serializer_Types;
with Simple_Sequencer_Types; use Simple_Sequencer_Types;
with Sequence_Enums; use Sequence_Enums.Sequence_Response_Behavior;
with Sequence_Event_Info.Assertion; use Sequence_Event_Info.Assertion;
with Sequence_Step_Event_Info.Assertion; use Sequence_Step_Event_Info.Assertion;
with Sequence_Sleep_Event_Info.Assertion; use Sequence_Sleep_Event_Info.Assertion;
with Sequence_Timeout_Event_Info.Assertion; use Sequence_Timeout_Event_Info.Assertion;
with Command_Response; use Command_Response;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Test_Assembly_Command_Sequences_Example_Sequences;
with Test_Component_Commands;
with Command.Assertion; use Command.Assertion;
with Packed_U32;
with Sequence_B_Arg;
with Tick;
with Interfaces;

package body Simple_Command_Sequencer_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Allocate heap memory to component:
      T.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10);

      -- Make necessary connections between tester and component:
      T.Connect;

      -- TODO Call component init here.
      T.Component_Instance.Init (Num_Concurrent_Sequences => 2, Sequences => Test_Assembly_Command_Sequences_Example_Sequences.Sequences);

      -- Call the component set up method that the assembly would normally call.
      T.Component_Instance.Set_Up;

      T.Command_Response_T_Send ((
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));

      T.Command_Response_T_Send ((
         Source_Id => 1,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));

      Natural_Assert.Eq (T.Dispatch_All, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- TODO Insert custom cleanup code here.
      null;
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Start_Sequence (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => 0, Subseconds => 0);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- We rely on the sequence's default Command_Timeout_Millis to be well above
      -- any time advance in this test, so no timeout fires.
      Status := T.Commands.Run_Sequence ((Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 0, Frame_Id => 0));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check no failed events
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      -- Change from magics?
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_2 ((Seconds => 3, Subseconds => 14)));

      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check no failed events
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Sleeping
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Haven't Finished Sleeping
      T.System_Time := (Seconds => 2, Subseconds => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Finished Sleep
      T.System_Time := (Seconds => 3, Subseconds => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_3 ((Value => 3)));

      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check no failed events
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check Sequence Completed
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Completed_History.Get (1), (Sequence_Id => 0, Frame_Id => 0));
   end Test_Start_Sequence;

   --  Sequence_B (id=1): two dynamic steps — Component_A.Command_3 then Component_B.Command_3,
   --  both resolved from the sequence argument. wait_for_cmd_resp=true, abort_on_failure=true.
   overriding procedure Test_Dynamic_Sequence (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
      Argument : Sequence_B_Arg.T;
      BArray : Simple_Sequencer_Types.Run_Sequence_Buffer_Type;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => 0, Subseconds => 0);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      Argument := (Component_A_Arg => (Value => 12), Component_B_Arg => (Value => 13));
      BArray := [others => 0];
      BArray (BArray'First .. BArray'First + Sequence_B_Arg.Serialization.Serialized_Length - 1) :=
         Sequence_B_Arg.Serialization.To_Byte_Array (Argument);
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 1, Response_Behavior => Send_After_Sequence_Start,
          Arg_Length => Sequence_B_Arg.Serialization.Serialized_Length, Buffer_Arg => BArray), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Sequence_Started is always emitted by Run_Sequence on a valid load
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 1, Frame_Id => 0));

      -- First tick: Frame 0 is Running -> Execute_Sequence dispatches step 0
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 (Argument.Component_A_Arg));

      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Second tick: Frame 0 Running again -> Execute_Sequence dispatches step 1
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_B_Commands.Command_3 (Argument.Component_B_Arg));

      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No new events yet (still 1: Sequence_Started)
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Final tick: step > last -> Sequence_Completed
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Completed_History.Get (1), (Sequence_Id => 1, Frame_Id => 0));
   end Test_Dynamic_Sequence;

   --  Exercises an autocoded per-sequence wrapper command end-to-end. Sending
   --  Sequence_B (the synthesized command for the second declared sequence) must
   --  behave exactly like the manual Run_Sequence form in Test_Dynamic_Sequence:
   --  the wrapper packs its typed User_Args into the Run_Sequence buffer and
   --  dispatches to Sequences-table slot 1, so the same sub-commands are emitted
   --  with the same unpacked arguments.
   overriding procedure Test_Synthesized_Sequence_Command (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
      Argument : constant Sequence_B_Arg.T := (Component_A_Arg => (Value => 12), Component_B_Arg => (Value => 13));
      Cmd : Command.T;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => 0, Subseconds => 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Build and send the synthesized Sequence_B command. No manual Sequence_Id
      -- or buffer packing -- that is the wrapper's job. Sequence_B_Run_Arg is a
      -- fixed-length type, so the suite generates the direct Command.T form.
      Cmd := T.Commands.Sequence_B ((User_Args => Argument, Response_Behavior => Send_After_Sequence_Start));

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- The wrapper dispatched to slot 1 (Sequence_B).
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 1, Frame_Id => 0));

      -- First tick: step 0 dispatches Component_A.Command_3 with the unpacked Component_A_Arg.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 (Argument.Component_A_Arg));

      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Second tick: step 1 dispatches Component_B.Command_3 with the unpacked Component_B_Arg.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_B_Commands.Command_3 (Argument.Component_B_Arg));
   end Test_Synthesized_Sequence_Command;

   --  A Run_Sequence command with a Sequence_Id outside the sequences array range
   --  must fire Invalid_Sequence_Id and return Failure.
   overriding procedure Test_Invalid_Sequence_Id (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      -- 99 is well out of range for our three-sequence table
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 99, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Invalid_Sequence_Id event; no Sequence_Started
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Sequence_Id_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 0);

      -- No command should have been dispatched downstream
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
   end Test_Invalid_Sequence_Id;

   --  All frames are occupied -> third Run_Sequence fires No_Frame_Available.
   overriding procedure Test_No_Frame_Available (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      -- Occupy Frame 0
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);

      -- Occupy Frame 1
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 2);

      -- Both frames now in Running state; attempt a third sequence
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No_Frame_Available event; no additional Sequence_Started
      Natural_Assert.Eq (T.No_Frame_Available_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 2);
   end Test_No_Frame_Available;

   --  Sequence_B (abort_on_failure=true): a Failure response emits Command_Failure
   --  followed by Sequence_Aborted, and the frame goes Not_Running immediately.
   overriding procedure Test_Command_Failure_Aborts_Sequence (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Argument : Packed_U32.T;
      BArray : Simple_Sequencer_Types.Run_Sequence_Buffer_Type;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);

      Argument := (Value => 5);
      BArray := [others => 0];
      BArray (BArray'First .. BArray'First + Packed_U32.Serialization.Serialized_Length - 1) :=
         Packed_U32.Serialization.To_Byte_Array (Argument);
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 1, Response_Behavior => Send_After_Sequence_Start,
          Arg_Length => Packed_U32.Serialization.Serialized_Length, Buffer_Arg => BArray), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);

      -- Advance to step 0 dispatch
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 (Argument));

      -- Respond with Failure
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Failure));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Command_Failure + Sequence_Aborted events (plus the earlier Sequence_Started = 3 total)
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Command_Failure_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Aborted_History.Get_Count, 1);

      -- Step was incremented to 1 before the response arrived
      Sequence_Step_Event_Info_Assert.Eq (T.Sequence_Aborted_History.Get (1),
         (Sequence_Id => 1, Frame_Id => 0, Step => 1));

      -- Subsequent ticks must not dispatch any further commands
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 0);
   end Test_Command_Failure_Aborts_Sequence;

   --  Sequence_A (abort_on_failure=false / continue_on_failure=true): a Failure response
   --  emits Command_Failure but NOT Sequence_Aborted; execution resumes on the next tick.
   overriding procedure Test_Command_Failure_Continues_Sequence (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => 0, Subseconds => 0);

      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);

      -- Dispatch step 0: Command_1
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- Fail Command_1
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Failure));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Only Command_Failure (no Sequence_Aborted)
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2); -- Started + Command_Failure
      Natural_Assert.Eq (T.Command_Failure_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Aborted_History.Get_Count, 0);

      -- Next tick: frame is Running, Execute_Sequence picks up at step 1 (Command_2)
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2),
         Component_A_Commands.Command_2 ((Seconds => 3, Subseconds => 14)));
   end Test_Command_Failure_Continues_Sequence;

   --  Two sequences run simultaneously on Frame 0 (Source_Id 0) and Frame 1 (Source_Id 1).
   --  A single tick advances both frames; each frame responds independently.
   overriding procedure Test_Concurrent_Sequences (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Frame_0_Commands : Test_Component_Commands.Instance;
      Frame_1_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Frame_0_Commands.Set_Id_Base (1);
      Frame_0_Commands.Set_Source_Id (0);
      Frame_1_Commands.Set_Id_Base (1);
      Frame_1_Commands.Set_Source_Id (1);

      T.System_Time := (Seconds => 0, Subseconds => 0);

      -- Start Sequence_A on Frame 0
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 0, Frame_Id => 0));

      -- Start Sequence_A on Frame 1
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 2);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (2), (Sequence_Id => 0, Frame_Id => 1));

      -- Single tick: the frame loop advances both Running frames, each dispatching step 0
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Frame_0_Commands.Command_1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Frame_1_Commands.Command_1);

      -- Neither sequence has completed yet
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 0);

      -- Respond to Frame 0
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => Frame_0_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Respond to Frame 1
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0,
         Command_Id => Frame_1_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next tick: both frames advance to step 1 (Command_2)
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3),
         Frame_0_Commands.Command_2 ((Seconds => 3, Subseconds => 14)));
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4),
         Frame_1_Commands.Command_2 ((Seconds => 3, Subseconds => 14)));

      -- Both sequences still running concurrently
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 0);
   end Test_Concurrent_Sequences;

   --  A command response with an unknown Source_Id emits Unexpected_Command_Response.
   --  A response whose Source_Id matches a frame that is not in Waiting_For_Cmd_Resp is silently
   --  ignored (no event, no command dispatched).
   overriding procedure Test_Spurious_Command_Response (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Source_Id 99 is not assigned to any frame
      T.Command_Response_T_Send ((Source_Id => 99, Registration_Id => 0,
         Command_Id => 0, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Unexpected_Command_Response_History.Get_Count, 1);

      -- Frame 0 (Source_Id 0) is Not_Running; response has matching Source_Id but no Waiting_For_Cmd_Resp state
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => 0, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      -- Still just the one Unexpected_Command_Response event from before
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
   end Test_Spurious_Command_Response;

   --  While a frame is in Waiting_For_Cmd_Resp, ticks must not advance execution.
   --  Only the timeout path is active; with the sequence's Command_Timeout_Millis
   --  set well above the tick interval, "Time >= Last_Send + Timeout" is always
   --  false, so no timeout fires either.
   overriding procedure Test_Tick_Does_Not_Wake_Waiting_For_Resp (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => 0, Subseconds => 0);

      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- First tick: dispatches Command_1, frame becomes Waiting_For_Cmd_Resp
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- Additional ticks must not dispatch any further command (frame still waiting)
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- No timeout event fired (default sequence timeout is well above the tick interval)
      Natural_Assert.Eq (T.Sequence_Timeout_History.Get_Count, 0);

      -- Confirm the response still wakes the frame correctly
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0,
         Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2),
         Component_A_Commands.Command_2 ((Seconds => 3, Subseconds => 14)));
   end Test_Tick_Does_Not_Wake_Waiting_For_Resp;

   --  A dropped Command_T message triggers a Dropped_Command event carrying the
   --  command header. The drop handler is synchronous; no queue dispatch is needed.
   overriding procedure Test_Dropped_Command (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      -- Simulate a queue-full drop by calling the drop handler directly
      T.Component_Instance.Command_T_Recv_Async_Dropped (Cmd);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Command_History.Get_Count, 1);

      -- The dropped command must not have started any sequence
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 0);
   end Test_Dropped_Command;

   --  A dropped Command_Response message triggers a Dropped_Command_Response event.
   --  The affected frame stays in Waiting_For_Cmd_Resp indefinitely (stuck executor).
   overriding procedure Test_Dropped_Command_Response (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
      Dropped_Resp : constant Command_Response.T :=
         (Source_Id => 0, Registration_Id => 0,
          Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success);
   begin
      T.System_Time := (Seconds => 0, Subseconds => 0);

      -- Start a sequence and get to Waiting_For_Cmd_Resp
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Drop the response instead of delivering it
      T.Component_Instance.Command_Response_T_Recv_Async_Dropped (Dropped_Resp);

      Natural_Assert.Eq (T.Dropped_Command_Response_History.Get_Count, 1);

      -- Frame is still stuck; a tick must not dispatch any new command
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
   end Test_Dropped_Command_Response;

   --  A dropped Tick triggers a Dropped_Tick event and is otherwise harmless.
   overriding procedure Test_Dropped_Tick (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Dropped : constant Tick.T := ((0, 0), 0);
   begin
      T.Component_Instance.Tick_T_Recv_Async_Dropped (Dropped);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Tick_History.Get_Count, 1);

      -- No sequence state should have changed
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 0);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
   end Test_Dropped_Tick;

   --  A command whose argument buffer fails validation causes Invalid_Command_Received
   --  to be emitted. Crafting a raw Command.T with Arg_Buffer_Length = 0 bypasses
   --  the serializer and arrives with the wrong size for Run_Sequence_Arg.
   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Bad_Cmd : constant Command.T := (
         Header => (
            Source_Id => 0,
            Id => T.Commands.Get_Run_Sequence_Id,
            Arg_Buffer_Length => 0),          -- correct length for Run_Sequence_Arg is > 0
         Arg_Buffer => [others => 0]);
   begin
      T.Command_T_Send (Bad_Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);

      -- No sequence must have been started
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 0);
   end Test_Invalid_Command;

   overriding procedure Test_No_Wait_Sequence (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => 0, Subseconds => 0);

      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 2, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 2, Frame_Id => 0));

      -- Single tick: Execute_Sequence loops through all steps without pausing,
      -- then crosses the end-of-sequence boundary in the same call.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Both commands dispatched in a single tick
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_1);

      -- Sequence_Completed also fires within the same Execute_Sequence call
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Completed_History.Get (1), (Sequence_Id => 2, Frame_Id => 0));

      -- Total events: Sequence_Started + Sequence_Completed
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
   end Test_No_Wait_Sequence;

   --  After a sequence completes, its frame returns to Not_Running with Has_Source_Id
   --  still set. Find_Available_Sequence_Frame must rediscover it for the next run.
   overriding procedure Test_Frame_Reuse_After_Completion (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      T.System_Time := (Seconds => 0, Subseconds => 0);

      -- Run Sequence_C (no-wait, completes in one tick) on Frame 0
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 2, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 2, Frame_Id => 0));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Completed_History.Get (1), (Sequence_Id => 2, Frame_Id => 0));

      -- Frame 0 is now Not_Running; start a second sequence and confirm it reuses Frame 0
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 2, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 2);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (2), (Sequence_Id => 2, Frame_Id => 0));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Completed_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4); -- 2 per run * 2 runs
   end Test_Frame_Reuse_After_Completion;

   overriding procedure Test_Out_Of_Range_Sleep (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      T.System_Time := (Seconds => Interfaces.Unsigned_32'Last, Subseconds => 0);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      Status := T.Commands.Run_Sequence ((Sequence_Id => 3, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 3, Frame_Id => 0));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Out_Of_Range_Sleep_History.Get_Count, 1);
      Sequence_Sleep_Event_Info_Assert.Eq (T.Sequence_Out_Of_Range_Sleep_History.Get (1), (Sequence_Id => 3, Frame_Id => 0, Milliseconds => Interfaces.Unsigned_32'Last));
   end Test_Out_Of_Range_Sleep;

   overriding procedure Test_Timeout (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => 0, Subseconds => 0);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      Status := T.Commands.Run_Sequence ((Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 0, Frame_Id => 0));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      T.System_Time := (Seconds => 10000, Subseconds => 0);

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Timeout_History.Get_Count, 1);
      Sequence_Step_Event_Info_Assert.Eq (T.Sequence_Timeout_History.Get (1), (Sequence_Id => 0, Frame_Id => 0, Step => 1));
   end Test_Timeout;

   overriding procedure Test_Out_Of_Range_Timeout (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Component_A_Commands : Test_Component_Commands.Instance;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);

      T.System_Time := (Seconds => Interfaces.Unsigned_32'Last, Subseconds => 0);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      Status := T.Commands.Run_Sequence ((Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start, Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);
      Sequence_Event_Info_Assert.Eq (T.Sequence_Started_History.Get (1), (Sequence_Id => 0, Frame_Id => 0));

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Out_Of_Range_Timeout_History.Get_Count, 1);
      Sequence_Timeout_Event_Info_Assert.Eq (T.Sequence_Out_Of_Range_Timeout_History.Get (1), (Sequence_Id => 0, Frame_Id => 0, Milliseconds => 10_000));
   end Test_Out_Of_Range_Timeout;

   overriding procedure Test_Extra_Sequence_Id (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      T.Command_Response_T_Send ((
         Source_Id => 2,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));

      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Extra_Sequence_Id_History.Get_Count, 1);
   end Test_Extra_Sequence_Id;

   --  Kill_All_Sequences halts running sequences. Frames previously assigned a Source_Id
   --  remain claimable for the next Run_Sequence call.
   overriding procedure Test_Kill_All_Sequences (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Status : Serialization_Status;
   begin
      T.System_Time := (Seconds => 0, Subseconds => 0);

      -- Start a sequence so a frame is in Running state
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start,
          Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 1);

      -- Fire the kill command
      Cmd := T.Commands.Kill_All_Sequences;
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Killed_All_Sequences_History.Get_Count, 1);

      -- A subsequent Run_Sequence must succeed - the frame is claimable again
      Status := T.Commands.Run_Sequence (
         (Sequence_Id => 0, Response_Behavior => Send_After_Sequence_Start,
          Arg_Length => 0, Buffer_Arg => [others => 0]), Cmd);
      pragma Assert (Status = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Sequence_Started_History.Get_Count, 2);
      Natural_Assert.Eq (T.No_Frame_Available_History.Get_Count, 0);
   end Test_Kill_All_Sequences;

   --  Set_Summary_Packet_Period accepts a Packed_U16 and returns Success. The packet
   --  itself is wired in Phase 4; this just exercises the command surface.
   overriding procedure Test_Set_Summary_Packet_Period (Self : in out Instance) is
      T : Component.Simple_Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      Cmd := T.Commands.Set_Summary_Packet_Period ((Value => 42));
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No events on the happy path
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Set_Summary_Packet_Period;
end Simple_Command_Sequencer_Tests.Implementation;
