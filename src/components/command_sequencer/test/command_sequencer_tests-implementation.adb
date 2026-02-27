--------------------------------------------------------------------------------
-- Command_Sequencer Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Ada.Sequential_IO;
with Basic_Types; use Basic_Types;
with Memory_Region;
with Ada.Text_IO; use Ada.Text_IO;
with Command_Sequencer_Enums;
use Command_Sequencer_Enums.Sequence_Load_Engine_Request_Type;
use Command_Sequencer_Enums.Sequence_Load_Status;
with Sequence_Load_Return.Assertion; use Sequence_Load_Return.Assertion;
with Sequence_Load.Assertion; use Sequence_Load.Assertion;
with Command.Assertion; use Command.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Response.Assertion; use Command_Response.Assertion;
with Tick.Assertion; use Tick.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Interfaces;
with Packed_Sequence_Engine_Id.Assertion; use Packed_Sequence_Engine_Id.Assertion;
with Packet.Assertion; use Packet.Assertion;
with Engine_Details_Type;
with Engine_Summary_Type;
with Sequence_Details_Type;
with Sequence_Load_Info.Assertion; use Sequence_Load_Info.Assertion;
with Sequence_Load_Error_Info.Assertion; use Sequence_Load_Error_Info.Assertion;
with Test_Component_Commands;
with Sequence_Header;
with Sequence_Length_Error.Assertion; use Sequence_Length_Error.Assertion;
with Sequence_Crc_Error.Assertion; use Sequence_Crc_Error.Assertion;
with Sequence_In_Use_Error.Assertion; use Sequence_In_Use_Error.Assertion;
with Sequence_Id_Error.Assertion; use Sequence_Id_Error.Assertion;
with Packed_U16.Assertion; use Packed_U16.Assertion;
with Engine_Error_Type.Assertion; use Engine_Error_Type.Assertion;
with Packed_Engine_Kill_Params.Assertion; use Packed_Engine_Kill_Params.Assertion;
with Crc_16;
with Seq_Enums; use Seq_Enums;
with Command_Fail_Error_Type.Assertion; use Command_Fail_Error_Type.Assertion;
with Packet_Types;
with System;
with Unit_Test_Sequence_Load_Type;
with Sequence_Types;
with Seq_Types;
with Data_Product_Enums;
with Engine_Id_Out_Of_Range.Assertion; use Engine_Id_Out_Of_Range.Assertion;
with Smart_Assert;
with Seq_Print_Event_Record.Assertion; use Seq_Print_Event_Record.Assertion;
with Unexpected_Engine_State.Assertion; use Unexpected_Engine_State.Assertion;
with Byte_Array_Util;
with Complex_Command_Arg;
with Complex_Command_Arg_2;

package body Command_Sequencer_Tests.Implementation is

   use Seq_Engine_State;
   use Seq_Runtime_State;
   use Seq_Error;

   -------------------------------------------------------------------------
   -- Custom smart assertions:
   -------------------------------------------------------------------------
   package Seq_Engine_State_Assert is new Smart_Assert.Discrete (Seq_Enums.Seq_Engine_State.E, Seq_Enums.Seq_Engine_State.E'Image);

   -------------------------------------------------------------------------
   -- Function for creating load sequence command for test assembly:
   -------------------------------------------------------------------------

   function Create_Sequence_Load_Command (Id : in Sequence_Types.Sequence_Id; Engine_Number : in Seq_Types.Sequence_Engine_Id; Engine_Request : in Command_Sequencer_Enums.Sequence_Load_Engine_Request_Type.E) return Command.T is
      To_Return : Command.T := (
         Header => (
            Source_Id => 0,
            Id => 59,
            Arg_Buffer_Length => Unit_Test_Sequence_Load_Type.Size_In_Bytes
         ),
         Arg_Buffer => [others => 0]
      );
   begin
      -- Serialize the data into the buffer:
      To_Return.Arg_Buffer (To_Return.Arg_Buffer'First .. To_Return.Arg_Buffer'First + Unit_Test_Sequence_Load_Type.Size_In_Bytes - 1) :=
         Unit_Test_Sequence_Load_Type.Serialization.To_Byte_Array ((
            Id => Id,
            Engine_Number => Engine_Number,
            Engine_Request => Engine_Request
         ));
      return To_Return;
   end Create_Sequence_Load_Command;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Allocate heap memory to component:
      T.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Make necessary connections between tester and component:
      T.Connect;

      -- Call component init here.
      T.Component_Instance.Init (
         Num_Engines => 3,
         Stack_Size => 3,
         Create_Sequence_Load_Command_Function => Create_Sequence_Load_Command'Access,
         Packet_Period => 1,
         Continue_On_Command_Failure => False,
         Timeout_Limit => 3,
         Instruction_Limit => 10000
      );

      -- Call the component set up method that the assembly would normally call.
      T.Component_Instance.Set_Up;

      -- We need to mimic the command router sending command source IDs to the component, one for each engine:
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

      T.Command_Response_T_Send ((
         Source_Id => 2,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));

      Natural_Assert.Eq (T.Dispatch_All, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Custom cleanup code here.
      T.Component_Instance.Final;

      -- Free component heap:
      T.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helpers:
   -------------------------------------------------------------------------

   function Load_Sequence (File_Path : in String; Buffer : out Basic_Types.Byte_Array) return Natural is
      package Io is new Ada.Sequential_IO (Basic_Types.Byte);
      use Io;
      File : Io.File_Type;
      Data : Basic_Types.Byte;
      Sequence_Size : Natural := Buffer'First;
   begin
      Put_Line ("Loading " & File_Path & "...");
      Open (File, In_File, File_Path);
      while not End_Of_File (File) loop
         Read (File, Data);
         Buffer (Sequence_Size) := Data;
         Sequence_Size := @ + 1;
      end loop;
      Put_Line ("Loaded " & Sequence_Size'Image & " bytes.");
      Close (File);
      return Sequence_Size;
   end Load_Sequence;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Load_And_Run_Sequence (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success)); Natural_Assert.Eq (T.Dispatch_All, 1);
      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_2 ((Seconds => 11, Subseconds => 15)));
      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_3 ((Value => 99)));

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_B_Commands.Command_1);

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_B_Commands.Command_2 ((Seconds => 22, Subseconds => 16)));

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check event:
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));
   end Test_Nominal_Load_And_Run_Sequence;

   overriding procedure Test_Nominal_Subsequence_Load (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/call_06.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Sequence_Buffer_2 : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size_2 : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/subseq_07.bin", Buffer => Sequence_Buffer_2);
      Sequence_Region_2 : constant Memory_Region.T := (Address => Sequence_Buffer_2'Address, Length => Sequence_Size_2);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
      Load_Command : Command.T;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 6, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Ok, send the command response.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Now expect the component to have output another command, the load sequence command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Load_Command := Create_Sequence_Load_Command (Id => 7, Engine_Number => 0, Engine_Request => Specific_Engine);
      Load_Command.Header.Source_Id := 0; -- From engine 0
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

      -- OK, now if we send the command response from the load command it should NOT cause the engine to send out the next command.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect nothing. Engine should remain static until subsequence load.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Load a subsequence with the wrong ID first, to make sure it is rejected.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Unexpected_Sequence_Id
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Sequence_Id_History.Get_Count, 1);
      Sequence_Id_Error_Assert.Eq (T.Invalid_Sequence_Id_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 6, Length => Sequence_Size),
         Expected_Id => 7
      ));

      -- OK, now load the valid subsequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region_2));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region_2),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region_2),
         Header => (Crc => Sequence_Buffer_2 (0 .. 1), Version => 0, Category => 0, Id => 7, Length => Sequence_Size_2),
         Engine_Id => 0,
         Stack_Level => 1
      ));

      -- Make sure we output the first command of subsequence:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_B_Commands.Command_2 ((Seconds => 10, Subseconds => 9)));

      -- Send response:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect parent sequence to be running again, and output the last command of sequence.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_A_Commands.Command_2 ((Seconds => 7, Subseconds => 12)));

      -- Send response:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect finished
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));
   end Test_Nominal_Subsequence_Load;

   overriding procedure Test_Nominal_Sequence_Spawn (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/spawn_05.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Sequence_Buffer_2 : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size_2 : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer_2);
      Sequence_Region_2 : constant Memory_Region.T := (Address => Sequence_Buffer_2'Address, Length => Sequence_Size_2);
      Sequence_Buffer_3 : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size_3 : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer_3);
      Sequence_Region_3 : constant Memory_Region.T := (Address => Sequence_Buffer_3'Address, Length => Sequence_Size_3);
      Component_A_Commands : Test_Component_Commands.Instance;
      Load_Command : Command.T;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 5, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Ok, send the command response.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Now expect the component to have output another command, the load sequence command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Load_Command := Create_Sequence_Load_Command (Id => 1, Engine_Number => 2, Engine_Request => Specific_Engine);
      Load_Command.Header.Source_Id := 0; -- From engine 0
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

      -- OK, now if we send the command response from the load command it should cause the engine to send out the next command.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check next command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_2 ((Seconds => 7, Subseconds => 12)));

      -- Send response and expect another spawn:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Test spawn 2 on 'any' engine:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      -- Engine 1 should have been reserved by component.
      Load_Command := Create_Sequence_Load_Command (Id => 1, Engine_Number => 1, Engine_Request => Specific_Engine);
      Load_Command.Header.Source_Id := 0; -- From engine 0
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Load_Command);

      -- Send the load response and expect engine to be finished executing:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));

      -- Now send the load for the spawn.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region_2));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure spawned sequence started on engine 2.
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region_2),
         Header => (Crc => Sequence_Buffer_2 (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size_2),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Check output commands:
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_A_Commands.Command_1);

      -- Now send the load for the spawn on 'any':
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region_3));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure spawned sequence started on engine 1.
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 3);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region_3),
         Header => (Crc => Sequence_Buffer_3 (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size_3),
         Engine_Id => 1,
         Stack_Level => 0
      ));

      -- Check output commands:
      Component_A_Commands.Set_Source_Id (1);   -- Engine 1
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (6), Component_A_Commands.Command_1);
   end Test_Nominal_Sequence_Spawn;

   overriding procedure Test_Nominal_Sequence_Replace (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/start_04.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Sequence_Buffer_2 : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size_2 : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/args_08.bin", Buffer => Sequence_Buffer_2);
      Sequence_Region_2 : constant Memory_Region.T := (Address => Sequence_Buffer_2'Address, Length => Sequence_Size_2);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
      Load_Command : Command.T;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (1);   -- Engine 1
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (1);   -- Engine 1

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 4, Length => Sequence_Size),
         Engine_Id => 1,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 16, Subseconds => 19)));

      -- Ok, send the command response.
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Now expect the component to have output another command, the load sequence command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Load_Command := Create_Sequence_Load_Command (Id => 8, Engine_Number => 1, Engine_Request => Specific_Engine);
      Load_Command.Header.Source_Id := 1;
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

      -- OK, now if we send the command response from the load command it should be ignored silently by the component.
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No new events or commands:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Now send the load.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region_2));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region_2),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region_2),
         Header => (Crc => Sequence_Buffer_2 (0 .. 1), Version => 0, Category => 0, Id => 8, Length => Sequence_Size_2),
         Engine_Id => 1,
         Stack_Level => 0
      ));

      -- Check output commands from sequence.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_1);

      -- Send a sequence load return from the first sequence again, this time late. It should still be ignored.
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No new events or commands:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_A_Commands.Command_2 ((Seconds => 77, Subseconds => 88)));

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_A_Commands.Command_3 ((Value => 99)));

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (6), Component_B_Commands.Command_1);

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 7);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (7), Component_B_Commands.Command_2 ((Seconds => 99, Subseconds => 88)));

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check event:
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 1));

      -- Expect no extra commands from previous sequence, since we replaced it:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 7);
   end Test_Nominal_Sequence_Replace;

   overriding procedure Test_Nominal_Sequence_Telemetry_Compare (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/waitval_03.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (2);   -- Engine 2

      -- Set data product for sequence.
      T.Component_A_Data_Product_1 := (Value => 1);

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 3, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Make sure no output commands, we have not yet reached the telemetry compare:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Send a tick to the component so it checks against the value, no commands sent yet,
      -- since the returned value is not zero.
      T.Component_A_Data_Product_1 := (Value => 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      -- T.Event_T_Recv_Sync_History.Print;
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Send a tick to the component so it checks against the value, no commands sent yet,
      -- since the returned value is not zero.
      T.Component_A_Data_Product_1 := (Value => 99);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Set the telemetry value to return to the passing value:
      T.Component_A_Data_Product_1 := (Value => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 ((Value => 5)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_B_Commands.Command_3 ((Value => 7))); -- no timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect sequencer to be blocking again:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Send a tick to the component so it checks against the value, no commands sent yet,
      -- since the returned value is not zero.
      T.Component_A_Data_Product_1 := (Value => 99);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Again.
      T.Component_A_Data_Product_1 := (Value => 99);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Set system time such that we are about to reach the timeout:
      T.System_Time := (119999, 0); -- One second   before:
      T.Component_A_Data_Product_1 := (Value => 99);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Set system time such that we are at the timeout:
      T.System_Time := (120002, 0);
      T.Component_A_Data_Product_1 := (Value => 99);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_3 ((Value => 8)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_B_Commands.Command_3 ((Value => 9))); -- yes timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect sequencer to be blocking again:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Send tick with unexpected data product value.
      T.Component_B_Data_Product_2 := (Value => 7.0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Again
      T.Component_B_Data_Product_2 := (Value => 7.0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- OK send expected value:
      T.Component_B_Data_Product_2 := (Value => 7.6);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_A_Commands.Command_3 ((Value => 11)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (6), Component_B_Commands.Command_3 ((Value => 13)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      T.Component_B_Data_Product_1 := (Value => 100);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect sequencer to be blocking again:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);

      -- Send tick with unexpected data product value.
      T.Component_B_Data_Product_1 := (Value => 100);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);

      -- Again.
      T.Component_B_Data_Product_1 := (Value => 100);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);

      -- Set time so we expire the relative wait:
      T.System_Time := (120008, 0);
      T.Component_A_Data_Product_1 := (Value => 100);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Event_T_Recv_Sync_History.Print;
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 7);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (7), Component_A_Commands.Command_3 ((Value => 14)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 8);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (8), Component_B_Commands.Command_3 ((Value => 15))); -- yes timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 9);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (9), Component_A_Commands.Command_3 ((Value => 17)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 10);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (10), Component_B_Commands.Command_3 ((Value => 18))); -- yes timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Set data product so it passes:
      T.Component_B_Data_Product_1 := (Value => 98);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 11);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (11), Component_A_Commands.Command_3 ((Value => 20)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 12);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (12), Component_B_Commands.Command_3 ((Value => 21))); -- yes timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect sequencer to be blocking again:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 12);

      -- End of sequence.
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 2));
   end Test_Nominal_Sequence_Telemetry_Compare;

   overriding procedure Test_Nominal_Sequence_Wait_New_Value (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/waitnew_09.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
   begin
      -- Set the data product time override to make data product older than system time.
      T.Data_Product_Fetch_Time_Override := True;
      T.Data_Product_Fetch_Time_Override_Value := (100, 0);
      T.System_Time := (101, 0);

      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (2);   -- Engine 2

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 9, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Make sure no output commands, we have not yet reached the telemetry compare:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Send a tick to the component so it checks against the value, no commands sent yet,
      -- since this value is not "new".
      T.Component_A_Data_Product_3 := (222, 999);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      -- T.Event_T_Recv_Sync_History.Print;
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Again.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Make the data product "new".
      T.Data_Product_Fetch_Time_Override_Value := T.System_Time;
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 ((Value => 5)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Progress time. Make new data products stale.
      T.Data_Product_Fetch_Time_Override := True;
      T.Data_Product_Fetch_Time_Override_Value := (1000, 0);
      T.System_Time := (1001, 0);

      -- Check the next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_B_Commands.Command_3 ((Value => 7))); -- no timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect sequencer to be blocking again:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Send a tick to the component so it checks against the value, no commands sent yet,
      -- since this value is not "new".
      T.Component_B_Data_Product_2 := (Value => 7.66);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      -- T.Event_T_Recv_Sync_History.Print;
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Again.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Make the data product "new".
      T.Data_Product_Fetch_Time_Override_Value := T.System_Time;
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_3 ((Value => 11)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_B_Commands.Command_3 ((Value => 13))); -- no timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- End of sequence.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 2));
   end Test_Nominal_Sequence_Wait_New_Value;

   overriding procedure Test_Nominal_Fetch_Data_Product (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/tlm_14.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Set data product values:
      T.Component_A_Data_Product_1 := (Value => 17);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 14, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 ((Value => 17)));

      -- Set some variables:
      T.Component_A_Data_Product_1 := (Value => 99);
      T.Component_B_Data_Product_2 := (Value => 3.14);
      T.Component_B_Data_Product_2 := (Value => 9.9);
      T.Component_B_Data_Product_1 := (Value => 22);

      -- OK now send the command response to allow the sequence to continue executing.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we output the second command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_3 ((Value => 99)));

      -- Set some variables:
      T.Component_A_Data_Product_1 := (Value => 2);
      T.Component_B_Data_Product_2 := (Value => 8.76);

      -- OK now send the command response to allow the sequence to continue executing.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we output the third command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_3 ((Value => 99)));

      -- OK now send the command response to allow the sequence to continue executing.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we output the fourth command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_A_Commands.Command_3 ((Value => 99)));

      -- OK now send the command response to allow the sequence to continue executing.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we DID not output the next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Send ticks, we should still not progress.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Set variable correctly to fall through.
      T.Component_B_Data_Product_2 := (Value => 999.89);

      -- Send ticks, we should still not progress.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Set variable correctly to fall through.
      T.Component_A_Data_Product_1 := (Value => 3);

      -- Send ticks, we should now progress.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_A_Commands.Command_3 ((Value => 99)));

      -- OK now send the command response to allow the sequence to continue executing.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we DID not output the next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);

      -- Check events.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_Extraction_Error_History.Get_Count, 1);

      -- Make sure engine is in the error state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);
   end Test_Nominal_Fetch_Data_Product;

   overriding procedure Test_Sequence_Telemetry_Compare_Error (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/waitval_03.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
   begin
      -- Set telemetry:
      T.Component_A_Data_Product_1 := (Value => 1);

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 3, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Make sure no output commands, we have not yet reached the telemetry compare:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Set the data product fetch length override to simulate a telemetry parsing error.
      T.Data_Product_Fetch_Length_Override := True;
      T.Data_Product_Fetch_Length_Override_Value := 1;

      -- Send a tick to the component so it checks against the value.
      T.Component_A_Data_Product_1 := (Value => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Expect error event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_Extraction_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Data_Product_Extraction_Error_History.Get (1), (
         Engine_Id => 2,
         Sequence_Id => 3,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 12, -- This was set by guess and check...
         Error_Type => Telemetry_Fail,
         Errant_Field_Number => 0
      ));

      -- Sequence is dead, so sending more ticks should do nothing.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
   end Test_Sequence_Telemetry_Compare_Error;

   overriding procedure Test_Sequence_Telemetry_Compare_Corner_Cases (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/tlmcases_13.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (2);   -- Engine 2

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 13, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Make sure no output commands, we have not yet reached the telemetry compare:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Send a tick to the component so it checks against the value, no commands sent yet,
      -- since the returned value is not zero.
      T.Component_A_Data_Product_1 := (Value => 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      -- T.Event_T_Recv_Sync_History.Print;
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Send a tick to the component so it checks against the value, no commands sent yet,
      -- since the returned value is not zero.
      T.Component_A_Data_Product_1 := (Value => 99);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Set the telemetry value to return to the passing value:
      T.Component_A_Data_Product_1 := (Value => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- OK, now next telemetry comparison.
      T.Component_A_Data_Product_1 := (Value => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Passes.
      T.Component_A_Data_Product_1 := (Value => 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- OK, now next telemetry comparison.
      T.Component_B_Data_Product_2 := (Value => 1.1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Passes
      T.Component_B_Data_Product_2 := (Value => 100.1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Ok next telemetry comparison = Passes
      T.Component_B_Data_Product_1 := (Value => 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check the command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 ((Value => 5)));
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check the next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_B_Commands.Command_3 ((Value => 7))); -- no timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- OK now let's make sure the timeout variable is set and reset correctly with subsequent waits.
      -- Set value and time to timeout:
      T.Component_B_Data_Product_1 := (Value => 1);
      T.System_Time := (120000, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_B_Commands.Command_3 ((Value => 8))); -- timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);

      -- Another timeout
      T.Component_B_Data_Product_1 := (Value => 1);
      T.System_Time := (120005, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_B_Commands.Command_3 ((Value => 10))); -- timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Not a timeout
      T.Component_A_Data_Product_1 := (Value => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_B_Commands.Command_3 ((Value => 13))); -- no timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
   end Test_Sequence_Telemetry_Compare_Corner_Cases;

   overriding procedure Test_Sequence_Spawn_Invalid_Engine (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/badspawn_10.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 10, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Ok, send the command response.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect spawn to fail since engine is out of range.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Engine_Id_Out_Of_Range_Error_History.Get_Count, 1);
      Engine_Id_Out_Of_Range_Assert.Eq (T.Engine_Id_Out_Of_Range_Error_History.Get (1), (
         Engine_Id => 0,
         Engine_Id_To_Load => 5
      ));

      -- No more commands sent out.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Make sure engine is in the error state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);
   end Test_Sequence_Spawn_Invalid_Engine;

   overriding procedure Test_Sequence_Spawn_Unavailable_Engine (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/busy_11.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 11, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Ok, send the command response.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect spawn to fail since engine is the same as the calling engine.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Execution_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Sequence_Execution_Error_History.Get (1), (
         Engine_Id => 0,
         Sequence_Id => 11,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 37, -- This was set by guess and check...
         Error_Type => Spawn,
         Errant_Field_Number => 0
      ));

      -- Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      -- Natural_Assert.Eq (T.Engine_Unavailable_For_Load_History.Get_Count, 1);
      -- Engine_Id_Out_Of_Range_Assert.Eq (T.Engine_Unavailable_For_Load_History.Get (1), (
      --    Engine_Id => 0,
      --    Engine_Id_To_Load => 0
      -- ));

      -- No more commands sent out.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Make sure engine is in the error state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);

      --
      -- OK, now load sequence into engine 0 and 1. Spawn should still fail in engine 1.
      --

      -- Load engine 0
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 11, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Load engine 1
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 3);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 11, Length => Sequence_Size),
         Engine_Id => 1,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Component_A_Commands.Set_Source_Id (1);   -- Engine 1
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Send command response to engine 1
      T.Command_Response_T_Send ((Source_Id => 1, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Engine_Unavailable_For_Load_History.Get_Count, 1);
      Engine_Id_Out_Of_Range_Assert.Eq (T.Engine_Unavailable_For_Load_History.Get (1), (
         Engine_Id => 1,
         Engine_Id_To_Load => 0
      ));
   end Test_Sequence_Spawn_Unavailable_Engine;

   overriding procedure Test_Sequence_Spawn_Any_Unavailable (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/spawnany_12.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Load_Command : Command.T;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 12, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Ok, send the command response.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Now expect the component to have output another command, the load sequence command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Load_Command := Create_Sequence_Load_Command (Id => 1, Engine_Number => 1, Engine_Request => Specific_Engine);
      Load_Command.Header.Source_Id := 0; -- From engine 0
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

      -- OK, now if we send the command response from the load command it should cause the engine to send out the next command.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Now expect the component to have output another command, the load sequence command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Load_Command := Create_Sequence_Load_Command (Id => 1, Engine_Number => 2, Engine_Request => Specific_Engine);
      Load_Command.Header.Source_Id := 0; -- From engine 0
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Load_Command);

      -- OK, now if we send the command response from the load command it should cause the engine to try to spawn yet another
      -- sequence, which will fail this time, since no engines are available.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check next command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.No_Engine_Available_For_Load_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.No_Engine_Available_For_Load_History.Get (1), (Engine_Id => 0));

      -- Make sure engine is in the error state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);

      -- Engine 1 and 2 should still be reserved by the spawns, let's first send an unexpected ID to load.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No commands should be send out:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Sequence_Id_History.Get_Count, 1);
      Sequence_Id_Error_Assert.Eq (T.Invalid_Sequence_Id_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 12, Length => Sequence_Size),
         Expected_Id => 1
      ));
   end Test_Sequence_Spawn_Any_Unavailable;

   overriding procedure Test_Data_Product_Fetch_Error (Self : in out Instance) is
      use Data_Product_Enums.Fetch_Status;
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/waitval_03.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (2);   -- Engine 2

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Set telemetry:
      T.Component_A_Data_Product_1 := (Value => 1);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 3, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Make sure no output commands, we have not yet reached the telemetry compare:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- Set the telemetry return status to not available:
      T.Data_Product_Fetch_Return_Status := Not_Available;

      -- Send a tick to the component so it checks against the value.
      T.Component_A_Data_Product_1 := (Value => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Again.
      T.Component_A_Data_Product_1 := (Value => 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Progress time and expect a timeout:
      T.System_Time := (120000, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3   ((Value => 5)));

      -- Check the next command:
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_B_Commands.Command_3 ((Value => 6))); -- yes timeout
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect sequencer to be blocking again:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- OK now set the data product return to be out of range:
      T.Data_Product_Fetch_Return_Status := Id_Out_Of_Range;

      -- Send tick.
      T.Component_A_Data_Product_1 := (Value => 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Expect event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_Id_Out_Of_Range_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Data_Product_Id_Out_Of_Range_Error_History.Get (1), (
         Engine_Id => 2,
         Sequence_Id => 3,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 135, -- This was set by guess and check...
         Error_Type => Telemetry_Fail,
         Errant_Field_Number => 0
      ));

      -- Sequence is dead, so sending more ticks should do nothing.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
   end Test_Data_Product_Fetch_Error;

   overriding procedure Test_Relative_And_Absolute_Wait_Sequence (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/waits_02.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (2);   -- Engine 2

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 2, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Make sure no output commands, we have not yet reached absolute time:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- OK now progress time to the absolute time.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      T.System_Time := (119000, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      T.System_Time := (120000, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3   ((Value => 3)));

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Do not expect another command to be output:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Send some ticks, no command, since relative time has not yet passed.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- OK now progress time and expect second command to be sent out.
      T.System_Time := (120001, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_B_Commands.Command_3   ((Value => 5)));

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Progress time and expect next command to be sent out.
      T.System_Time := (120002, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      T.System_Time := (120003, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      T.System_Time := (120005, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- Now a command comes out:
      T.System_Time := (120006, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);

      -- Progress time and expect next command to be sent out.
      T.System_Time := (120009, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);

      -- Absolute time met, command comes out.
      T.System_Time := (120010, 0);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);

      -- Test what happens when time is in past for an absolute wait, expect error state.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Execution_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Sequence_Execution_Error_History.Get (1), (
         Engine_Id => 2,
         Sequence_Id => 2,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 512, -- This was set by guess and check...
         Error_Type => Wait,
         Errant_Field_Number => 0
      ));
   end Test_Relative_And_Absolute_Wait_Sequence;

   overriding procedure Test_Sequence_Load_Error (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer);
      Sequence_Region : Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Header : Sequence_Header.T with Import, Convention => Ada, Address => Sequence_Buffer'Address;
      Crc : constant Crc_16.Crc_16_Type := Header.Crc;
   begin
      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Modify the memory region size so it is one byte too small.
      Sequence_Region.Length := @ - 1;

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Length_Error
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Sequence_Length_History.Get_Count, 1);
      Sequence_Length_Error_Assert.Eq (T.Invalid_Sequence_Length_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => Header
      ));

      -- Make the length smaller than a sequence header;
      Sequence_Region.Length := 1;

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Length_Error
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Sequence_Length_History.Get_Count, 2);
      Sequence_Length_Error_Assert.Eq (T.Invalid_Sequence_Length_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => ([0, 0], 0, 0, 0, 0) -- Header cannot be extracted, too small, so zeros.
      ));

      -- Make the length field in header smaller than a sequence header;
      Sequence_Region.Length := Sequence_Size;
      Header.Length := 1;

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Length_Error
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Sequence_Length_History.Get_Count, 3);
      Sequence_Length_Error_Assert.Eq (T.Invalid_Sequence_Length_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => Header
      ));

      -- Make the CRC bad.
      Header.Length := Sequence_Size;
      Header.Crc := [1, 1];

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 4);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (4), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Crc_Error
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Invalid_Sequence_Crc_History.Get_Count, 1);
      Sequence_Crc_Error_Assert.Eq (T.Invalid_Sequence_Crc_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => Header,
         Computed_Crc => Crc
      ));

      -- Test invalid engine ID:
      Header.Crc := Crc;

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 4, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 5);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (5), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 4, Sequence_Region => Sequence_Region),
         Status => Invalid_Engine_Number
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Load_To_Invalid_Engine_Id_History.Get_Count, 1);
      Sequence_Load_Assert.Eq (T.Load_To_Invalid_Engine_Id_History.Get (1), (Engine_Request => Specific_Engine, Engine_Id => 4, Sequence_Region => Sequence_Region));

      -- Test engine in use error. Load sequence successfully first.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 6);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (6), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => Header,
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- OK now try to load again:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 7);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (7), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Engine_In_Use
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Engine_In_Use_History.Get_Count, 1);
      Sequence_In_Use_Error_Assert.Eq (T.Engine_In_Use_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => Header,
         State => Wait_Command
      ));

      -- Test loading a sequence that calls itself recursively to test engine stack overflow.
      declare
         Sequence_Recursive_Buffer : Basic_Types.Byte_Array (0 .. 99999);
         Sequence_Recursive_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/recurs_subseq_18.bin", Buffer => Sequence_Recursive_Buffer);
         Sequence_Recursive_Region : constant Memory_Region.T := (Address => Sequence_Recursive_Buffer'Address, Length => Sequence_Recursive_Size);
         Load_Command : Command.T;
      begin
         -- Load recursive sequence:
         T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Make sure we got a sequence load return:
         Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 8);
         Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (8), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Status => Success
         ));

         -- Check events:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
         Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
         Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Header => (Crc => Sequence_Recursive_Buffer (0 .. 1), Version => 0, Category => 0, Id => 18, Length => Sequence_Recursive_Size),
            Engine_Id => 1,
            Stack_Level => 0
         ));

         -- Expect load command:
         Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
         Load_Command := Create_Sequence_Load_Command (Id => 18, Engine_Number => 1, Engine_Request => Specific_Engine);
         Load_Command.Header.Source_Id := 1; -- From engine 0
         Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

         -- Load recursive sequence again:
         T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Make sure we got a sequence load return:
         Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 9);
         Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (9), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Status => Success
         ));

         -- Check events:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
         Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 3);
         Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (3), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Header => (Crc => Sequence_Recursive_Buffer (0 .. 1), Version => 0, Category => 0, Id => 18, Length => Sequence_Recursive_Size),
            Engine_Id => 1,
            Stack_Level => 1
         ));

         -- Expect load command:
         Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
         Load_Command := Create_Sequence_Load_Command (Id => 18, Engine_Number => 1, Engine_Request => Specific_Engine);
         Load_Command.Header.Source_Id := 1; -- From engine 0
         Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Load_Command);

         -- Load recursive sequence again:
         T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Make sure we got a sequence load return:
         Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 10);
         Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (10), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Status => Success
         ));

         -- Check events:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
         Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 4);
         Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (4), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Header => (Crc => Sequence_Recursive_Buffer (0 .. 1), Version => 0, Category => 0, Id => 18, Length => Sequence_Recursive_Size),
            Engine_Id => 1,
            Stack_Level => 2
         ));

         -- Expect load command:
         Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
         Load_Command := Create_Sequence_Load_Command (Id => 18, Engine_Number => 1, Engine_Request => Specific_Engine);
         Load_Command.Header.Source_Id := 1; -- From engine 0
         Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Load_Command);

         -- Load recursive sequence again:
         T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Make sure we got a sequence load return:
         Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 11);
         Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (11), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Status => Load_Error
         ));

         -- Check events:
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 11);
         Natural_Assert.Eq (T.Sequence_Load_Error_History.Get_Count, 1);
         Sequence_Load_Error_Info_Assert.Eq (T.Sequence_Load_Error_History.Get (1), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Recursive_Region),
            Header => (Crc => Sequence_Recursive_Buffer (0 .. 1), Version => 0, Category => 0, Id => 18, Length => Sequence_Recursive_Size),
            Stack_Level => 2,
            State => Error,
            Sequence_Error_Code => Load
         ));
      end;

      -- Load to any engine, until then engines are full then expect error.
      T.Sequence_Load_T_Send ((Engine_Request => Any_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 12);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (12), (
         Load => (Engine_Request => Any_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 12);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 5);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (5), (
         Load => (Engine_Request => Any_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => Header,
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Fill er up.
      T.Sequence_Load_T_Send ((Engine_Request => Any_Engine, Engine_Id => 99, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 13);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (13), (
         Load => (Engine_Request => Any_Engine, Engine_Id => 99, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 6);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (6), (
         Load => (Engine_Request => Any_Engine, Engine_Id => 99, Sequence_Region => Sequence_Region),
         Header => Header,
         Engine_Id => 1,
         Stack_Level => 0
      ));

      -- OK all engines full. Expect error:
      T.Sequence_Load_T_Send ((Engine_Request => Any_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 14);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (14), (
         Load => (Engine_Request => Any_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Status => Engine_In_Use
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 14);
      Natural_Assert.Eq (T.No_Engine_Available_History.Get_Count, 1);
      Sequence_Load_Assert.Eq (T.No_Engine_Available_History.Get (1), (Engine_Request => Any_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));

      -- Load to uninitialized engine.
      -- OK now let's reinitialize the component
      T.Component_Instance.Final;
      -- Call component init here and initialize engines again.
      T.Component_Instance.Init (
         Num_Engines => 3,
         Stack_Size => 3,
         Create_Sequence_Load_Command_Function => Create_Sequence_Load_Command'Access,
         Packet_Period => 1,
         Continue_On_Command_Failure => False,
         Timeout_Limit => 3,
         Instruction_Limit => 10000
      );

      -- OK all engines are uninitialized. Let's try to load a sequence into one.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 15);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (15), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Status => Engine_Uninitialized
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 15);
      Natural_Assert.Eq (T.Load_To_Uninitialized_Engine_History.Get_Count, 1);
      Sequence_Load_Assert.Eq (T.Load_To_Uninitialized_Engine_History.Get (1), (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
   end Test_Sequence_Load_Error;

   overriding procedure Test_Sequence_Execution_Error (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- Send the command response to the sequencer. Make the source_id unexpected.
      T.Command_Response_T_Send ((Source_Id => 4, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Unexpected_Command_Response_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Unexpected_Command_Response_History.Get (1), (Source_Id => 4, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));

      -- No more commands should have been sent out from sequence.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Send command response to the sequencer. Make command ID unexpected.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events. The component should ignore this case.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      -- TODO - delete this event in component?
      -- Natural_Assert.Eq (T.Unexpected_Command_Response_Id_History.Get_Count, 1);
      -- Unexpected_Command_Response_Info_Assert.Eq (T.Unexpected_Command_Response_Id_History.Get (1), (
      --    Response => (Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success),
      --    Last_Sent_Command_Id => Component_A_Commands.Get_Command_1_Id
      -- ));

      -- No more commands should have been sent out from sequence.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Send a command response that indicates the command failed.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Failure));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Sequence_Command_Failure_History.Get_Count, 1);
      Command_Fail_Error_Type_Assert.Eq (T.Sequence_Command_Failure_History.Get (1), (
         Response => (Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Failure),
         Error_Report => (
            Engine_Id => 0,
            Sequence_Id => 1,
            Engine_State => Active,
            Sequence_State => Wait_Command,
            Stack_Level => 0,
            Program_Counter => 25, -- This was set by guess and check...
            Error_Type => None,
            Errant_Field_Number => 0
         )
      ));

      -- No more commands should have been sent out from sequence.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Make sure engine is in the error state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);

      -- OK, now send another command response. This should do nothing except send out an event,
      -- the component should not keep executing because is in an error state.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Failure));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events, expect one:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Unexpected_Command_Response_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Unexpected_Command_Response_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Failure));
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- OK now let's send an extra command source registration request. This should be rejected by the component
      -- since we registered all the engines in the fixture for the unit tests.
      T.Command_Response_T_Send ((
         Source_Id => 1,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Unexpected_Register_Source_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Unexpected_Register_Source_History.Get (1), (
         Source_Id => 1,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- OK now let's reinitialize the component and put it in continue through command error
      -- mode and make sure that works correctly.
      T.Component_Instance.Final;
      -- Call component init here and initialize engines again.
      T.Component_Instance.Init (
         Num_Engines => 3,
         Stack_Size => 3,
         Create_Sequence_Load_Command_Function => Create_Sequence_Load_Command'Access,
         Packet_Period => 1,
         Continue_On_Command_Failure => True,
         Timeout_Limit => 3,
         Instruction_Limit => 10000
      );
      T.Command_Response_T_Send ((
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);

      -- Load a simple sequence, again:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_1);

      -- Send the command response to the sequencer. Make the source_id unexpected.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Failure));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events, expect reported failure.:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Sequence_Command_Failure_History.Get_Count, 2);
      Command_Fail_Error_Type_Assert.Eq (T.Sequence_Command_Failure_History.Get (2), (
         Response => (Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Failure),
         Error_Report => (
            Engine_Id => 0,
            Sequence_Id => 1,
            Engine_State => Active,
            Sequence_State => Wait_Command,
            Stack_Level => 0,
            Program_Counter => 25, -- This was set by guess and check...
            Error_Type => None,
            Errant_Field_Number => 0
         )
      ));

      -- Since we continue on errors now, a new command should have been sent out.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_2 ((Seconds => 11, Subseconds => 15)));
   end Test_Sequence_Execution_Error;

   overriding procedure Test_Sequence_Timeouts (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- Instead of sending a command response, let's time out the sequence waiting on a response.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Timeout_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Sequence_Timeout_Error_History.Get (1), (
         Engine_Id => 0,
         Sequence_Id => 1,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 25, -- This was set by guess and check...
         Error_Type => Command_Timeout,
         Errant_Field_Number => 0
      ));

      -- Next tick should report nothing extra.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Now let's send the command response, late. We should stay in the error state.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check no commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Unexpected_Command_Response_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Unexpected_Command_Response_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));

      -- OK now let's reinitialize the component and put it in continue through command error
      -- mode and make sure that works correctly.
      T.Component_Instance.Final;
      -- Call component init here and initialize engines again.
      T.Component_Instance.Init (
         Num_Engines => 3,
         Stack_Size => 3,
         Create_Sequence_Load_Command_Function => Create_Sequence_Load_Command'Access,
         Packet_Period => 1,
         Continue_On_Command_Failure => True,
         Timeout_Limit => 3,
         Instruction_Limit => 10000
      );
      T.Command_Response_T_Send ((
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);

      -- Load a simple sequence, again:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_1);

      -- Instead of sending a command response, let's time out the sequence waiting on a response.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Sequence_Timeout_Error_History.Get_Count, 2);
      Engine_Error_Type_Assert.Eq (T.Sequence_Timeout_Error_History.Get (2), (
         Engine_Id => 0,
         Sequence_Id => 1,
         Engine_State => Active,
         Sequence_State => Wait_Command,
         Stack_Level => 0,
         Program_Counter => 25, -- This was set by guess and check...
         Error_Type => None,
         Errant_Field_Number => 0
      ));

      -- Since we continue on errors now, a new command should have been sent out.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_2 ((Seconds => 11, Subseconds => 15)));
   end Test_Sequence_Timeouts;

   overriding procedure Test_Issue_Details_Packet (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Buffer : Packet_Types.Packet_Buffer_Type := [others => 0];
   begin
      -- Send the command to issue the details.
      T.Command_T_Send (T.Commands.Issue_Details_Packet ((Engine_Id => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Issue_Details_Packet_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Details_Packet_Sent_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Details_Packet_Sent_History.Get (1), (Engine_Id => 0));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Details_Packet_History.Get_Count, 1);

      -- Check contents of details packet.
      Buffer (0 .. Engine_Details_Type.Size_In_Bytes + Sequence_Details_Type.Size_In_Bytes * 3 - 1) :=
         Engine_Details_Type.Serialization.To_Byte_Array ((
            Engine_Id => 0,
            Source_Id => 0,
            Engine_State => Inactive,
            Last_Command_Id_Sent => 0,
            Engine_Command_Send_Counter => 0,
            Engine_Command_Error_Counter => 0,
            Sequence_Error_Code => None,
            Wakeup_Time => 0,
            Stack_Level => 0
         )) &
         Sequence_Details_Type.Serialization.To_Byte_Array ((
            Header => (
               Crc => [0, 0],
               Version => 0,
               Category => 0,
               Id => 0,
               Length => 0
            ),
            Sequence_State => Unloaded,
            Sequence_Address => (Address => System.Null_Address),
            Program_Counter => 0,
            Start_Time => 0,
            Last_Executed_Time => 0
         )) &
         Sequence_Details_Type.Serialization.To_Byte_Array ((
            Header => (
               Crc => [0, 0],
               Version => 0,
               Category => 0,
               Id => 0,
               Length => 0
            ),
            Sequence_State => Unloaded,
            Sequence_Address => (Address => System.Null_Address),
            Program_Counter => 0,
            Start_Time => 0,
            Last_Executed_Time => 0
         )) &
         Sequence_Details_Type.Serialization.To_Byte_Array ((
            Header => (
               Crc => [0, 0],
               Version => 0,
               Category => 0,
               Id => 0,
               Length => 0
            ),
            Sequence_State => Unloaded,
            Sequence_Address => (Address => System.Null_Address),
            Program_Counter => 0,
            Start_Time => 0,
            Last_Executed_Time => 0
         ));
      Packet_Assert.Eq (T.Details_Packet_History.Get (1), (
         Header => (
            Time => T.System_Time,
            Id => T.Packets.Get_Details_Packet_Id,
            Sequence_Count => 0,
            Buffer_Length => Engine_Details_Type.Size_In_Bytes + Sequence_Details_Type.Size_In_Bytes * 3
         ),
         Buffer => Buffer
      ));

      -- This a copy of a portion of Test_Nominal_Subsequence_Load to make the engine details more
      -- interesting.
      declare
         Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
         Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/call_06.bin", Buffer => Sequence_Buffer);
         Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
         Sequence_Buffer_2 : Basic_Types.Byte_Array (0 .. 99999);
         Sequence_Size_2 : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/subseq_07.bin", Buffer => Sequence_Buffer_2);
         Sequence_Region_2 : constant Memory_Region.T := (Address => Sequence_Buffer_2'Address, Length => Sequence_Size_2);
         Component_A_Commands : Test_Component_Commands.Instance;
         Component_B_Commands : Test_Component_Commands.Instance;
         Load_Command : Command.T;
      begin
         -- Set nonzero system time to make sure it shows up in packet.
         T.System_Time := (12, 11);

         -- Setup commands for component A and B:
         Component_A_Commands.Set_Id_Base (1);
         Component_A_Commands.Set_Source_Id (0);   -- Engine 0
         Component_B_Commands.Set_Id_Base (7);
         Component_B_Commands.Set_Source_Id (0);   -- Engine 0

         -- Load a simple sequence:
         T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Make sure we got a sequence load return:
         Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
         Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
            Status => Success
         ));

         -- Make sure we output the first command:
         Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
         Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

         -- Ok, send the command response.
         T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Now expect the component to have output another command, the load sequence command:
         Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
         Load_Command := Create_Sequence_Load_Command (Id => 7, Engine_Number => 0, Engine_Request => Specific_Engine);
         Load_Command.Header.Source_Id := 0; -- From engine 0
         Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

         -- OK, now if we send the command response from the load command it should NOT cause the engine to send out the next command.
         T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => 59, Status => Success));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Expect nothing. Engine should remain static until subsequence load.
         Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

         -- OK, now load the valid subsequence:
         T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region_2));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Make sure we got a sequence load return:
         Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
         Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
            Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region_2),
            Status => Success
         ));

         -- Make sure we output the first command of subsequence:
         Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
         Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_B_Commands.Command_2 ((Seconds => 10, Subseconds => 9)));

         ---------------------------------------

         -- Send the command to issue the details.
         T.Command_T_Send (T.Commands.Issue_Details_Packet ((Engine_Id => 0)));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
         Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Issue_Details_Packet_Id, Status => Success));

         -- Check packet:
         Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
         Natural_Assert.Eq (T.Details_Packet_History.Get_Count, 2);

         -- Check contents of details packet.
         Buffer (0 .. Engine_Details_Type.Size_In_Bytes + Sequence_Details_Type.Size_In_Bytes * 3 - 1) :=
            Engine_Details_Type.Serialization.To_Byte_Array ((
               Engine_Id => 0,
               Source_Id => 0,
               Engine_State => Active,
               Last_Command_Id_Sent => Component_B_Commands.Get_Command_2_Id,
               Engine_Command_Send_Counter => 2,
               Engine_Command_Error_Counter => 0,
               Sequence_Error_Code => None,
               Wakeup_Time => 0,
               Stack_Level => 1
            )) &
            Sequence_Details_Type.Serialization.To_Byte_Array ((
               Header => (
                  Crc => Sequence_Buffer (0 .. 1),
                  Version => 16#00000000#,
                  Category => 0,
                  Id => 6,
                  Length => Sequence_Size
               ),
               Sequence_State => Wait_Load_New_Sub_Seq,
               Sequence_Address => (Address => Sequence_Region.Address),
               Program_Counter => 69,
               Start_Time => 12,
               Last_Executed_Time => 12
            )) &
            Sequence_Details_Type.Serialization.To_Byte_Array ((
               Header => (
                  Crc => Sequence_Buffer_2 (0 .. 1),
                  Version => 16#00000000#,
                  Category => 0,
                  Id => 7,
                  Length => Sequence_Size_2
               ),
               Sequence_State => Wait_Command,
               Sequence_Address => (Address => Sequence_Region_2.Address),
               Program_Counter => 73,
               Start_Time => 12,
               Last_Executed_Time => 12
            )) &
            Sequence_Details_Type.Serialization.To_Byte_Array ((
               Header => (
                  Crc => [0, 0],
                  Version => 0,
                  Category => 0,
                  Id => 0,
                  Length => 0
               ),
               Sequence_State => Unloaded,
               Sequence_Address => (Address => System.Null_Address),
               Program_Counter => 0,
               Start_Time => 0,
               Last_Executed_Time => 0
            ));
         Packet_Assert.Eq (T.Details_Packet_History.Get (2), (
            Header => (
               Time => T.System_Time,
               Id => T.Packets.Get_Details_Packet_Id,
               Sequence_Count => 1,
               Buffer_Length => Engine_Details_Type.Size_In_Bytes + Sequence_Details_Type.Size_In_Bytes * 3
            ),
            Buffer => Buffer
         ));

         -- Send tick and expect the summary packet. We do this here to make sure it is filled in correctly
         -- with the same info from above.
         T.Tick_T_Send (((0, 0), 0));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Summary_Packet_History.Get_Count, 1);

         -- Check contents of summary packet.
         Buffer (0 .. Engine_Summary_Type.Size_In_Bytes * 3 - 1) :=
            Engine_Summary_Type.Serialization.To_Byte_Array ((
               Engine_State => Active,
               Sequence_State => Wait_Command,
               Sequence_Error_Code => None,
               Stack_Level => 1,
               Parent_Sequence_Id => 6,
               Parent_Program_Counter => 69,
               Lowest_Child_Id => 7,
               Lowest_Child_Program_Counter => 73,
               Wakeup_Time => 0,
               Command_Error_Counter => 0
            )) &
            Engine_Summary_Type.Serialization.To_Byte_Array ((
               Engine_State => Inactive,
               Sequence_State => Unloaded,
               Sequence_Error_Code => None,
               Stack_Level => 0,
               Parent_Sequence_Id => 0,
               Parent_Program_Counter => 0,
               Lowest_Child_Id => 0,
               Lowest_Child_Program_Counter => 0,
               Wakeup_Time => 0,
               Command_Error_Counter => 0
            )) &
            Engine_Summary_Type.Serialization.To_Byte_Array ((
               Engine_State => Inactive,
               Sequence_State => Unloaded,
               Sequence_Error_Code => None,
               Stack_Level => 0,
               Parent_Sequence_Id => 0,
               Parent_Program_Counter => 0,
               Lowest_Child_Id => 0,
               Lowest_Child_Program_Counter => 0,
               Wakeup_Time => 0,
               Command_Error_Counter => 0
            ));
         Packet_Assert.Eq (T.Summary_Packet_History.Get (1), (
            Header => (
               Time => T.System_Time,
               Id => T.Packets.Get_Summary_Packet_Id,
               Sequence_Count => 0,
               Buffer_Length => Engine_Summary_Type.Size_In_Bytes * 3
            ),
            Buffer => Buffer
         ));
      end;
   end Test_Issue_Details_Packet;

   overriding procedure Test_Set_Summary_Packet_Period (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Buffer : Packet_Types.Packet_Buffer_Type := [others => 0];
   begin
      -- Only setup data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      -- Send some ticks and expect the summary packet every time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Summary_Packet_History.Get_Count, 1);

      -- Check contents of summary packet.
      Buffer (0 .. Engine_Summary_Type.Size_In_Bytes * 3 - 1) :=
         Engine_Summary_Type.Serialization.To_Byte_Array ((
            Engine_State => Inactive,
            Sequence_State => Unloaded,
            Sequence_Error_Code => None,
            Stack_Level => 0,
            Parent_Sequence_Id => 0,
            Parent_Program_Counter => 0,
            Lowest_Child_Id => 0,
            Lowest_Child_Program_Counter => 0,
            Wakeup_Time => 0,
            Command_Error_Counter => 0
         )) &
         Engine_Summary_Type.Serialization.To_Byte_Array ((
            Engine_State => Inactive,
            Sequence_State => Unloaded,
            Sequence_Error_Code => None,
            Stack_Level => 0,
            Parent_Sequence_Id => 0,
            Parent_Program_Counter => 0,
            Lowest_Child_Id => 0,
            Lowest_Child_Program_Counter => 0,
            Wakeup_Time => 0,
            Command_Error_Counter => 0
         )) &
         Engine_Summary_Type.Serialization.To_Byte_Array ((
            Engine_State => Inactive,
            Sequence_State => Unloaded,
            Sequence_Error_Code => None,
            Stack_Level => 0,
            Parent_Sequence_Id => 0,
            Parent_Program_Counter => 0,
            Lowest_Child_Id => 0,
            Lowest_Child_Program_Counter => 0,
            Wakeup_Time => 0,
            Command_Error_Counter => 0
         ));
      Packet_Assert.Eq (T.Summary_Packet_History.Get (1), (
         Header => (
            Time => T.System_Time,
            Id => T.Packets.Get_Summary_Packet_Id,
            Sequence_Count => 0,
            Buffer_Length => Engine_Summary_Type.Size_In_Bytes * 3
         ),
         Buffer => Buffer
      ));

      -- Send some ticks and expect the summary packet every time:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Summary_Packet_History.Get_Count, 2);

      -- Check contents of summary packet.
      Packet_Assert.Eq (T.Summary_Packet_History.Get (2), (
         Header => (
            Time => T.System_Time,
            Id => T.Packets.Get_Summary_Packet_Id,
            Sequence_Count => 1,
            Buffer_Length => Engine_Summary_Type.Size_In_Bytes * 3
         ),
         Buffer => Buffer
      ));

      -- OK send command to turn off summary packet:
      T.Command_T_Send (T.Commands.Set_Summary_Packet_Period ((Value => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Summary_Packet_Period_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Summary_Packet_Period_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Summary_Packet_Period_History.Get (1), (Value => 1));
      Packed_U16_Assert.Eq (T.Summary_Packet_Period_History.Get (2), (Value => 0));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Summary_Packet_Period_Set_History.Get_Count, 1);
      Packed_U16_Assert.Eq (T.Summary_Packet_Period_Set_History.Get (1), (Value => 0));

      -- OK now no packets should come out:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- OK send command to turn on summary packet at rate of 3:
      T.Command_T_Send (T.Commands.Set_Summary_Packet_Period ((Value => 3)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Summary_Packet_Period_Id, Status => Success));

      -- Check data products:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Summary_Packet_Period_History.Get_Count, 3);
      Packed_U16_Assert.Eq (T.Summary_Packet_Period_History.Get (3), (Value => 3));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Summary_Packet_Period_Set_History.Get_Count, 2);
      Packed_U16_Assert.Eq (T.Summary_Packet_Period_Set_History.Get (2), (Value => 3));

      -- Send some ticks:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);

      -- Check contents of summary packet.
      Packet_Assert.Eq (T.Summary_Packet_History.Get (3), (
         Header => (
            Time => T.System_Time,
            Id => T.Packets.Get_Summary_Packet_Id,
            Sequence_Count => 2,
            Buffer_Length => Engine_Summary_Type.Size_In_Bytes * 3
         ),
         Buffer => Buffer
      ));

      -- Send some ticks:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 3);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 4);

      -- Check contents of summary packet.
      Packet_Assert.Eq (T.Summary_Packet_History.Get (4), (
         Header => (
            Time => T.System_Time,
            Id => T.Packets.Get_Summary_Packet_Id,
            Sequence_Count => 3,
            Buffer_Length => Engine_Summary_Type.Size_In_Bytes * 3
         ),
         Buffer => Buffer
      ));
   end Test_Set_Summary_Packet_Period;

   overriding procedure Test_Command_Invalid_Engine (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send the command to issue the details packet to an invalid engine ID.
      T.Command_T_Send (T.Commands.Issue_Details_Packet ((Engine_Id => 4)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Issue_Details_Packet_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Engine_Id_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Invalid_Engine_Id_History.Get (1), (Engine_Id => 4));

      -- Check packet:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send command to kill an engine with an invalid engine ID.
      T.Command_T_Send (T.Commands.Kill_Engine ((Engine_Id => 99)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Kill_Engine_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Invalid_Engine_Id_History.Get_Count, 2);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Invalid_Engine_Id_History.Get (2), (Engine_Id => 99));
   end Test_Command_Invalid_Engine;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Set_Summary_Packet_Period ((Value => 0));
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 0;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Summary_Packet_Period_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Set_Summary_Packet_Period_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 0]));
   end Test_Invalid_Command;

   overriding procedure Test_Queue_Overflow (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create maximum size command to fill queue.
      Cmd : Command.T;
   begin
      -- Send 3 commands to fill up queue.
      Cmd.Header.Arg_Buffer_Length := Cmd.Arg_Buffer'Length;
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);

      -- OK the next command should overflow the queue.
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (Cmd);

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dropped_Command_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Dropped_Command_History.Get (1), Cmd.Header);

      -- OK now send a command response and expect it to be dropped:
      T.Expect_Command_Response_T_Send_Dropped := True;
      T.Command_Response_T_Send ((
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => 15,
         Status => Success
      ));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dropped_Command_Response_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Dropped_Command_Response_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => 15,
         Status => Success
      ));

      -- OK now send a tick and expect it to be dropped:
      T.Expect_Tick_T_Send_Dropped := True;
      T.Tick_T_Send (((0, 0), 1));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dropped_Tick_History.Get_Count, 1);
      Tick_Assert.Eq (T.Dropped_Tick_History.Get (1), ((0, 0), 1));

      -- OK now send a sequence load.
      T.Expect_Sequence_Load_T_Send_Dropped := True;
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => (Address => T.Expect_Sequence_Load_T_Send_Dropped'Address, Length => 0)));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dropped_Sequence_Load_History.Get_Count, 1);
      Sequence_Load_Assert.Eq (T.Dropped_Sequence_Load_History.Get (1), (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => (Address => T.Expect_Sequence_Load_T_Send_Dropped'Address, Length => 0)));

      -- Make sure sequence load return sent:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => (Address => T.Expect_Sequence_Load_T_Send_Dropped'Address, Length => 0)),
         Status => Dropped
      ));
   end Test_Queue_Overflow;

   overriding procedure Test_Sequence_Internal_Execution_Error (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- OK, now corrupt the sequence to that it errors internally.
      Sequence_Buffer (12 .. 99999) := [others => 255];

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No more commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Execution_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Sequence_Execution_Error_History.Get (1), (
         Engine_Id => 0,
         Sequence_Id => 1,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 25, -- This was set by guess and check...
         Error_Type => Opcode,
         Errant_Field_Number => 0
      ));

      -- Make sure engine is in the error state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);

      -- Send tick, nothing should happen:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Make sure engine is in the error state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
   end Test_Sequence_Internal_Execution_Error;

   -- This unit test tests the kill engine command.
   overriding procedure Test_Kill_Engine_Command (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      ---------------------------------------------
      -- Test killing single engine:
      ---------------------------------------------

      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- Make sure engine is in the active state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Active);

      -- OK now send the kill command to kill the running sequence:
      T.Command_T_Send (T.Commands.Kill_Engine ((Engine_Id => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Kill_Engine_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Killed_Engine_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Killed_Engine_History.Get (1), (Engine_Id => 0));

      -- Make sure engine is in the inactive state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Inactive);

      ---------------------------------------------
      -- Test killing invalid engine:
      ---------------------------------------------

      -- OK now send the kill command to kill non existent engine:
      T.Command_T_Send (T.Commands.Kill_Engine ((Engine_Id => 3)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Kill_Engine_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Invalid_Engine_Id_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Invalid_Engine_Id_History.Get (1), (Engine_Id => 3));

      ---------------------------------------------
      -- Test killing all engines:
      ---------------------------------------------

      -- Load a simple sequence into engine 0:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_1);

      -- Make sure engine is in the active state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Active);

      -- Load a simple sequence into engine 2:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 3);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_1);

      -- Make sure engine is in the active state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (2), Active);

      -- Send command to kill all engines:
      -- OK now send the kill command to kill the running sequence:
      T.Command_T_Send (T.Commands.Kill_All_Engines);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Kill_All_Engines_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Killed_All_Engines_History.Get_Count, 1);

      -- Make sure engines are in the inactive state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (1), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (2), Inactive);

      -- Send command to kill an engine that is not running anything.
      T.Command_T_Send (T.Commands.Kill_Engine ((Engine_Id => 0)));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Kill_Engine_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Killed_Engine_History.Get_Count, 2);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Killed_Engine_History.Get (2), (Engine_Id => 0));

      -- Send command to kill all engines that are not running anything.
      T.Command_T_Send (T.Commands.Kill_All_Engines);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Kill_All_Engines_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Killed_All_Engines_History.Get_Count, 2);
   end Test_Kill_Engine_Command;

   -- This unit test tests the kill engine sequence opcode.
   overriding procedure Test_Kill_Opcode (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/commands_01.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Kill_Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Kill_Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/kill_15.bin", Buffer => Kill_Sequence_Buffer);
      Kill_Sequence_Region : constant Memory_Region.T := (Address => Kill_Sequence_Buffer'Address, Length => Kill_Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      ---------------------------------------------
      -- Start sequences in engines 0 and 1
      ---------------------------------------------

      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Check output commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_1);

      -- Make sure engine is in the active state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Active);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 1, Length => Sequence_Size),
         Engine_Id => 1,
         Stack_Level => 0
      ));

      -- Check output commands:
      Component_A_Commands.Set_Source_Id (1);   -- Engine 1
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_1);

      -- Make sure engine is in the active state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (1), Active);

      ---------------------------------------------
      -- Load and run kill sequence in engine 2
      ---------------------------------------------
      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Kill_Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Kill_Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 3);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Kill_Sequence_Region),
         Header => (Crc => Kill_Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 15, Length => Kill_Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));
      Natural_Assert.Eq (T.Engines_Killed_History.Get_Count, 1);
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (1), (
         Executing_Engine => 2, First_Engine => 0, Num_Engines => 2
      ));

      -- Check output commands:
      Component_A_Commands.Set_Source_Id (2);   -- Engine 2
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_1);

      -- Make sure engines 0 and 1 are in inactive state.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (1), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (2), Active);

      -- Send the command response to the sequencer:
      T.Command_Response_T_Send ((Source_Id => 2, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_1_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect invalid kill opcode to be run, check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Invalid_Engine_Kill_Range_History.Get_Count, 1);
      Packed_Engine_Kill_Params_Assert.Eq (T.Invalid_Engine_Kill_Range_History.Get (1), (
         Executing_Engine => 2, First_Engine => 2, Num_Engines => 2
      ));

      -- Make sure engines 0 and 1 are in inactive state and 2 in error.
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (1), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (2), Engine_Error);

      -- No more commands:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
   end Test_Kill_Opcode;

   -- This unit test tests the sequence recursion error, where the execute recursion limit is exceeded.
   overriding procedure Test_Recursion_Error (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/recursion_16.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
   begin
      ---------------------------------------------
      -- Start sequences in engines 0 and 1
      ---------------------------------------------

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 13);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 16, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));
      Natural_Assert.Eq (T.Engines_Killed_History.Get_Count, 11);
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (1), (
         Executing_Engine => 0, First_Engine => 1, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (2), (
         Executing_Engine => 0, First_Engine => 1, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (3), (
         Executing_Engine => 0, First_Engine => 2, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (4), (
         Executing_Engine => 0, First_Engine => 2, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (5), (
         Executing_Engine => 0, First_Engine => 1, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (6), (
         Executing_Engine => 0, First_Engine => 1, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (7), (
         Executing_Engine => 0, First_Engine => 2, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (8), (
         Executing_Engine => 0, First_Engine => 2, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (9), (
         Executing_Engine => 0, First_Engine => 1, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (10), (
         Executing_Engine => 0, First_Engine => 2, Num_Engines => 1
      ));
      Packed_Engine_Kill_Params_Assert.Eq (T.Engines_Killed_History.Get (11), (
         Executing_Engine => 0, First_Engine => 2, Num_Engines => 1
      ));
      Natural_Assert.Eq (T.Execute_Recursion_Limit_Exceeded_History.Get_Count, 1);

      -- Check engine states:
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Engine_Error);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (1), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (2), Inactive);
   end Test_Recursion_Error;

   -- This unit test tests the sequencer print opcode which produces an event from the command sequencer.
   overriding procedure Test_Print (Self : in out Instance) is
      use Seq_Enums.Seq_Print_Type;
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/print_17.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;

      function Create_Print_String (Str : in String) return Seq_Print_Event_Record.Print_String_Type is
         To_Return : Seq_Print_Event_Record.Print_String_Type := [others => 0];
         pragma Warnings (Off, "overlay changes scalar storage order");
         Overlay : Basic_Types.Byte_Array (1 .. Str'Length) with Import, Convention => Ada, Address => Str'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         Byte_Array_Util.Safe_Left_Copy (Dest => To_Return, Src => Overlay);
         return To_Return;
      end Create_Print_String;
   begin
      ---------------------------------------------
      -- Start sequences in engines 0 and 1
      ---------------------------------------------

      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 17, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));
      Natural_Assert.Eq (T.Print_History.Get_Count, 7);
      Seq_Print_Event_Record_Assert.Eq (T.Print_History.Get (1), (
         Header => (
            Engine_Id => 0,
            Sequence_Id => 17,
            Print_Type => Info
         ),
         Print_String => Create_Print_String ("This is my informational statement")
      ));
      Seq_Print_Event_Record_Assert.Eq (T.Print_History.Get (2), (
         Header => (
            Engine_Id => 0,
            Sequence_Id => 17,
            Print_Type => Error
         ),
         Print_String => Create_Print_String ("Bad thing is happening!")
      ));
      Seq_Print_Event_Record_Assert.Eq (T.Print_History.Get (3), (
         Header => (
            Engine_Id => 0,
            Sequence_Id => 17,
            Print_Type => Debug
         ),
         Print_String => Create_Print_String ("The value of 2 + 2 is 4")
      ));
      Seq_Print_Event_Record_Assert.Eq (T.Print_History.Get (4), (
         Header => (
            Engine_Id => 0,
            Sequence_Id => 17,
            Print_Type => Critical
         ),
         Print_String => Create_Print_String ("Burn it all down")
      ));
      Seq_Print_Event_Record_Assert.Eq (T.Print_History.Get (5), (
         Header => (
            Engine_Id => 0,
            Sequence_Id => 17,
            Print_Type => Info
         ),
         Print_String => Create_Print_String (" 1234")
      ));
      Seq_Print_Event_Record_Assert.Eq (T.Print_History.Get (6), (
         Header => (
            Engine_Id => 0,
            Sequence_Id => 17,
            Print_Type => Debug
         ),
         Print_String => Create_Print_String ("-1234")
      ));
      Seq_Print_Event_Record_Assert.Eq (T.Print_History.Get (7), (
         Header => (
            Engine_Id => 0,
            Sequence_Id => 17,
            Print_Type => Critical
         ),
         Print_String => Create_Print_String ("-1.35000E+01")
      ));
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));

      -- Check engine states:
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (0), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (1), Inactive);
      Seq_Engine_State_Assert.Eq (T.Get_Engine_State (2), Inactive);
   end Test_Print;

   -- This unit test tests the set engine arguments command.
   overriding procedure Test_Set_Engine_Arguments (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/lots_args_19.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- Send the command to set arguments details.
      T.Command_T_Send (T.Commands.Set_Engine_Arguments ((
         Engine_Id => 0,
         Argument_01 => 0,
         Argument_02 => 0,
         Argument_03 => 0,
         Argument_04 => 0,
         Argument_05 => 0,
         Argument_06 => 0,
         Argument_07 => 0,
         Argument_08 => 0,
         Argument_09 => 0,
         Argument_10 => 0,
         Argument_11 => 0,
         Argument_12 => 0,
         Argument_13 => 0,
         Argument_14 => 0,
         Argument_15 => 0,
         Argument_16 => 0
      )));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Engine_Arguments_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Loaded_Engine_Arguments_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Loaded_Engine_Arguments_History.Get (1), (Engine_Id => 0));

      -- OK now run the sequence in engine 0 to test arguments.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events and expect sequence to have errored:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 19, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));
      Natural_Assert.Eq (T.Sequence_Execution_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Sequence_Execution_Error_History.Get (1), (
         Engine_Id => 0,
         Sequence_Id => 19,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 68, -- This was set by guess and check...
         Error_Type => Eval,
         Errant_Field_Number => 0
      ));

      -- No command output
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- OK, the arguments were not correct. Let's try again, but this time with correct arguments
      -- so the sequence does not fail. All arguments must be converted to unsigned integer representation.
      T.Command_T_Send (T.Commands.Set_Engine_Arguments ((
         Engine_Id => 0,
         Argument_01 => 1, --   1
         Argument_02 => 2, --   2
         Argument_03 => 3, --   3
         Argument_04 => 4294967292, -- -4
         Argument_05 => 5, --   5
         Argument_06 => 4294967290, -- -6
         Argument_07 => 16#3f8ccccd#, --   1.1
         Argument_08 => 16#bf99999a#, -- -1.2
         Argument_09 => 16#3fc00000#, --   1.5
         Argument_10 => 16#bfcccccd#, -- -1.6
         Argument_11 => 7, --   7
         Argument_12 => 8, --   8
         Argument_13 => 9, --   9
         Argument_14 => 10, -- 10
         Argument_15 => 11, -- 11
         Argument_16 => 12   -- 12
      )));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Engine_Arguments_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Loaded_Engine_Arguments_History.Get_Count, 2);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Loaded_Engine_Arguments_History.Get (2), (Engine_Id => 0));

      -- OK now run the sequence in engine 0 to test arguments.
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events and expect sequence to have errored:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 19, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Expect a command to be output:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 ((Value => 19)));

      -- OK, now try the error cases. First try to run the command on an engine that is out of range.
      T.Command_T_Send (T.Commands.Set_Engine_Arguments ((
         Engine_Id => 4,
         Argument_01 => 0,
         Argument_02 => 0,
         Argument_03 => 0,
         Argument_04 => 0,
         Argument_05 => 0,
         Argument_06 => 0,
         Argument_07 => 0,
         Argument_08 => 0,
         Argument_09 => 0,
         Argument_10 => 0,
         Argument_11 => 0,
         Argument_12 => 0,
         Argument_13 => 0,
         Argument_14 => 0,
         Argument_15 => 0,
         Argument_16 => 0
      )));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Engine_Arguments_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Invalid_Engine_Id_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Invalid_Engine_Id_History.Get (1), (Engine_Id => 4));

      -- OK, now try sending command to already running engine.
      T.Command_T_Send (T.Commands.Set_Engine_Arguments ((
         Engine_Id => 0,
         Argument_01 => 0,
         Argument_02 => 0,
         Argument_03 => 0,
         Argument_04 => 0,
         Argument_05 => 0,
         Argument_06 => 0,
         Argument_07 => 0,
         Argument_08 => 0,
         Argument_09 => 0,
         Argument_10 => 0,
         Argument_11 => 0,
         Argument_12 => 0,
         Argument_13 => 0,
         Argument_14 => 0,
         Argument_15 => 0,
         Argument_16 => 0
      )));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Set_Engine_Arguments_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Unable_To_Load_Engine_Arguments_History.Get_Count, 1);
      Unexpected_Engine_State_Assert.Eq (T.Unable_To_Load_Engine_Arguments_History.Get (1), (Engine_Id => 0, Engine_State => Active));

      -- Send the command response to the sequencer to finish engine execution cleanly
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check event:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));
   end Test_Set_Engine_Arguments;

   -- This unit test tests the return value feature.
   overriding procedure Test_Return_Val (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/return_val_20.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
      Load_Command : Command.T;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (0);   -- Engine 0

      -- OK now run the sequence in engine 0;
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events and expect sequence to have errored:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 20, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- One command output
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 ((Value => 1)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect load command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Load_Command := Create_Sequence_Load_Command (Id => 20, Engine_Number => 0, Engine_Request => Specific_Engine);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

      -- Load recursive sequence again:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events;
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 20, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 1
      ));

      -- One command output
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_3 ((Value => 2)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect load command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Load_Command := Create_Sequence_Load_Command (Id => 20, Engine_Number => 0, Engine_Request => Specific_Engine);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Load_Command);

      -- Load recursive sequence again:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events;
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 3);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 20, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 2
      ));

      -- One command output
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_A_Commands.Command_3 ((Value => 3)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect other commands to come back from recursive calls.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (6), Component_B_Commands.Command_3 ((Value => 3)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect other commands to come back from recursive calls.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 7);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (7), Component_B_Commands.Command_3 ((Value => 2)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_3_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect finished event.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 7);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));
   end Test_Return_Val;

   -- This unit test tests what happens when a floating point sequence variable overflows.
   overriding procedure Test_Bad_Float (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/bad_float_21.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
   begin
      -- OK now run the sequence in engine 0;
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events and expect sequence to have errored:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 21, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));
      Natural_Assert.Eq (T.Sequence_Execution_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Sequence_Execution_Error_History.Get (1), (
         Engine_Id => 0,
         Sequence_Id => 21,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 60, -- This was set by guess and check...
         Error_Type => Eval,
         Errant_Field_Number => 0
      ));
   end Test_Bad_Float;

   -- This unit test tests to make sure commands with complex arguments are formed correctly by the sequencer.
   overriding procedure Test_Complex_Command (Self : in out Instance) is
      use Complex_Command_Arg;
      use Complex_Command_Arg_2;
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/complex_command_22.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Component_B_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0
      Component_B_Commands.Set_Id_Base (7);
      Component_B_Commands.Set_Source_Id (0);   -- Engine 0

      -- OK now run the sequence in engine 0;
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events and expect sequence to have errored:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 22, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Command produced:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_4 ((A => Green, B => -3.33, C => 30)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Command produced:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Component_A_Commands.Command_4 ((A => Green, B => -3.33, C => 30)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Command produced:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 3);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (3), Component_A_Commands.Command_4 ((A => Green, B => -3.33, C => 30)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Command produced:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 4);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (4), Component_A_Commands.Command_4 ((A => Green, B => -3.33, C => 30)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 5);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (5), Component_A_Commands.Command_4 ((A => Red, B => 0.0, C => 0)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 6);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (6), Component_A_Commands.Command_4 ((A => Red, B => 2.22, C => 17)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 7);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (7), Component_B_Commands.Command_4 ((A => Green, B => -2.22, C => 1)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_B_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 8);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (8), Component_A_Commands.Command_4 ((A => Blue, B => 0.5, C => 12)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_4_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 9);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (9), Component_A_Commands.Command_5 ((A => True, B => -14, C => 989)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_5_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 10);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (10), Component_A_Commands.Command_5 ((A => False, B => 12, C => 0)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_5_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 11);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (11), Component_A_Commands.Command_5 ((A => True, B => -13, C => 44)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_5_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 12);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (12), Component_A_Commands.Command_5 ((A => True, B => -13, C => 44)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_5_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 13);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (13), Component_A_Commands.Command_5 ((A => True, B => -13, C => 44)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_5_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 14);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (14), Component_A_Commands.Command_5 ((A => True, B => -13, C => 44)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_5_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Next command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 15);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (15), Component_A_Commands.Command_5 ((A => True, B => -13, C => 44)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_5_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect finished event.
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 15);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));
   end Test_Complex_Command;

   -- This unit test tests a signed integer corner case that needs to be handled.
   overriding procedure Test_Signed_Integer_Handling (Self : in out Instance) is
      use Interfaces;
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/integer_command_23.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- Set telemetry value:
      T.Component_A_Data_Product_4 := (Value => -1);

      -- OK now run the sequence in engine 0;
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events and expect sequence to have errored:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 23, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Command produced:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_6 ((Value => -1)));

      -- Send the command response to the sequencer
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_6_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect finished event.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Finished_Sequence_History.Get_Count, 1);
      Packed_Sequence_Engine_Id_Assert.Eq (T.Finished_Sequence_History.Get (1), (Engine_Id => 0));
   end Test_Signed_Integer_Handling;

   -- This unit test tests a telemetry fetch when the telemetry item never becomes available.
   overriding procedure Test_Set_Telemetry_Timeout (Self : in out Instance) is
      use Data_Product_Enums.Fetch_Status;
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/tlm_14.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
   begin
      -- Set telem status to return not available:
      T.Data_Product_Fetch_Return_Status := Not_Available;

      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);

      -- Set data product values:
      T.Component_A_Data_Product_1 := (Value => 17);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 1);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 14, Length => Sequence_Size),
         Engine_Id => 0,
         Stack_Level => 0
      ));

      -- Make sure we did not output the first command yet:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 0);

      -- OK send ticks to simulate timeout:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Sike, no timeout, the telem is now available:
      T.Data_Product_Fetch_Return_Status := Success;
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Check command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_3 ((Value => 17)));

      --
      -- OK this sequence is executing nominally, let's start it again in another engine
      --

      -- Set telem status to return not available:
      T.Data_Product_Fetch_Return_Status := Not_Available;

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Set data product values:
      T.Component_A_Data_Product_1 := (Value => 19);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 2);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 2);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (2), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 1, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 14, Length => Sequence_Size),
         Engine_Id => 1,
         Stack_Level => 0
      ));

      -- Make sure we did not output the first command yet:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- OK timeout this time
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Sequence_Timeout_Error_History.Get_Count, 2);
      Engine_Error_Type_Assert.Eq (T.Sequence_Timeout_Error_History.Get (1), (
         Engine_Id => 0,
         Sequence_Id => 14,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 105, -- This was set by guess and check...
         Error_Type => Command_Timeout,
         Errant_Field_Number => 0
      ));
      Engine_Error_Type_Assert.Eq (T.Sequence_Timeout_Error_History.Get (2), (
         Engine_Id => 1,
         Sequence_Id => 14,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 24, -- This was set by guess and check...
         Error_Type => Telemetry_Timeout,
         Errant_Field_Number => 0
      ));

      --
      -- OK start third engine and have error telem case.
      --

      -- Set telem status to return not available:
      T.Data_Product_Fetch_Return_Status := Not_Available;

      -- No events at startup:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);

      -- Set data product values:
      T.Component_A_Data_Product_1 := (Value => 19);

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 3);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Starting_Sequence_History.Get_Count, 3);
      Sequence_Load_Info_Assert.Eq (T.Starting_Sequence_History.Get (3), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 2, Sequence_Region => Sequence_Region),
         Header => (Crc => Sequence_Buffer (0 .. 1), Version => 0, Category => 0, Id => 14, Length => Sequence_Size),
         Engine_Id => 2,
         Stack_Level => 0
      ));

      -- Make sure we did not output the first command yet:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);

      -- Set telem status to return id out of range:
      T.Data_Product_Fetch_Return_Status := Id_Out_Of_Range;

      -- Send tick.
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Data_Product_Id_Out_Of_Range_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Data_Product_Id_Out_Of_Range_Error_History.Get (1), (
         Engine_Id => 2,
         Sequence_Id => 14,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 24, -- This was set by guess and check...
         Error_Type => Telemetry_Fail,
         Errant_Field_Number => 0
      ));
   end Test_Set_Telemetry_Timeout;

   -- This unit test tests when a sub sequence load fails due to timeout.
   overriding procedure Test_Sub_Seq_Load_Timeout (Self : in out Instance) is
      T : Component.Command_Sequencer.Implementation.Tester.Instance_Access renames Self.Tester;
      Sequence_Buffer : Basic_Types.Byte_Array (0 .. 99999);
      Sequence_Size : constant Natural := Load_Sequence (File_Path => "test_sequences/build/bin/call_06.bin", Buffer => Sequence_Buffer);
      Sequence_Region : constant Memory_Region.T := (Address => Sequence_Buffer'Address, Length => Sequence_Size);
      Component_A_Commands : Test_Component_Commands.Instance;
      Load_Command : Command.T;
   begin
      -- OK now let's reinitialize the component and put it in continue through command error
      -- mode and make sure that works correctly.
      T.Component_Instance.Final;
      -- Call component init here and initialize engines again.
      T.Component_Instance.Init (
         Num_Engines => 3,
         Stack_Size => 3,
         Create_Sequence_Load_Command_Function => Create_Sequence_Load_Command'Access,
         Packet_Period => 1,
         Continue_On_Command_Failure => True,
         Timeout_Limit => 3,
         Instruction_Limit => 10000
      );
      T.Command_Response_T_Send ((
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => 0,
         Status => Register_Source
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Setup commands for component A and B:
      Component_A_Commands.Set_Id_Base (1);
      Component_A_Commands.Set_Source_Id (0);   -- Engine 0

      -- Load a simple sequence:
      T.Sequence_Load_T_Send ((Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Make sure we got a sequence load return:
      Natural_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get_Count, 1);
      Sequence_Load_Return_Assert.Eq (T.Sequence_Load_Return_T_Recv_Sync_History.Get (1), (
         Load => (Engine_Request => Specific_Engine, Engine_Id => 0, Sequence_Region => Sequence_Region),
         Status => Success
      ));

      -- Make sure we output the first command:
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 1);
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (1), Component_A_Commands.Command_2 ((Seconds => 22, Subseconds => 88)));

      -- Ok, send the command response.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => Component_A_Commands.Get_Command_2_Id, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Now expect the component to have output another command, the load sequence command:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      Load_Command := Create_Sequence_Load_Command (Id => 7, Engine_Number => 0, Engine_Request => Specific_Engine);
      Load_Command.Header.Source_Id := 0; -- From engine 0
      Command_Assert.Eq (T.Command_T_Recv_Sync_History.Get (2), Load_Command);

      -- OK, now if we send the command response from the load command it should NOT cause the engine to send out the next command.
      T.Command_Response_T_Send ((Source_Id => 0, Registration_Id => 0, Command_Id => 59, Status => Success));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Expect nothing. Engine should remain static until subsequence load.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);

      -- OK, now let's time out the engine while it is waiting on the subseq load:
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_T_Recv_Sync_History.Get_Count, 2);
      T.Tick_T_Send (((0, 0), 0));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Sequence_Timeout_Error_History.Get_Count, 1);
      Engine_Error_Type_Assert.Eq (T.Sequence_Timeout_Error_History.Get (1), (
         Engine_Id => 0,
         Sequence_Id => 6,
         Engine_State => Engine_Error,
         Sequence_State => Error,
         Stack_Level => 0,
         Program_Counter => 69, -- This was set by guess and check...
         Error_Type => Load_Timeout,
         Errant_Field_Number => 0
      ));
   end Test_Sub_Seq_Load_Timeout;

end Command_Sequencer_Tests.Implementation;
