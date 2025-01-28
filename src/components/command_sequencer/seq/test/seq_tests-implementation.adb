--------------------------------------------------------------------------------
-- Seq Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Basic_Assertions; use Basic_Assertions;
with Ada.Sequential_IO;
with Sys_Time; use Sys_Time;
with Packed_U16; use Packed_U16;
with Packed_U32; use Packed_U32;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Sys_Time.Assertion; use Sys_Time.Assertion;
with Seq_Print.Assertion; use Seq_Print.Assertion;
with Sequence_Header.Assertion; use Sequence_Header.Assertion;
with Interfaces; use Interfaces;
with Telemetry_Record;
with Byte_Array_Util;
with Command;

with Sequence_Header;
with Goto_Record;
with Set_Bit_Record;
with Send_Bit_Record;
with Load_New_Seq_Record;
with Push_Record;
with Fetch_Var_Record;
with Store_Var_Record;
with Eval_Record;
with Var_Record;
with Wait_Record;
with Jump_Zero_Record;
with Jump_Equal_Record;
with Fetch_Tlm_Record;
with Cast_Record;
with Wait_On_B_Record;
with Packed_Poly_32_Type;
with Print_Var_Record;

with Seq_Runtime.Tester;

package body Seq_Tests.Implementation is

   use Seq_Runtime_State;
   use Seq_Engine_State;
   use Seq_Error;
   use Var_Origin;

   -- We reuse engine a lot in here and it hides the Engine type stored in seq.
   pragma Warnings (Off, "declaration hides ""Engine""");

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Call base Set_Up:
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      null;
   end Tear_Down_Test;

   -------------------------
   -- Helper functions
   -------------------------

   function Load_Sequence_In_Memory (File_Path : in String; Buffer : in Basic_Types.Byte_Array_Access) return Memory_Region.T is
      package Io is new Ada.Sequential_Io (Basic_Types.Byte);
      use Io;
      File : Io.File_Type;
      Data : Basic_Types.Byte;
      Sequence : Memory_Region.T;
      Sequence_Size : Natural := 0;
   begin
      --Put_Line ("Loading: "&file_path);
      Open (File, In_File, File_Path);
      while not End_Of_File (File) loop
         Read (File, Data);
            -- Put_Line (sequence_size'Image&" "&data'Image);
         Buffer (Sequence_Size) := Data;
         Sequence_Size := @ + 1;
      end loop;
      --Put_Line ("Size: "&sequence_size'Image&" bytes");
      Sequence := (Address => (Buffer (0)'Address), Length => Sequence_Size);
      Close (File);
      return Sequence;
   end Load_Sequence_In_Memory;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Load_Sequence_Test (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Sequence : Memory_Region.T;
      Buffer : Basic_Types.Byte_Array_Access;
      Engine : Seq.Engine;
      Prev_Length : Natural;
   begin
      -- Allocate buffer for sequence:
      Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
      Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/1.bin", Buffer);

      -- Make sure uninitialized engine throws error on sequence load:
      Engine.Initialize (1, 1);
      begin
         Load_Status_Assert.Eq (Engine.Load (Sequence), Failure);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         Assert (False, "Failed to throw an exception when trying to load seq into an uninitialized engine");
      exception
         when others =>
            null;
      end;

      declare
         Seq_Header : Sequence_Header.T with
           Import, Convention => Ada, Address => Sequence.Address;
      begin
         -- Make sure initialized engine does not throw error on sequence load:
         Engine.Set_Source_Id (0);
         Natural_Assert.Eq (Natural (Engine.Get_Parent_Position), 0);
         Natural_Assert.Eq (Natural (Engine.Get_Lowest_Child_Position), 0);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/1.bin", Buffer);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success); -- Loading valid seq
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Natural_Assert.Eq (Natural (Engine.Get_Stack_Level), 0);
         Memory_Region_Assert.Eq (Engine.Get_Sequence_Region (Engine.Get_Stack_Level), Sequence);
         Natural_Assert.Neq (Natural (Engine.Get_Parent_Position), 0);
         Natural_Assert.Neq (Natural (Engine.Get_Lowest_Child_Position), 0);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Start_Time (Engine.Get_Stack_Level), (0, 0));
         Sequence_Header_Assert.Eq (Engine.Get_Sequence_Header (Engine.Get_Stack_Level), Seq_Header);
         Natural_Assert.Eq (Natural (Engine.Get_Parent_Id), 1);
         Natural_Assert.Eq (Natural (Engine.Get_Lowest_Child_Id), 1);

         -- Make sure sequence with memory region too small gets rejected with error:
         Engine.Reset;
         Natural_Assert.Eq (Natural (Engine.Get_Parent_Position), 0);
         Natural_Assert.Eq (Natural (Engine.Get_Lowest_Child_Position), 0);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Prev_Length := Sequence.Length;
         Sequence.Length := Sequence_Header.Size_In_Bytes - 1; -- too small
         Load_Status_Assert.Eq (Engine.Load (Sequence), Failure);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
         Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Load_Header);
         Sequence.Length := Prev_Length;
         Natural_Assert.Eq (Natural (Engine.Get_Parent_Id), 0);
         Natural_Assert.Eq (Natural (Engine.Get_Lowest_Child_Id), 0);

         -- Make sure sequence with header with too large length gets rejected with error:
         Engine.Reset;
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Seq_Header.Length := Sequence.Length + 1;
         Load_Status_Assert.Eq (Engine.Load (Sequence), Failure);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
         Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Load_Length);
         Seq_Header.Length := Sequence.Length - 1;

         -- Make sure loading to sequence not in unloaded state process error.
         Load_Status_Assert.Eq (Engine.Load (Sequence), Failure);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
         Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Load);
      end;

      -- Reset:
      Engine.Destroy;
      pragma Unreferenced (Engine);
      Free (Buffer);
      --Assert(False, "Test 'Load_Sequence_Test' is unimplemented. Sequences still use old header, must update.");
   end Load_Sequence_Test;

   overriding procedure Run_Valid_Sequences (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
   begin
      -- Test for seq 1
      -- Sequence 1 sends a command within a while loop, after sending the command it will wait for 5 seconds
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : Sys_Time.T := (0, 33);
         Test_Command : Command.T;
         Expected_Id : constant Command_Id := 1;
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Initialize (1, 1);
         Engine.Set_Source_Id (0);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/1.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Start_Time (Engine.Get_Stack_Level), Time);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Last_Executed_Time (Engine.Get_Stack_Level), Time);
         -- Sys_Time_Assert.Eq (Engine.Get_Sequence_Telemetry_Wait_Start_Time (Engine.Get_Stack_Level), (0, 0));
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (0, 0));
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Relative);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Start_Time (Engine.Get_Stack_Level), Time);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Last_Executed_Time (Engine.Get_Stack_Level), Time);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (0, 0));
         Engine.Change_Relative_Wait_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Absolute);
         Done_Waiting_Status_Assert.Eq (Engine.Is_Done_Waiting (Time), Still_Waiting);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Telemetry_Wait_Start_Time (Engine.Get_Stack_Level), Time);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Waiting);
         Time.Seconds := 5;
         Done_Waiting_Status_Assert.Eq (Engine.Is_Done_Waiting (Time), Done);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Start_Time (Engine.Get_Stack_Level), (0, 33));
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Last_Executed_Time (Engine.Get_Stack_Level), Time);
         Sys_Time_Assert.Eq (Engine.Get_Sequence_Telemetry_Wait_Start_Time (Engine.Get_Stack_Level), (5, 33));
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (0, 0));
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test for seq 2, 3, & 4
      -- Sequence 2 will call sequence 3 with 10 arguments, then sequence 3 will verify that they are correct
      -- Sequence 2 will then call sequence 4 with 3 arguments. Sequence 4 also has local vars defined.
      declare
         Sequence2 : Memory_Region.T;
         Sequence3 : Memory_Region.T;
         Sequence4 : Memory_Region.T;
         Buffer2 : Basic_Types.Byte_Array_Access;
         Buffer3 : Basic_Types.Byte_Array_Access;
         Buffer4 : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
         Test_Command : Command.T;
         Expected_Id : constant Command_Id := 1;
      begin
         Buffer2 := new Basic_Types.Byte_Array (0 .. 16_383);
         Buffer3 := new Basic_Types.Byte_Array (0 .. 16_383);
         Buffer4 := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 2, Engine_Id => 1);
         Sequence2 := Load_Sequence_In_Memory ("test_sequences/build/bin/2.bin", Buffer2);
         Sequence3 := Load_Sequence_In_Memory ("test_sequences/build/bin/3.bin", Buffer3);
         Sequence4 := Load_Sequence_In_Memory ("test_sequences/build/bin/4.bin", Buffer4);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence2), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Load_Seq);
         Load_Status_Assert.Eq (Engine.Load (Sequence3), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Load_Seq);
         Load_Status_Assert.Eq (Engine.Load (Sequence4), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer2);
         Free (Buffer3);
         Free (Buffer4);
      end;

      -- Test seq 5
      -- Sequence 5 does all of the possible unsigned arithmetic operations, if it goes into an ERROR state then the test has failed
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/5.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 6
      -- Sequence 6 does all of the possible signed arithmetic operations, if it goes into an ERROR state then the test has failed
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/6.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 7
      -- Sequence 7 tests overflow and underflow (allowed). Also tests that the sequence error's on certain operations (div by 0 and set unsigned as negative)
      -- TODO: Figure out what to do about this without the ability to ignore errors. Wrap engine in tester unit?
      -- declare
      --    sequence : Memory_Region.T;
      --    buffer : Basic_Types.Byte_Array_Access;
      --    engine : seq.Engine;
      --    time : constant Sys_Time.T := (0,0);
      --    error_code : Seq_Error.E;
      -- begin
      --    buffer := new Basic_Types.Byte_Array (0 .. 16383);
      --    engine.set_Source_Id (0);
      --    engine.initialize (stack_depth => 1, engine_id => 1);
      --    sequence := load_Sequence_In_Memory ("test_sequences/build/bin/7.bin", buffer);
      --    State_Assert.eq (engine.Get_Running_Sequence_State, UNLOADED);
      --    Load_Status_Assert.eq (engine.load (sequence), Success);
      --    Engine_State_Assert.eq (engine.get_engine_state, Active);
      --    Exec_State_Assert.eq (engine.execute (1000, time), Seq_Execute_State.ERROR);
      --    Seq_Error_Code_Assert.eq (engine.get_Seq_Error_Code, EVAL);
      --    engine.ignore_Error;
      --    Exec_State_Assert.eq (engine.execute (1000, time), Seq_Execute_State.ERROR);
      --    Seq_Error_Code_Assert.eq (engine.get_Seq_Error_Code, EVAL);
      --    engine.ignore_Error;
      --    Exec_State_Assert.eq (engine.execute (1000, time), Seq_Execute_State.ERROR);
      --    Seq_Error_Code_Assert.eq (engine.get_Seq_Error_Code, EVAL);
      --    engine.ignore_Error;
      --    Exec_State_Assert.eq (engine.execute (1000, time), Seq_Execute_State.ERROR);
      --    Seq_Error_Code_Assert.eq (engine.get_Seq_Error_Code, CAST);
      --    engine.ignore_Error;
      --    Exec_State_Assert.eq (engine.execute (1000, time), Seq_Execute_State.ERROR);
      --    Seq_Error_Code_Assert.eq (engine.get_Seq_Error_Code, EVAL);
      --    engine.ignore_Error;
      --    Exec_State_Assert.eq (engine.execute (1000, time), Seq_Execute_State.UNLOADED);
      --    engine.destroy;
      --    pragma Unreferenced (Engine);
      --    Free (buffer);
      -- end;

      -- Test seq 8
      -- Sequence 8 tests floating point arithmetic operations
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/8.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 9
      -- Sequence 9 tests creating and sending a command that has a local argument as a parameter
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
         Test_Command : Command.T;
         Expected_Id : constant Command_Id := 2;
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/9.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Command);
         Test_Command := Engine.Get_Command;
         Command_Id_Assert.Eq (Test_Command.Header.Id, Expected_Id);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 10
      -- Sequence 10 tests the telemetery routines, both regular waits and wait on B
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/10.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (1_000, 0));
         Engine.Set_Telemetry ([0, 0, 0, 0]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (5, 0));
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (5, 0));
         Engine.Set_Telemetry ([0, 0, 0, 0]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (2_000, 0));
         Engine.Set_Telemetry ([0, 0, 0, 1]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (10, 0));
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (10, 0));
         Engine.Set_Telemetry ([0, 0, 0, 1]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (5, 0));
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Sys_Time_Assert.Eq (Engine.Get_Telemetry_Timeout, (5, 0));
         Engine.Set_Telemetry ([0, 0, 0, 3]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Engine.Set_Telemetry ([0, 0, 0, 10]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 10 with timeouts
      -- Sequence 10 tests the telemetery routines, both regular waits and wait on B
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/10.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         -- wrong value and then timeout:
         Engine.Set_Telemetry ([0, 0, 0, 18]);
         Time := (1_000, 0);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Engine.Set_Telemetry ([0, 0, 0, 2]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Engine.Set_Telemetry ([0, 0, 0, 1]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         -- timeout:
         Time := (1_010, 0);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Engine.Set_Telemetry ([0, 0, 0, 4]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry_Relative);
         Engine.Change_Relative_Timeout_To_Absolute (Time);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Telemetry);
         Engine.Set_Telemetry ([0, 0, 0, 10]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 10 with absolute time in the past.
      -- Sequence 10 tests the telemetery routines, both regular waits and wait on B
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/10.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Time := (5_000, 0);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Error);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
         Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Wait);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 11
      -- Sequence 11 tests setting a telemetry with the set keyword instead of the waitvalue keyword
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/11.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Set_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Set_Telemetry);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Set_Telemetry);
         Engine.Set_Telemetry ([0, 0, 0, 0]);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 12
      -- Sequence 12 tests waiting on a relative value in a variable, then updating it and sending it via a command
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/12.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

         -- Should be waiting relatively for 5 seconds, should successfully wake up after that time.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Relative);
         Engine.Change_Relative_Wait_To_Absolute (Time);
         Time.Seconds := 5;
         Done_Waiting_Status_Assert.Eq (Engine.Is_Done_Waiting (Time), Done);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

         -- Should attempt to send a command
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Command);

         -- Should be in wait absolute
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Absolute);
         Sys_Time_Assert.Eq (Engine.Get_Wakeup_Time, (120_010, 0));

         -- Should still be in wait absolute
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Absolute);
         Sys_Time_Assert.Eq (Engine.Get_Wakeup_Time, (120_010, 0));

         Time.Seconds := 120_009;
         Done_Waiting_Status_Assert.Eq (Engine.Is_Done_Waiting (Time), Still_Waiting);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Waiting);
         Sys_Time_Assert.Eq (Engine.Get_Wakeup_Time, (120_010, 0));

         Time.Seconds := 120_010;
         Done_Waiting_Status_Assert.Eq (Engine.Is_Done_Waiting (Time), Done);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Sys_Time_Assert.Eq (Engine.Get_Wakeup_Time, (120_010, 0));

         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Command);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);

         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 12 with absolute time in the past.
      -- Sequence 12 tests waiting on a relative value in a variable, then updating it and sending it via a command
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/12.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

         -- Should be waiting relatively for 5 seconds, should successfully wake up after that time.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Relative);
         Engine.Change_Relative_Wait_To_Absolute (Time);
         Time.Seconds := 5;
         Done_Waiting_Status_Assert.Eq (Engine.Is_Done_Waiting (Time), Done);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

         -- Should attempt to send a command
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Command);

         -- Should still be in wait absolute, but make wait time in the past
         Time.Seconds := 800_000;
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Error);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
         Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Wait);

         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 15
      -- Sequence 15 tests the kill instruction.
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/15.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

         -- Should be waiting relatively for 5 seconds, should successfully wake up after that time.
         Natural_Assert.Eq (Natural (Engine.Get_Kill_Eng_Start), 0);
         Natural_Assert.Eq (Natural (Engine.Get_Num_Eng_Kill), 0);
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Kill_Engines);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Natural_Assert.Eq (Natural (Engine.Get_Kill_Eng_Start), 7);
         Natural_Assert.Eq (Natural (Engine.Get_Num_Eng_Kill), 3);

         -- Finish sequence
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Inactive);

         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 16
      -- Sequence 16 tests a variety of things.
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/16.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

         -- Go to spawn
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Load_Seq);

         -- Go to start
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Wait_Load_Seq);

         -- Finish sequence.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Inactive);

         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test seq 17
      -- Sequence 17 tests print functions.
      declare
         use Seq_Enums.Print_Type;
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 0);

         function Create_Print_String (Str : in String) return Seq_Types.Seq_String is
            To_Return : Seq_Types.Seq_String := [others => 0];
            pragma Warnings (Off, "overlay changes scalar storage order");
            Overlay : Basic_Types.Byte_Array (1 .. Str'Length) with
              Import, Convention => Ada, Address => Str'Address;
            pragma Warnings (On, "overlay changes scalar storage order");
         begin
            Byte_Array_Util.Safe_Left_Copy (Dest => To_Return, Src => Overlay);
            return To_Return;
         end Create_Print_String;
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Set_Source_Id (0);
         Engine.Initialize (Stack_Depth => 1, Engine_Id => 1);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/17.bin", Buffer);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

         -- Check print.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Print);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Seq_Print_Assert.Eq (Engine.Get_String_To_Print, (Info, Create_Print_String ("This is my informational statement")));

         -- Check print.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Print);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Seq_Print_Assert.Eq (Engine.Get_String_To_Print, (Error, Create_Print_String ("Bad thing is happening!")));

         -- Check print.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Print);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Seq_Print_Assert.Eq (Engine.Get_String_To_Print, (Debug, Create_Print_String ("The value of 2 + 2 is 4")));

         -- Check print.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Print);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Seq_Print_Assert.Eq (Engine.Get_String_To_Print, (Critical, Create_Print_String ("Burn it all down")));

         -- Check print.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Print);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Seq_Print_Assert.Eq (Engine.Get_String_To_Print, (Info, Create_Print_String (" 1234")));

         -- Check print.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Print);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Seq_Print_Assert.Eq (Engine.Get_String_To_Print, (Debug, Create_Print_String ("-1234")));

         -- Check print.
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Print);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         Seq_Print_Assert.Eq (Engine.Get_String_To_Print, (Critical, Create_Print_String ("-1.35000E+01")));

         -- Finish sequence
         Exec_State_Assert.Eq (Engine.Execute (1_000, Time), Seq_Execute_State.Unloaded);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Inactive);

         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;
   end Run_Valid_Sequences;

   -- A test that a sequence that executes too many instructions is terminated.
   overriding procedure Instruction_Overflow (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
      Sequence : Memory_Region.T;
      Buffer : Basic_Types.Byte_Array_Access;
      Engine : Seq.Engine;
      Time : Sys_Time.T;
   begin
      -- Allocate buffer for sequence:
      Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
      Engine.Initialize (1, 1);
      Engine.Set_Source_Id (0);

      -- Make sure initialized engine does not throw error on sequence load:
      State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
      Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/14.bin", Buffer);
      Load_Status_Assert.Eq (Engine.Load (Sequence), Success); -- Loading valid seq
      Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);

      -- Now execute the engine.
      Exec_State_Assert.Eq (Engine.Execute (10_000, Time), Seq_Execute_State.Error);
      Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
      State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
      Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Limit);

      -- Let's force another engine error for fun to make sure that works too:
      Engine.Set_Engine_Error (Error_Code => Spawn);
      Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
      State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
      Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Spawn);

      Engine.Reset;
      Engine_State_Assert.Eq (Engine.Get_Engine_State, Inactive);
      State_Assert.Eq (Engine.Get_Running_Sequence_State, Unloaded);
      Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, None);

      -- Let's force another engine error for fun to make sure that works too:
      Engine.Set_Engine_Error (Error_Code => Execute);
      Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
      State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
      Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Execute);

      -- Reset:
      Engine.Destroy;
      pragma Unreferenced (Engine);
      Free (Buffer);
      --Assert(False, "Test 'Load_Sequence_Test' is unimplemented. Sequences still use old header, must update.");
   end Instruction_Overflow;

   overriding procedure Set_Bit_Pattern (Self : in out Instance) is
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 30);
      Sbp_Record : constant Set_Bit_Record.T := (Opcode => 0, Pad => 0, Length => 0);
      Instruction : constant Basic_Types.Byte_Array := Set_Bit_Record.Serialization.To_Byte_Array (Sbp_Record);

      Instruction_With_Long_Command : constant Basic_Types.Byte_Array (0 .. 35) := [0, 0, 0, 33, 0, 1, 0, 2, 28, others => 0];
      Instruction_With_Wrong_Size : constant Basic_Types.Byte_Array (0 .. 35) := [0, 0, 0, 33, 0, 1, 0, 2, 20, others => 0];

      Instruction_Valid : constant Basic_Types.Byte_Array (0 .. 35) := [0, 0, 0, 20, 0, 1, 0, 2, 15, others => 0];
   begin
      -- Cause a command serialization failure (zero value)
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Set_Bit_Pattern (Self.Wb_Runner, Header, Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Cause a read off the end of the sequence failure
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Set_Bit_Pattern (Self.Wb_Runner, Header, Instruction_With_Long_Command), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Cause byte serialized mismatch
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Set_Bit_Pattern (Self.Wb_Runner, Header, Instruction_With_Wrong_Size), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Successfully read a command
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Set_Bit_Pattern (Self.Wb_Runner, Header, Instruction_Valid), 24);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Set_Bit_Pattern;

   overriding procedure Send_Bit_Pattern (Self : in out Instance) is
   begin
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Send_Bit_Pattern (Self.Wb_Runner), Seq_Position (Send_Bit_Record.Size_In_Bytes));
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Command);
   end Send_Bit_Pattern;

   overriding procedure Update_Bit_Pattern (Self : in out Instance) is
      -- U8 - U8 - U16 - U32 | Opcode - Pad - Offset - Length
      -- cannot offset of zero, that would overwrite command header.
      Bad_Offset_Bit_Pattern : constant Basic_Types.Byte_Array := [2, 0, 0, 0, 0, 0, 0, 0];
      Bad_Length_Bit_Pattern : constant Basic_Types.Byte_Array := [2, 0, 250, 0, 255, 255, 255, 255];
      Bad_Combined_Bit_Pattern : constant Basic_Types.Byte_Array := [2, 0, 250, 0, 0, 0, 0, 33];
   begin
      -- Bad offset test
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Update_Bit_Pattern (Self.Wb_Runner, Bad_Offset_Bit_Pattern), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Update_Bit_Pattern);
      Unsigned_32_Assert.Eq (Self.Wb_Runner.Get_Errant_Field_Number, 3);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Bad length test
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Update_Bit_Pattern (Self.Wb_Runner, Bad_Length_Bit_Pattern), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Update_Bit_Pattern);
      Unsigned_32_Assert.Eq (Self.Wb_Runner.Get_Errant_Field_Number, 4);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Bad combined + length test
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Update_Bit_Pattern (Self.Wb_Runner, Bad_Combined_Bit_Pattern), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Command_Argument);
      Unsigned_32_Assert.Eq (Self.Wb_Runner.Get_Errant_Field_Number, 0);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Update_Bit_Pattern;

   overriding procedure Call (Self : in out Instance) is
      --testId : Packed_U16.T := (Value => 32);
      Call_Instruction : constant Load_New_Seq_Record.T := (Opcode => 15, Engine => 0, Id => 32);
      Instruction : constant Basic_Types.Byte_Array := Load_New_Seq_Record.Serialization.To_Byte_Array (Call_Instruction);
   begin
      -- Verify parse and jump, execute the instruction
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Call (Self.Wb_Runner, Instruction), 4);
      -- Verify correct state post exec
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Load_New_Sub_Seq);
      -- Verify seq id post exec
      Seq_Id_Assert.Eq (Self.Wb_Runner.Get_Seq_Id_To_Load, 32);
   end Call;

   overriding procedure Spawn (Self : in out Instance) is
      --testId : Packed_U16.T := (Value => 4);
      Spawn_Instruction : constant Load_New_Seq_Record.T := (Opcode => 15, Engine => 3, Id => 4);
      Instruction : constant Basic_Types.Byte_Array := Load_New_Seq_Record.Serialization.To_Byte_Array (Spawn_Instruction);
      Dest : Sequence_Engine_Id;
   begin
      -- Verify parse and jump, execute the instruction
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Spawn (Self.Wb_Runner, Instruction), 4);
      -- Verify correct state post exec
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Load_New_Seq_Elsewhere);
      -- Verify seq id post exec
      Seq_Id_Assert.Eq (Self.Wb_Runner.Get_Seq_Id_To_Load, 4);
      -- Verify spawn destination
      Dest := Self.Wb_Runner.Get_Spawn_Destination;
      Seq_Engine_Id_Assert.Eq (Dest, 3);
   end Spawn;

   overriding procedure Start (Self : in out Instance) is
      --testId : Packed_U16.T := (Value => 2137);
      Start_Instruction : constant Load_New_Seq_Record.T := (Opcode => 15, Engine => 0, Id => 2_137);
      Instruction : constant Basic_Types.Byte_Array := Load_New_Seq_Record.Serialization.To_Byte_Array (Start_Instruction);
   begin
      -- Verify parse and jump, execute the instruction
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Start (Self.Wb_Runner, Instruction), 4);
      -- Verify correct state post exec
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Load_New_Seq_Overwrite);
      -- Verify seq id post exec
      Seq_Id_Assert.Eq (Self.Wb_Runner.Get_Seq_Id_To_Load, 2_137);
   end Start;

   -- TODO make this test more explicit
   overriding procedure Push (Self : in out Instance) is
      Goodpush : constant Push_Record.T := (Opcode => 6, Engine => 0, Destination => 14, Pad => 0);
      Badpush : constant Push_Record.T := (Opcode => 6, Engine => 0, Destination => 0, Pad => 0);
      Goodinstruction : constant Basic_Types.Byte_Array := Push_Record.Serialization.To_Byte_Array (Goodpush);
      Badinstruction : Basic_Types.Byte_Array := Push_Record.Serialization.To_Byte_Array (Badpush);
   begin
      Badinstruction (2) := 20;
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Push (Self.Wb_Runner, Goodinstruction), 4);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Push (Self.Wb_Runner, Badinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
   end Push;

   overriding procedure Eval (Self : in out Instance) is
      Add_Record : constant Eval_Record.T := (Opcode => 7, Operation => Seq_Operation.Addition, Pad1 => 0, Pad2 => 0);
      Sub_Record : constant Eval_Record.T := (Opcode => 7, Operation => Seq_Operation.Subtraction, Pad1 => 0, Pad2 => 0);

      Max_Var : constant Var_Record.T := (Id => 4_294_967_295, Var_Type => In_Sequence, Pad => 0);
      Max_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Max_Var);
      Zero_Var : constant Var_Record.T := (Id => 0, Var_Type => In_Sequence, Pad => 0);
      Zero_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Zero_Var);

      Max_Fetch_Instruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Max_Fetch);
      Zero_Fetch_Instruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Zero_Fetch);

      Add_Instruction : constant Basic_Types.Byte_Array := Eval_Record.Serialization.To_Byte_Array (Add_Record);
      Sub_Instruction : constant Basic_Types.Byte_Array := Eval_Record.Serialization.To_Byte_Array (Sub_Record);
      Invalid_Instruction : constant Basic_Types.Byte_Array := [7, 0, 0, 0];
   begin
      -- Instruction parse error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval (Self.Wb_Runner, Invalid_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- DENY Overflow
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Max_Fetch_Instruction, Seq_Internal.A), 12);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Max_Fetch_Instruction, Seq_Internal.B), 12);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval (Self.Wb_Runner, Add_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Deny Underflow
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Zero_Fetch_Instruction, Seq_Internal.A), 12);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Max_Fetch_Instruction, Seq_Internal.B), 12);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval (Self.Wb_Runner, Sub_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Valid operation
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Max_Fetch_Instruction, Seq_Internal.A), 12);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Zero_Fetch_Instruction, Seq_Internal.B), 12);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval (Self.Wb_Runner, Add_Instruction), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 4_294_967_295);
   end Eval;

   overriding procedure Fetch_Var (Self : in out Instance) is
      Value1 : constant Packed_U32.T := (Value => 12);
      Value2 : constant Packed_U32.T := (Value => 103);
      Id1 : constant Packed_U32.T := (Value => 2);
      Id2 : constant Packed_U32.T := (Value => 3);
      Badid : constant Packed_U32.T := (Value => 30);
      Test_Seq_Vara : constant Packed_Poly_32_Type.T := (Value => [0, 0, 0, 22]);
      Test_Seq_Varb : constant Packed_Poly_32_Type.T := (Value => [255, 255, 255, 255]);
      Constantvarrecorda : constant Var_Record.T := (Id => Value1.Value, Var_Type => In_Sequence, Pad => 0);
      Constantvarrecordb : constant Var_Record.T := (Id => Value2.Value, Var_Type => In_Sequence, Pad => 0);
      Localvarrecorda : constant Var_Record.T := (Id => Id1.Value, Var_Type => Local, Pad => 0);
      Localvarrecordb : constant Var_Record.T := (Id => Id2.Value, Var_Type => Local, Pad => 0);
      Badvarrecord : constant Var_Record.T := (Id => Badid.Value, Var_Type => Local, Pad => 0);
      Badvarrecord2 : constant Var_Record.T := (Id => Badid.Value, Var_Type => Internal, Pad => 0);
      Globalrecord : constant Var_Record.T := (Id => Id1.Value, Var_Type => Global, Pad => 0);
      Goodfetcha : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Constantvarrecorda);
      Goodfetchb : constant Fetch_Var_Record.T := (Opcode => 9, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Constantvarrecordb);
      Goodfetchalocal : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Localvarrecorda);
      Goodfetchblocal : constant Fetch_Var_Record.T := (Opcode => 9, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Localvarrecordb);
      Badfetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Badvarrecord);
      Badfetch2 : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Badvarrecord2);
      Globalfetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Globalrecord);

      Goodfetchainstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Goodfetcha);
      Goodfetchbinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Goodfetchb);
      Goodfetchalocalinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Goodfetchalocal);
      Goodfetchblocalinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Goodfetchblocal);
      Badfetchinstruction : Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Badfetch);
      Badfetchindexinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Badfetch);
      Badfetchindexinstruction2 : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Badfetch2);
      Globalfetchinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Globalfetch);
   begin
      Badfetchinstruction (9) := 5;
      -- Test that constant for internal A works
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Goodfetchainstruction, Seq_Internal.A), 12);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, Value1.Value); -- Check that internal A is value 1
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error); -- Check that no error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test that constant for internal B works
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Goodfetchbinstruction, Seq_Internal.B), 12);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, Value2.Value); -- Check that internal B is value 2
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error); -- Check that no error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test that local array for internal A works
      Seq_Runtime.Tester.Set_Local (Self.Wb_Runner, 2, Test_Seq_Vara);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Goodfetchalocalinstruction, Seq_Internal.A), 12);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 22); -- Check that internal A is 22
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error); -- Check that no error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test that local array for internal B works
      Seq_Runtime.Tester.Set_Local (Self.Wb_Runner, 3, Test_Seq_Varb);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Goodfetchblocalinstruction, Seq_Internal.B), 12);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, 4_294_967_295); -- Check that internal B is 4,294,967,295 (32 bit unsigned max)
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error); -- Check that no error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test for invalid instruction parsing
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Badfetchinstruction, Seq_Internal.A), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error); -- Check that an error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test for array index that is out of bounds
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Badfetchindexinstruction, Seq_Internal.A), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error); -- Check that an error was thrown

      -- Test for array index that is out of bounds
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Badfetchindexinstruction2, Seq_Internal.A), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error); -- Check that an error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test for global instruction parsing
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Globalfetchinstruction, Seq_Internal.A), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error); -- Check that an error was thrown
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Unimplemented);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Fetch_Var;

   overriding procedure Store_Var (Self : in out Instance) is
      Val : constant Packed_U32.T := (Value => 2_653);
      Validid : constant Packed_U32.T := (Value => 15);
      Invalidid : constant Packed_U32.T := (Value => 16);
      Constantvar : constant Var_Record.T := (Id => Val.Value, Var_Type => In_Sequence, Pad => 0);
      Storevar : constant Var_Record.T := (Id => Validid.Value, Var_Type => Local, Pad => 0);
      Storevarinternal : constant Var_Record.T := (Id => 3, Var_Type => Internal, Pad => 0); -- internal b
      Invalidstorevar : constant Var_Record.T := (Id => Invalidid.Value, Var_Type => Local, Pad => 0);
      Invalidstorevar2 : constant Var_Record.T := (Id => Invalidid.Value, Var_Type => Internal, Pad => 0);
      Globalvar : constant Var_Record.T := (Id => Validid.Value, Var_Type => Global, Pad => 0);

      Fetchconstant : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Constantvar);
      Storerecord : constant Store_Var_Record.T := (Opcode => 10, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Storevar);
      Storerecordinternal : constant Store_Var_Record.T := (Opcode => 10, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Storevarinternal);
      Fetchintob : constant Fetch_Var_Record.T := (Opcode => 9, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Storevar);
      Invalidstore : constant Store_Var_Record.T := (Opcode => 10, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Storevar);
      Invalidstoreid : constant Store_Var_Record.T := (Opcode => 10, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Invalidstorevar);
      Invalidstoreid2 : constant Store_Var_Record.T := (Opcode => 10, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Invalidstorevar2);
      Globalrecord : constant Store_Var_Record.T := (Opcode => 10, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Globalvar);

      Fetchconstantinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetchconstant);
      Storerecordinstruction : constant Basic_Types.Byte_Array := Store_Var_Record.Serialization.To_Byte_Array (Storerecord);
      Storerecordinternalinstruction : constant Basic_Types.Byte_Array := Store_Var_Record.Serialization.To_Byte_Array (Storerecordinternal);
      Fetchintobinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetchintob);
      Invalidstoreinstruction : Basic_Types.Byte_Array := Store_Var_Record.Serialization.To_Byte_Array (Invalidstore);
      Invalidstoreidinstruction : constant Basic_Types.Byte_Array := Store_Var_Record.Serialization.To_Byte_Array (Invalidstoreid);
      Invalidstoreidinstruction2 : constant Basic_Types.Byte_Array := Store_Var_Record.Serialization.To_Byte_Array (Invalidstoreid2);
      Globalrecordinstruction : constant Basic_Types.Byte_Array := Store_Var_Record.Serialization.To_Byte_Array (Globalrecord);
   begin
      Invalidstoreinstruction (9) := 5;
      -- Check that the correct data gets stored at the correct ID
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Fetchconstantinstruction, Seq_Internal.A), 12); -- Loads 2653 into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);                                                                                                    -- Sets position to 0
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Store_Var (Self.Wb_Runner, Storerecordinstruction), 12);                      -- Stores internal A at position 15 in the var array
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);                                                                                                    -- Sets position to 0
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Fetchintobinstruction, Seq_Internal.B), 12);      -- Loads position 15 from var array into internal B
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, Val.Value);                      -- Verify that internal B contains the value we expect
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);                                                                                                    -- Check that no error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);                                                                                                      -- Reset the instance, this test is over

      -- Check that the correct data gets stored at internal b
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Fetchconstantinstruction, Seq_Internal.A), 12); -- Loads 2653 into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);                                                                                                    -- Sets position to 0
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Store_Var (Self.Wb_Runner, Storerecordinternalinstruction), 12);                      -- Stores internal A at position 2 in the internal array
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);                                                                                                    -- Sets position to 0
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, Val.Value);                      -- Verify that internal B contains the value we expect
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);                                                                                                    -- Check that no error was thrown
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);                                                                                                      -- Reset the instance, this test is over

      -- Check that the instruction parsing will fail
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Store_Var (Self.Wb_Runner, Invalidstoreinstruction), 0);                      -- Run the instruction with an errant field
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);                                                                                                      -- Check that the sequencer is in an ERROR state
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);                                                                                                      -- Reset the instance, this test is over

      -- Check that an invalid variable array ID will fail
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Store_Var (Self.Wb_Runner, Invalidstoreidinstruction), 0);                      -- Run the instruction with an invalid var ID
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);                                                                                                      -- Check that the sequencer is in an ERROR state
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Variable);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Check that an invalid variable array ID will fail
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Store_Var (Self.Wb_Runner, Invalidstoreidinstruction2), 0);                      -- Run the instruction with an invalid var ID
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);                                                                                                      -- Check that the sequencer is in an ERROR state
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Variable);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Check that the instruction global
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Store_Var (Self.Wb_Runner, Globalrecordinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Unimplemented);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Store_Var;

   overriding procedure Fetch_Tlm (Self : in out Instance) is
      use Seq_Enums.Seq_Data_Format;
      Val1 : constant Packed_U16.T := (Value => 1);
      Val2 : constant Packed_U16.T := (Value => 2);
      Val3 : constant Packed_U16.T := (Value => 3);
      Testtlm1 : constant Telemetry_Record.T := (Id => Val1.Value, Offset => Val2.Value, Size => 3, New_Value_Required => True, Tlm_Type => Unsigned_Byte);
      Testtlm2 : constant Telemetry_Record.T := (Id => Val2.Value, Offset => Val3.Value, Size => 4, New_Value_Required => False, Tlm_Type => Unsigned_Byte);
      Testtlminvalid : constant Telemetry_Record.T := (Id => Val1.Value, Offset => Val2.Value, Size => 3, New_Value_Required => True, Tlm_Type => Unsigned_Byte);
      Testtlmreca : constant Fetch_Tlm_Record.T := (Opcode => 11, Pad1 => 0, Pad2 => 0, Waiton => True, Tlm_Info => Testtlm1);
      Testtlmrecb : constant Fetch_Tlm_Record.T := (Opcode => 12, Pad1 => 0, Pad2 => 0, Waiton => True, Tlm_Info => Testtlm2);
      Testtlmrecinv : constant Fetch_Tlm_Record.T := (Opcode => 11, Pad1 => 0, Pad2 => 0, Waiton => True, Tlm_Info => Testtlminvalid);

      Tlmainstruction : constant Basic_Types.Byte_Array := Fetch_Tlm_Record.Serialization.To_Byte_Array (Testtlmreca);
      Tlmbinstruction : constant Basic_Types.Byte_Array := Fetch_Tlm_Record.Serialization.To_Byte_Array (Testtlmrecb);
      Tlminvinstruction : Basic_Types.Byte_Array := Fetch_Tlm_Record.Serialization.To_Byte_Array (Testtlmrecinv);
   begin
      Tlminvinstruction (10) := 255;
      -- Test instruction parse failure
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Tlm (Self.Wb_Runner, Tlminvinstruction, Seq_Internal.A), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Test success to internal A
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Tlm (Self.Wb_Runner, Tlmainstruction, Seq_Internal.A), 12);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_16_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.Id, 1);
      Unsigned_16_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.Offset, 2);
      Seq_Positive_16_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.Size, 3);
      Boolean_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.New_Value_Required, True);
      Internal_Src_Assert.Eq (Seq_Runtime.Tester.Get_Telemetry_Destination (Self.Wb_Runner), Seq_Internal.A);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Test success to internal B
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Tlm (Self.Wb_Runner, Tlmbinstruction, Seq_Internal.B), 12);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_16_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.Id, 2);
      Unsigned_16_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.Offset, 3);
      Seq_Positive_16_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.Size, 4);
      Boolean_Assert.Eq (Self.Wb_Runner.Get_Telemetry_Request.New_Value_Required, False);
      Internal_Src_Assert.Eq (Seq_Runtime.Tester.Get_Telemetry_Destination (Self.Wb_Runner), Seq_Internal.B);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Fetch_Tlm;

   overriding procedure Wait (Self : in out Instance) is
      Testtime : constant Packed_U32.T := (Value => 234);
      Rwait : constant Wait_Record.T := (Opcode => 14, Wait_Type => Seq_Wait_Type.Relative, Pad1 => 0, Pad2 => 0, Wait_Time => Testtime.Value);
      Await : constant Wait_Record.T := (Opcode => 14, Wait_Type => Seq_Wait_Type.Absolute, Pad1 => 0, Pad2 => 0, Wait_Time => Testtime.Value);
      Badwait : constant Wait_Record.T := (Opcode => 14, Wait_Type => Seq_Wait_Type.Relative, Pad1 => 0, Pad2 => 0, Wait_Time => Testtime.Value);

      Rwaitinstruction : constant Basic_Types.Byte_Array := Wait_Record.Serialization.To_Byte_Array (Rwait);
      Awaitinstruction : constant Basic_Types.Byte_Array := Wait_Record.Serialization.To_Byte_Array (Await);
      Badwaitinstruction : Basic_Types.Byte_Array := Wait_Record.Serialization.To_Byte_Array (Badwait);
   begin
      Badwaitinstruction (1) := 0;
      -- Test Relative Wait Set
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait (Self.Wb_Runner, Rwaitinstruction), 8);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Relative);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test Absolute Wait Set
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait (Self.Wb_Runner, Awaitinstruction), 8);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Absolute);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Test the instruction fails
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait (Self.Wb_Runner, Badwaitinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
   end Wait;

   overriding procedure Seq_Goto (Self : in out Instance) is
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 10);

      Position_Valid : constant Packed_U16.T := (Value => 6);
      Position_Invalid_High : constant Packed_U16.T := (Value => 15);

      Instruction_Valid_Record : constant Goto_Record.T := (Opcode => 15, Pad => 0, Position => Position_Valid.Value);
      Instruction_Invalid_High_Record : constant Goto_Record.T := (Opcode => 15, Pad => 0, Position => Position_Invalid_High.Value);
      Instruction_Valid : constant Basic_Types.Byte_Array := Goto_Record.Serialization.To_Byte_Array (Instruction_Valid_Record);
      Instruction_Invalid_High : constant Basic_Types.Byte_Array := Goto_Record.Serialization.To_Byte_Array (Instruction_Invalid_High_Record);

   begin

      -- Make sure the jump was performed to the correct position
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Goto (Self.Wb_Runner, Header, Instruction_Valid), 6);
      -- Make sure sequencer is not an error
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);

      -- Jump to position that is too high
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Goto (Self.Wb_Runner, Header, Instruction_Invalid_High), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
   end Seq_Goto;

   overriding procedure Jump_If_Zero (Self : in out Instance) is
      Zero : constant Packed_U32.T := (Value => 0);
      Nonzero : constant Packed_U32.T := (Value => 123);
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 99);
      Zerovar : constant Var_Record.T := (Id => Zero.Value, Var_Type => In_Sequence, Pad => 0);
      Nonzerovar : constant Var_Record.T := (Id => Nonzero.Value, Var_Type => In_Sequence, Pad => 0);
      Fetchzero : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Zerovar);
      Fetchnonzero : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Nonzerovar);
      Validposition : constant Packed_U16.T := (Value => 85);
      Invalidposition : constant Packed_U16.T := (Value => 100);
      Validjump : constant Jump_Zero_Record.T := (Opcode => 16, Pad => 0, Position => Validposition.Value);
      Invalidjump : constant Jump_Zero_Record.T := (Opcode => 16, Pad => 0, Position => Invalidposition.Value);

      Validjumpinstruction : constant Basic_Types.Byte_Array := Jump_Zero_Record.Serialization.To_Byte_Array (Validjump);
      Invalidjumpinstruction : constant Basic_Types.Byte_Array := Jump_Zero_Record.Serialization.To_Byte_Array (Invalidjump);
      Zerovarinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetchzero);
      Nonzerovarinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetchnonzero);
   begin
      -- Invalid jump position
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_If_Zero (Self.Wb_Runner, Header, Invalidjumpinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position but no jump performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Nonzerovarinstruction, Seq_Internal.A), 12); -- Loads a non-zero value into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_If_Zero (Self.Wb_Runner, Header, Validjumpinstruction), 4);   -- No jump should have occurred
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position with jump performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Zerovarinstruction, Seq_Internal.A), 12); -- Loads zero into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_If_Zero (Self.Wb_Runner, Header, Validjumpinstruction), 85);   -- A jump to 85 should have occurred
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
   end Jump_If_Zero;

   overriding procedure Jump_Not_Zero (Self : in out Instance) is
      Zero : constant Packed_U32.T := (Value => 0);
      Nonzero : constant Packed_U32.T := (Value => 123);
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 99);
      Zerovar : constant Var_Record.T := (Id => Zero.Value, Var_Type => In_Sequence, Pad => 0);
      Nonzerovar : constant Var_Record.T := (Id => Nonzero.Value, Var_Type => In_Sequence, Pad => 0);
      Fetchzero : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Zerovar);
      Fetchnonzero : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Nonzerovar);
      Validposition : constant Packed_U16.T := (Value => 85);
      Invalidposition : constant Packed_U16.T := (Value => 100);
      Validjump : constant Jump_Zero_Record.T := (Opcode => 17, Pad => 0, Position => Validposition.Value);
      Invalidjump : constant Jump_Zero_Record.T := (Opcode => 17, Pad => 0, Position => Invalidposition.Value);

      Validjumpinstruction : constant Basic_Types.Byte_Array := Jump_Zero_Record.Serialization.To_Byte_Array (Validjump);
      Invalidjumpinstruction : constant Basic_Types.Byte_Array := Jump_Zero_Record.Serialization.To_Byte_Array (Invalidjump);
      Zerovarinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetchzero);
      Nonzerovarinstruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetchnonzero);
   begin
      -- Invalid jump position
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_Not_Zero (Self.Wb_Runner, Header, Invalidjumpinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position but with jump being performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Nonzerovarinstruction, Seq_Internal.A), 12); -- Loads a non-zero value into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_Not_Zero (Self.Wb_Runner, Header, Validjumpinstruction), 85);   -- Jump!
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position with no jump performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Zerovarinstruction, Seq_Internal.A), 12); -- Loads zero into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_Not_Zero (Self.Wb_Runner, Header, Validjumpinstruction), 4);   -- No jump!
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
   end Jump_Not_Zero;

   overriding procedure Jump_If_Equal (Self : in out Instance) is
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 99);
      Val1 : constant Packed_U32.T := (Value => 12);
      Val2 : constant Packed_U32.T := (Value => 32_432);
      Var1 : constant Var_Record.T := (Id => Val1.Value, Var_Type => In_Sequence, Pad => 0);
      Var2 : constant Var_Record.T := (Id => Val2.Value, Var_Type => In_Sequence, Pad => 0);
      Fetch1 : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Var1);
      Fetch2 : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Var2);
      Validposition : constant Packed_U16.T := (Value => 85);
      Invalidposition : constant Packed_U16.T := (Value => 100);
      Validjump : constant Jump_Equal_Record.T := (Opcode => 18, Pad => 0, Position => Validposition.Value, To_Compare => Val2.Value);
      Invalidjump : constant Jump_Equal_Record.T := (Opcode => 18, Pad => 0, Position => Invalidposition.Value, To_Compare => Val1.Value);

      Validjumpinstruction : constant Basic_Types.Byte_Array := Jump_Equal_Record.Serialization.To_Byte_Array (Validjump);
      Invalidjumpinstruction : constant Basic_Types.Byte_Array := Jump_Equal_Record.Serialization.To_Byte_Array (Invalidjump);
      Var1instruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetch1);
      Var2instruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetch2);
   begin
      -- Invalid jump position
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_If_Equal (Self.Wb_Runner, Header, Invalidjumpinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position but with jump being performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Var2instruction, Seq_Internal.A), 12); -- Loads 32432 into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_If_Equal (Self.Wb_Runner, Header, Validjumpinstruction), 85);   -- Jump!
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position with no jump performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Var1instruction, Seq_Internal.A), 12); -- Loads 12 into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_If_Equal (Self.Wb_Runner, Header, Validjumpinstruction), 8);   -- No jump!
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
   end Jump_If_Equal;

   overriding procedure Jump_Not_Equal (Self : in out Instance) is
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 99);
      Val1 : constant Packed_U32.T := (Value => 12);
      Val2 : constant Packed_U32.T := (Value => 32_432);
      Var1 : constant Var_Record.T := (Id => Val1.Value, Var_Type => In_Sequence, Pad => 0);
      Var2 : constant Var_Record.T := (Id => Val2.Value, Var_Type => In_Sequence, Pad => 0);
      Fetch1 : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Var1);
      Fetch2 : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Var2);
      Validposition : constant Packed_U16.T := (Value => 85);
      Invalidposition : constant Packed_U16.T := (Value => 100);
      Validjump : constant Jump_Equal_Record.T := (Opcode => 18, Pad => 0, Position => Validposition.Value, To_Compare => Val2.Value);
      Invalidjump : constant Jump_Equal_Record.T := (Opcode => 18, Pad => 0, Position => Invalidposition.Value, To_Compare => Val1.Value);

      Validjumpinstruction : constant Basic_Types.Byte_Array := Jump_Equal_Record.Serialization.To_Byte_Array (Validjump);
      Invalidjumpinstruction : constant Basic_Types.Byte_Array := Jump_Equal_Record.Serialization.To_Byte_Array (Invalidjump);
      Var1instruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetch1);
      Var2instruction : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetch2);
   begin
      -- Invalid jump position
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_Not_Equal (Self.Wb_Runner, Header, Invalidjumpinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position but with jump being performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Var2instruction, Seq_Internal.A), 12); -- Loads 32432 into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_Not_Equal (Self.Wb_Runner, Header, Validjumpinstruction), 8);   -- No jump!
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid jump position with no jump performed
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Var1instruction, Seq_Internal.A), 12); -- Loads 12 into internal A
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Jump_Not_Equal (Self.Wb_Runner, Header, Validjumpinstruction), 85);   -- Jump!
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
   end Jump_Not_Equal;

   overriding procedure Seq_Return (Self : in out Instance) is
   begin
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Return (Self.Wb_Runner), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Done);
   end Seq_Return;

   overriding procedure Wait_If_Zero (Self : in out Instance) is
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 20);
      -- U8 - E8 - U16 - U32 | Opcode - waitType - Position - Timeout
      Parseerror : constant Basic_Types.Byte_Array := [21, 0, 0, 0, 0, 0, 0, 0];
      Waiterror : constant Basic_Types.Byte_Array := [21, 1, 0, 0, 0, 0, 0, 0];
      Jumperror : constant Basic_Types.Byte_Array := [21, 2, 0, 40, 0, 0, 0, 0];
   begin
      -- Parse Error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_If_Zero (Self.Wb_Runner, Header, Parseerror), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Unsigned_32_Assert.Eq (Self.Wb_Runner.Get_Errant_Field_Number, 2);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Wait Error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_If_Zero (Self.Wb_Runner, Header, Waiterror), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Wait);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Jump Error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_If_Zero (Self.Wb_Runner, Header, Jumperror), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Jump);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Wait_If_Zero;

   overriding procedure Eval_Flt (Self : in out Instance) is
      Add_Instruction : constant Basic_Types.Byte_Array := [27, 43, 0, 0];
      Invalid_Instruction : constant Basic_Types.Byte_Array := [27, 0, 0, 0];
      Invalid_Op_Instruction : constant Basic_Types.Byte_Array := [27, 124, 0, 0];
   begin

      -- Instruction parse error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_Flt (Self.Wb_Runner, Invalid_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Unsigned_32_Assert.Eq (Self.Wb_Runner.Get_Errant_Field_Number, 2);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Invalid float operation
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_Flt (Self.Wb_Runner, Invalid_Op_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Invalid_Op);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- NaN
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.A, (Value => 4_290_772_992));
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, (Value => 0));
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_Flt (Self.Wb_Runner, Add_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Float_Value);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Inf
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.A, (Value => 2_139_095_040));
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, (Value => 0));
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_Flt (Self.Wb_Runner, Add_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Float_Value);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Neg Inf
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.A, (Value => 16#ff80_0000#));
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, (Value => 0));
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_Flt (Self.Wb_Runner, Add_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Float_Value);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Operation to inf
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.A, (Value => 2_139_095_039)); -- not inf, but close
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, (Value => 2_139_095_039)); -- also not inf, but close
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_Flt (Self.Wb_Runner, Add_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Eval);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Operation to neg inf
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.A, (Value => 16#ff00_0000#)); -- not -inf, but close
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, (Value => 16#ff00_0000#)); -- also not -inf, but close
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_Flt (Self.Wb_Runner, Add_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Eval);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Eval_Flt;

   overriding procedure Cast_F_To_U (Self : in out Instance) is
      Negative : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([191, 153, 153, 154]); -- -1.2 other values that would cause are (255,255,255,255)
      Positive : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([67, 106, 87, 10]); -- 234.34
      Neg_Inf : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([255, 16#80#, 0, 0]); -- negative infinity
      Neg_Var : constant Var_Record.T := (Id => Negative.Value, Var_Type => In_Sequence, Pad => 0);
      Pos_Var : constant Var_Record.T := (Id => Positive.Value, Var_Type => In_Sequence, Pad => 0);
      Neg_Inf_Var : constant Var_Record.T := (Id => Neg_Inf.Value, Var_Type => In_Sequence, Pad => 0);
      Neg_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Neg_Var);
      Pos_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Pos_Var);
      Neg_Inf_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Neg_Inf_Var);
      Cast_Record_A : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);
      Cast_Record_B : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.B, Pad1 => 0, Pad2 => 0);
      Bad_Cast : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);

      Instructionneg : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Neg_Fetch);
      Instructionpos : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Pos_Fetch);
      Instructionneginf : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Neg_Inf_Fetch);
      Instructiona : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_A);
      Instructionb : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_B);
      Instructionbad : Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Bad_Cast);
      Position_Ignore : Seq_Position;
   begin
      Instructionbad (1) := 5;

      -- Fail on instruction parse (Id field not 2 or 3)
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructionbad), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Fail on casting the float to unsigned (negative values)
      -- Load F32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructiona), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Cast);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load F32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructionb), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Cast);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid instruction
      -- Load F32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructiona), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 234);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load F32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructionb), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, 234);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load negative infinity and expect failure
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneginf, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructionb), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Float_Value);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cast_F_To_U;

   overriding procedure Cast_U_To_F (Self : in out Instance) is
      Val : constant Packed_U32.T := (Value => 345);
      Var : constant Var_Record.T := (Id => Val.Value, Var_Type => In_Sequence, Pad => 0);
      Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Var);
      Cast_Record_A : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);
      Cast_Record_B : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.B, Pad1 => 0, Pad2 => 0);
      Bad_Cast : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);

      Instructionfetch : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetch);
      Instructiona : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_A);
      Instructionb : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_B);
      Instructionbad : Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Bad_Cast);
      Position_Ignore : Seq_Position;
   begin
      Instructionbad (1) := 5;

      -- Fail on instruction parse (Id field not 2 or 3)
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_F (Self.Wb_Runner, Instructionbad), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Valid instruction
      -- Load U32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionfetch, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_F (Self.Wb_Runner, Instructiona), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Neq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 345);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructiona), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 345);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load U32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionfetch, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_F (Self.Wb_Runner, Instructionb), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Neq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, 345);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_U (Self.Wb_Runner, Instructionb), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, 345);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cast_U_To_F;

   overriding procedure Eval_S (Self : in out Instance) is
      Add_Instruction : constant Basic_Types.Byte_Array := [27, 43, 0, 0];
      Sub_Instruction : constant Basic_Types.Byte_Array := [30, 45, 0, 0];
      -- sub_instruction : Basic_Types.Byte_Array := Eval_Record.Serialization.To_Byte_Array (sub_record);
      Parseerror : constant Basic_Types.Byte_Array := [27, 0, 0, 0];
   begin
      -- Parse Error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_S (Self.Wb_Runner, Parseerror), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Unsigned_32_Assert.Eq (Self.Wb_Runner.Get_Errant_Field_Number, 2);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Eval Error (should fail on overflow)
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.A, (Value => 2_147_483_647));
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, (Value => 1));
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_S (Self.Wb_Runner, Add_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Eval);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Eval Error (should fail on underflow)
      pragma Warnings (Off);
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.A, (Value => -2_147_483_648));
      pragma Warnings (On);
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, (Value => 1));
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Eval_S (Self.Wb_Runner, Sub_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Eval);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Eval_S;

   overriding procedure Cast_S_To_U (Self : in out Instance) is
      Negative : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([255, 255, 255, 255]); -- -1 so will cause a casting error
      Positive : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([0, 0, 0, 100]); -- 100, will not cause any issues
      Neg_Var : constant Var_Record.T := (Id => Negative.Value, Var_Type => In_Sequence, Pad => 0);
      Pos_Var : constant Var_Record.T := (Id => Positive.Value, Var_Type => In_Sequence, Pad => 0);
      Neg_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Neg_Var);
      Pos_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Pos_Var);
      Cast_Record_A : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);
      Cast_Record_B : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.B, Pad1 => 0, Pad2 => 0);
      Bad_Cast : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);

      Instructionneg : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Neg_Fetch);
      Instructionpos : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Pos_Fetch);
      Instructiona : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_A);
      Instructionb : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_B);
      Instructionbad : Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Bad_Cast);
      Position_Ignore : Seq_Position;
   begin
      Instructionbad (1) := 5;

      -- Fail on instruction parse (Id field not 2 or 3)
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_U (Self.Wb_Runner, Instructionbad), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Fail on casting the signed to unsigned (negative values)
      -- Load I32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_U (Self.Wb_Runner, Instructiona), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load I32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_U (Self.Wb_Runner, Instructionb), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid instruction
      -- Load I32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_U (Self.Wb_Runner, Instructiona), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 100);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load I32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_U (Self.Wb_Runner, Instructionb), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, 100);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cast_S_To_U;

   overriding procedure Cast_U_To_S (Self : in out Instance) is
      Negative : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([255, 255, 255, 255]); -- u32 max, will cause an error
      Positive : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([0, 0, 0, 100]); -- 100, will not cause any issues
      Neg_Var : constant Var_Record.T := (Id => Negative.Value, Var_Type => In_Sequence, Pad => 0);
      Pos_Var : constant Var_Record.T := (Id => Positive.Value, Var_Type => In_Sequence, Pad => 0);
      Neg_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Neg_Var);
      Pos_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Pos_Var);
      Cast_Record_A : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);
      Cast_Record_B : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.B, Pad1 => 0, Pad2 => 0);
      Bad_Cast : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);

      Instructionneg : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Neg_Fetch);
      Instructionpos : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Pos_Fetch);
      Instructiona : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_A);
      Instructionb : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_B);
      Instructionbad : Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Bad_Cast);
      Position_Ignore : Seq_Position;

   begin
      Instructionbad (1) := 5;

      -- Fail on instruction parse (Id field not 2 or 3)
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_S (Self.Wb_Runner, Instructionbad), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Fail on casting the signed to unsigned (negative values)
      -- Load U32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_S (Self.Wb_Runner, Instructiona), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load U32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_S (Self.Wb_Runner, Instructionb), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid instruction
      -- Load U32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_S (Self.Wb_Runner, Instructiona), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 100);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load U32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_U_To_S (Self.Wb_Runner, Instructionb), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, 100);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cast_U_To_S;

   overriding procedure Cast_F_To_S (Self : in out Instance) is
      -- nifty float converter: https://www.h-schmidt.net/FloatConverter/IEEE754.html
      Negative : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([16#cf#, 16#9d#, 16#cd#, 16#65#]); -- -5294967296 float
      Positive : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([67, 106, 87, 10]); -- 234.34
      Nan : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([255, 255, 255, 255]); -- nan
      Neg_Var : constant Var_Record.T := (Id => Negative.Value, Var_Type => In_Sequence, Pad => 0);
      Pos_Var : constant Var_Record.T := (Id => Positive.Value, Var_Type => In_Sequence, Pad => 0);
      Nan_Var : constant Var_Record.T := (Id => Nan.Value, Var_Type => In_Sequence, Pad => 0);
      Neg_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Neg_Var);
      Pos_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Pos_Var);
      Nan_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Nan_Var);
      Cast_Record_A : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);
      Cast_Record_B : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.B, Pad1 => 0, Pad2 => 0);
      Bad_Cast : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);

      Instructionneg : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Neg_Fetch);
      Instructionpos : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Pos_Fetch);
      Instructionnan : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Nan_Fetch);
      Instructiona : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_A);
      Instructionb : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_B);
      Instructionbad : Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Bad_Cast);
      Position_Ignore : Seq_Position;
   begin
      Instructionbad (1) := 5;

      -- Fail on instruction parse (Id field not 2 or 3)
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_S (Self.Wb_Runner, Instructionbad), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Fail on extracting float, since its a nan
      -- Load F32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionnan, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_S (Self.Wb_Runner, Instructiona), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Float_Value);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Fail on casting the float to unsigned (negative values)
      -- Load F32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_S (Self.Wb_Runner, Instructiona), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Cast);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load F32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionneg, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_S (Self.Wb_Runner, Instructionb), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Valid instruction
      -- Load F32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_S (Self.Wb_Runner, Instructiona), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, 234);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load F32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionpos, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_F_To_S (Self.Wb_Runner, Instructionb), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.B).Value, 234);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cast_F_To_S;

   overriding procedure Cast_S_To_F (Self : in out Instance) is
      Val : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([255, 255, 255, 255]); -- -1 signed
      Var : constant Var_Record.T := (Id => Val.Value, Var_Type => In_Sequence, Pad => 0);
      Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Var);
      Cast_Record_A : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);
      Cast_Record_B : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.B, Pad1 => 0, Pad2 => 0);
      Bad_Cast : constant Cast_Record.T := (Opcode => 28, Id => Seq_Internal.A, Pad1 => 0, Pad2 => 0);

      Instructionfetch : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Fetch);
      Instructiona : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_A);
      Instructionb : constant Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Cast_Record_B);
      Instructionbad : Basic_Types.Byte_Array := Cast_Record.Serialization.To_Byte_Array (Bad_Cast);
      Position_Ignore : Seq_Position;
   begin
      Instructionbad (1) := 5;

      -- Fail on instruction parse (Id field not 2 or 3)
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_F (Self.Wb_Runner, Instructionbad), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Valid instruction
      -- Load I32 into Internal A
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionfetch, Seq_Internal.A);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_F (Self.Wb_Runner, Instructiona), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
      -- Load I32 into Internal B
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionfetch, Seq_Internal.B);
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Cast_S_To_F (Self.Wb_Runner, Instructionb), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cast_S_To_F;

   overriding procedure Wait_On_B (Self : in out Instance) is
      Relativerecord : constant Wait_On_B_Record.T := (Opcode => 35, Wait_Type => Seq_Wait_Type.Relative, Pad1 => 0, Pad2 => 0);
      Absoluterecord : constant Wait_On_B_Record.T := (Opcode => 35, Wait_Type => Seq_Wait_Type.Absolute, Pad1 => 0, Pad2 => 0);

      Relativeinstruction : constant Basic_Types.Byte_Array := Wait_On_B_Record.Serialization.To_Byte_Array (Relativerecord);
      Absoluteinstruction : constant Basic_Types.Byte_Array := Wait_On_B_Record.Serialization.To_Byte_Array (Absoluterecord);
      Badinstruction : Basic_Types.Byte_Array := Wait_On_B_Record.Serialization.To_Byte_Array (Relativerecord);
   begin
      Badinstruction (1) := 123;
      -- Can fail on wait type
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_On_B (Self.Wb_Runner, Badinstruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Can be relative
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, Packed_U32.Serialization.From_Byte_Array ([0, 0, 0, 10]));
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_On_B (Self.Wb_Runner, Relativeinstruction), 4);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Relative);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Can be absolute
      Seq_Runtime.Tester.Update_Internal (Self.Wb_Runner, Seq_Internal.B, Packed_U32.Serialization.From_Byte_Array ([0, 0, 0, 10]));
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_On_B (Self.Wb_Runner, Absoluteinstruction), 4);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Wait_Absolute);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Wait_On_B;

   overriding procedure Wait_If_Zero_On_B (Self : in out Instance) is
      Header : constant Sequence_Header.T := (Crc => [0, 0], Version => 0, Category => 0, Id => 0, Length => 20);
      -- U8 - E8 - U16 | Opcode - Wait Type - Position
      Parseerror : constant Basic_Types.Byte_Array := [21, 0, 0, 0];
      Waiterror : constant Basic_Types.Byte_Array := [21, 1, 0, 0];
      Jumperror : constant Basic_Types.Byte_Array := [21, 1, 0, 40];
   begin
      -- Parse Error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_If_Zero_On_B (Self.Wb_Runner, Header, Parseerror), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Unsigned_32_Assert.Eq (Self.Wb_Runner.Get_Errant_Field_Number, 2);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Wait Error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_If_Zero_On_B (Self.Wb_Runner, Header, Waiterror), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Wait);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Jump Error
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Wait_If_Zero_On_B (Self.Wb_Runner, Header, Jumperror), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Jump);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Wait_If_Zero_On_B;

   -- A test for the Cmd Print instruction
   overriding procedure Cmd_Print (Self : in out Instance) is
      use Basic_Types;
      The_String : constant Basic_Types.Byte_Array (0 .. 64) := [0 => 72, 1 => 73, others => 0];
      -- U8 - E8 - U8 - U8 - U8xN
      Good_Instruction : constant Basic_Types.Byte_Array := [0, 1, 0, 0] & The_String;
      The_Bad_Instruction : constant Basic_Types.Byte_Array := [0, 254, 0, 0] & The_String;
   begin
      -- Bad parse
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Print (Self.Wb_Runner, The_Bad_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Good parse
      Seq_Position_Assert.Gt (Seq_Runtime.Tester.Call_Print (Self.Wb_Runner, Good_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Print);
   end Cmd_Print;

   -- A test for the Cmd Print Var instruction
   overriding procedure Cmd_Print_Var (Self : in out Instance) is
      use Seq_Enums.Print_Type;
      use Seq_Enums.Seq_Data_Type;

      -- U8 - E8 - E8 - U8 - var record
      Bad_Instruction1 : constant Basic_Types.Byte_Array := [0, 254, 0, 0];
      Bad_Instruction2 : constant Basic_Types.Byte_Array := [0, 0, 254, 0];

      Max_Var : constant Var_Record.T := (Id => 4_294_967_295, Var_Type => Local, Pad => 0);
      Max_Fetch : constant Print_Var_Record.T := (Opcode => 8, Print_Type => Debug, Data_Type => Unsigned, Pad => 0, Var_Info => Max_Var);
      Max_Fetch_Instruction : constant Basic_Types.Byte_Array := Print_Var_Record.Serialization.To_Byte_Array (Max_Fetch);

      Nan : constant Packed_U32.T := Packed_U32.Serialization.From_Byte_Array ([255, 255, 255, 255]); -- nan
      Nan_Var : constant Var_Record.T := (Id => Nan.Value, Var_Type => In_Sequence, Pad => 0);
      Nan_Fetch : constant Fetch_Var_Record.T := (Opcode => 8, Pad1 => 0, Pad2 => 0, Pad3 => 0, Var_Info => Nan_Var);
      Instructionnan : constant Basic_Types.Byte_Array := Fetch_Var_Record.Serialization.To_Byte_Array (Nan_Fetch);

      Nan_Var2 : constant Var_Record.T := (Id => 2, Var_Type => Internal, Pad => 0);
      Nan_Fetch2 : constant Print_Var_Record.T := (Opcode => 8, Print_Type => Debug, Data_Type => Seq_Data_Type.Float, Pad => 0, Var_Info => Nan_Var2);
      Nan_Fetch_Instruction : constant Basic_Types.Byte_Array := Print_Var_Record.Serialization.To_Byte_Array (Nan_Fetch2);
      Position_Ignore : Seq_Position;
   begin
      -- Bad parse
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Print_Var (Self.Wb_Runner, Bad_Instruction1), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Bad parse
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Print_Var (Self.Wb_Runner, Bad_Instruction2), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Parse);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Bad variable:
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Print_Var (Self.Wb_Runner, Max_Fetch_Instruction), 0);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Variable);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);

      -- Load bad F32 into Internal A and the print that variable
      Position_Ignore := Seq_Runtime.Tester.Call_Fetch_Var (Self.Wb_Runner, Instructionnan, Seq_Internal.A);
      Unsigned_32_Assert.Eq (Seq_Runtime.Tester.Check_Internal (Self.Wb_Runner, Seq_Internal.A).Value, Nan.Value); -- Check that internal A is value 1
      Seq_Runtime.Tester.Set_Position (Self.Wb_Runner, 0);
      Position_Ignore := Seq_Runtime.Tester.Call_Print_Var (Self.Wb_Runner, Nan_Fetch_Instruction);
      State_Assert.Eq (Self.Wb_Runner.Get_State, Error);
      Seq_Error_Code_Assert.Eq (Self.Wb_Runner.Get_Error_Code, Float_Value);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cmd_Print_Var;

   -- A test for the Cmd Str Alloc instruction. This is essentially a noop for Adamant, and is tested as such.
   overriding procedure Cmd_Str_Alloc (Self : in out Instance) is
      Doesnt_Matter : constant Basic_Types.Byte_Array := [0, 0, 0, 0];
   begin
      -- This is just a noop. Adamant does not implement allocation
      Seq_Position_Assert.Eq (Seq_Runtime.Tester.Call_Str_Alloc (Self.Wb_Runner, Doesnt_Matter), 4);
      State_Assert.Neq (Self.Wb_Runner.Get_State, Error);
      Seq_Runtime.Tester.Reset_Instance (Self.Wb_Runner);
   end Cmd_Str_Alloc;

   -- A test for parsing a bad instruction with invalid field or opcode.
   overriding procedure Bad_Instruction (Self : in out Instance) is
      Ignore_Self : Instance renames Self;
   begin
      -- Test invalid instruction
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 33);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Initialize (1, 1);
         Engine.Set_Source_Id (0);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/1.bin", Buffer);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         -- OK now overwrite first instruction in buffer with bad opcode.
         Buffer (Buffer'First + Sequence_Header.Size_In_Bytes) := 99;
         -- Execute and expect error
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Error);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
         Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Opcode);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;

      -- Test unimplemented instruction
      declare
         Sequence : Memory_Region.T;
         Buffer : Basic_Types.Byte_Array_Access;
         Engine : Seq.Engine;
         Time : constant Sys_Time.T := (0, 33);
      begin
         Buffer := new Basic_Types.Byte_Array (0 .. 16_383);
         Engine.Initialize (1, 1);
         Engine.Set_Source_Id (0);
         Sequence := Load_Sequence_In_Memory ("test_sequences/build/bin/1.bin", Buffer);
         Load_Status_Assert.Eq (Engine.Load (Sequence), Success);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Active);
         -- OK now overwrite first instruction in buffer with unimplemented opcode.
         Buffer (Buffer'First + Sequence_Header.Size_In_Bytes) := 22; -- Kill_Category is unimplemented for Adamant
         -- Execute and expect error
         Exec_State_Assert.Eq (Engine.Execute (100, Time), Seq_Execute_State.Error);
         Engine_State_Assert.Eq (Engine.Get_Engine_State, Engine_Error);
         State_Assert.Eq (Engine.Get_Running_Sequence_State, Error);
         Seq_Error_Code_Assert.Eq (Engine.Get_Seq_Error_Code, Unimplemented);
         Engine.Destroy;
         pragma Unreferenced (Engine);
         Free (Buffer);
      end;
   end Bad_Instruction;

end Seq_Tests.Implementation;
