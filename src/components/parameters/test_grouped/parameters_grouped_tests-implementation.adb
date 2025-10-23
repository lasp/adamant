--------------------------------------------------------------------------------
-- Parameters Grouped Tests Body
--------------------------------------------------------------------------------

with Test_Grouped_Params;
with Test_Grouped_Params_Record; use Test_Grouped_Params_Record;
with Basic_Assertions; use Basic_Assertions;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Parameter_Table_Entry;
with Parameter_Table_Entry_Id.Assertion; use Parameter_Table_Entry_Id.Assertion;
with Packet;
with Interfaces; use Interfaces;
with Serializer_Types; use Serializer_Types;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Parameters_Memory_Region_Release.Assertion; use Parameters_Memory_Region_Release.Assertion;
with Parameter_Enums;
with Basic_Types;
with Parameter_Operation_Status.Assertion; use Parameter_Operation_Status.Assertion;
with Parameter_Entry_Comparison;
with Parameter_Entry_Comparison.Assertion;
with Parameter_Table_Header;
with Crc_16;
with System.Storage_Elements; use System.Storage_Elements;

package body Parameters_Grouped_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3, Parameter_Update_T_Provide_Count => 3);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init here.
      Self.Tester.Component_Instance.Init (Parameter_Table_Entries => Test_Grouped_Params.Parameter_Table_Entries'Access, Dump_Parameters_On_Change => True);

      -- Call the component set up method that the assembly would normally call.
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Grouped_Dump_Parameters (Self : in out Instance) is
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
      Table_Bytes : Test_Grouped_Params_Record.Serialization.Byte_Array;
      Crc : Crc_16.Crc_16_Type;
   begin
      -- Make sure no packets thrown on startup:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send the command to dump the parameters.
      T.Command_T_Send (T.Commands.Dump_Parameters);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameters_Id, Status => Success));

      -- Make sure a packet is produced.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Grouped_Params_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents - verify grouped parameters have the same default values:
      -- Both Component_A and Component_B should have the same values for I32 and U16
      Table_Bytes :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_A_Parameter_U16 => (Value => 15),
               Component_C_The_Tick => ((1, 2), 3)));
      Crc := Crc_16.Compute_Crc_16 (Table_Bytes (Table_Bytes'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table_Bytes'Last));
      Table_Bytes :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => Crc, Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_A_Parameter_U16 => (Value => 15),
               Component_C_The_Tick => ((1, 2), 3)));
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Table_Bytes);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);
   end Test_Grouped_Dump_Parameters;

   overriding procedure Test_Grouped_Update_Parameters (Self : in out Instance) is
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
      Param_Entry : constant Parameter_Table_Entry.T := (Header => (Id => 0, Buffer_Length => 4), Buffer => [0 => 0, 1 => 0, 2 => 0, 3 => 99, others => 0]);
      Cmd : Command.T;
      Table_Bytes : Test_Grouped_Params_Record.Serialization.Byte_Array;
      Crc : Crc_16.Crc_16_Type;
   begin
      -- Send a command to update the grouped I32 parameter (Entry_ID 0):
      -- This should update the shared memory location, so both components see the change
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Update_Success_History.Get_Count, 1);
      Parameter_Table_Entry_Id_Assert.Eq (T.Parameter_Update_Success_History.Get (1), (Id => 0));

      -- A packet should have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Grouped_Params_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents - With grouped parameters sharing memory, the FIRST fetch wins.
      -- We updated Entry_ID 0 (grouped I32 parameters) to 99, so both Component_A and Component_B now have 99.
      -- When we fetch, the fetch order is: ID 2 (Component_A) is fetched first and returns 99.
      -- That value is used in the buffer. ID 4 (Component_B) is fetched second but its value is NOT used (first wins).
      -- So the final packet will have 99. This is the nature of grouped/union parameters with first-fetch-wins behavior.
      Table_Bytes :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => 99), Component_A_Parameter_U16 => (Value => 15),
               Component_C_The_Tick => ((1, 2), 3)));
      Crc := Crc_16.Compute_Crc_16 (Table_Bytes (Table_Bytes'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table_Bytes'Last));
      Table_Bytes :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => Crc, Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => 99), Component_A_Parameter_U16 => (Value => 15),
               Component_C_The_Tick => ((1, 2), 3)));
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Table_Bytes);
   end Test_Grouped_Update_Parameters;

   overriding procedure Test_Grouped_Table_Upload (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data with new values.
      Table : aliased Test_Grouped_Params_Record.Serialization.Byte_Array :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 1.0), Component_A_Parameter_I32 => (Value => 200), Component_A_Parameter_U16 => (Value => 50),
               Component_C_The_Tick => ((5, 6), 7)));
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address + Crc_16.Crc_16_Type'Length, Length => Test_Grouped_Params_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length);
      Pkt : Packet.T;
      Expected_Packet_Data : Test_Grouped_Params_Record.Serialization.Byte_Array := Table;
   begin
      -- Set the CRC:
      Table (Table'First + Crc_16.Crc_16_Type'Length .. Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));
      Table (Table'First .. Table'First + Crc_16.Crc_16_Type'Length - 1) := Crc;
      Expected_Packet_Data (Table'First + Crc_16.Crc_16_Type'Length .. Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));
      Expected_Packet_Data (Expected_Packet_Data'First .. Expected_Packet_Data'First + Crc_16.Crc_16_Type'Length - 1) := Crc;

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Parameter_Table_Update_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Update_History.Get (1), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Update_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Update_History.Get (1), (Region, Success));

      -- A packet should have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Grouped_Params_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents - verify both grouped components got updated with the same values:
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Expected_Packet_Data);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));
   end Test_Grouped_Table_Upload;

   overriding procedure Test_Grouped_Table_Validate (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Test_Grouped_Params_Record.Serialization.Byte_Array :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 1.0), Component_A_Parameter_I32 => (Value => 300), Component_A_Parameter_U16 => (Value => 75),
               Component_C_The_Tick => ((8, 9), 10)));
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address + Crc_16.Crc_16_Type'Length, Length => Test_Grouped_Params_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length);
   begin
      -- Set the CRC:
      Table (Table'First + Crc_16.Crc_16_Type'Length .. Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));
      Table (Table'First .. Table'First + Crc_16.Crc_16_Type'Length - 1) := Crc;

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Validate));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Starting_Parameter_Table_Validate_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Validate_History.Get (1), Region);
      Natural_Assert.Eq (T.Parameter_Validation_Failed_History.Get_Count, 0);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Validate_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Validate_History.Get (1), (Region, Success));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));
   end Test_Grouped_Table_Validate;

   overriding procedure Test_Grouped_Table_Fetch (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- A variable that contains the expected table values to compare against
      Dump : constant Test_Grouped_Params_Record.Serialization.Byte_Array :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_A_Parameter_U16 => (Value => 15),
               Component_C_The_Tick => ((1, 2), 3)));
      Table : Basic_Types.Byte_Array (0 .. Test_Grouped_Params_Record.Size_In_Bytes - Crc_16.Crc_16_Type'Length - 1) := Dump (Dump'First + Crc_16.Crc_16_Type'Length .. Dump'Last);
      -- Create a memory region that will hold the parameter table data.
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Grouped_Params_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length - 1) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Memory'Length);
   begin
      -- Set the CRC:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 0.0));

      -- Send the memory region to the component with a get request:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Parameter_Table_Fetch_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Fetch_History.Get (1), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Fetch_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Fetch_History.Get (1), (Region, Success));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));

      -- Check the memory region contents and make sure it is correct:
      Byte_Array_Assert.Eq (Memory, Table);
   end Test_Grouped_Table_Fetch;

   overriding procedure Test_Grouped_Dump_Parameters_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
   begin
      -- Make sure no packets thrown on startup:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Set Component_A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Validation_Error, Length => 0);

      -- Send the command to dump the parameters.
      T.Command_T_Send (T.Commands.Dump_Parameters);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameters_Id, Status => Failure));

      -- Make sure a packet is produced.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Grouped_Params_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check events - both grouped I32 and U16 should fail for Component_A:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Fetch_Failed_History.Get_Count, 2);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (1), (Operation => Fetch, Status => Validation_Error, Id => 2));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (2), (Operation => Fetch, Status => Validation_Error, Id => 1));

      -- Turn off the override.
      T.Component_A.Disable_Parameter_Return_Override;
   end Test_Grouped_Dump_Parameters_Error;

   overriding procedure Test_Grouped_Update_Parameters_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Param_Entry : constant Parameter_Table_Entry.T := (Header => (Id => 0, Buffer_Length => 4), Buffer => [0 => 0, 1 => 0, 2 => 0, 3 => 99, others => 0]);
      Cmd : Command.T;
   begin
      -- Set Component_A to return a bad status on stage:
      T.Component_A.Override_Parameter_Return (Status => Validation_Error, Length => 0);
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Stage_Failed_History.Get_Count, 1);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Stage_Failed_History.Get (1), (Operation => Stage, Status => Validation_Error, Id => 2));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Turn off the override.
      T.Component_A.Disable_Parameter_Return_Override;
   end Test_Grouped_Update_Parameters_Error;

   overriding procedure Test_Grouped_Table_Upload_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Dump : constant Test_Grouped_Params_Record.Serialization.Byte_Array :=
         Test_Grouped_Params_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 1.0), Component_A_Parameter_I32 => (Value => 99), Component_A_Parameter_U16 => (Value => 19),
               Component_C_The_Tick => ((3, 2), 8)));
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Grouped_Params_Record.Size_In_Bytes - Crc_16.Crc_16_Type'Length - 1) := Dump (Dump'First + Crc_16.Crc_16_Type'Length .. Dump'Last);
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
      Pkt : Packet.T;
   begin
      -- Set the CRC:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));

      -- Set Component_A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Validation_Error, Length => 0);

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events - both grouped parameters should fail:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Parameter_Stage_Failed_History.Get_Count, 2);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Stage_Failed_History.Get (1), (Operation => Stage, Status => Validation_Error, Id => 2));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Stage_Failed_History.Get (2), (Operation => Stage, Status => Validation_Error, Id => 1));
      Natural_Assert.Eq (T.Parameter_Update_Failed_History.Get_Count, 1);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Update_Failed_History.Get (1), (Operation => Update, Status => Validation_Error, Id => 0));
      Natural_Assert.Eq (T.Parameter_Fetch_Failed_History.Get_Count, 2);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (1), (Operation => Fetch, Status => Validation_Error, Id => 2));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (2), (Operation => Fetch, Status => Validation_Error, Id => 1));
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Starting_Parameter_Table_Update_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Update_History.Get (1), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Update_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Update_History.Get (1), (Region, Parameter_Error));

      -- A packet should have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Grouped_Params_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Parameter_Error));

      -- Turn off the override.
      T.Component_A.Disable_Parameter_Return_Override;
   end Test_Grouped_Table_Upload_Error;

   overriding procedure Test_Grouped_Fetch_Value_Mismatch (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      -- Update Entry_ID 0 (grouped I32 parameters) to value 99 for Component_A only.
      -- This will cause Component_A and Component_B to have different values.
      -- Component_A will have 99, Component_B will still have default -56.
      declare
         Param_Entry : constant Parameter_Table_Entry.T := (Header => (Id => 0, Buffer_Length => 4), Buffer => [0 => 0, 1 => 0, 2 => 0, 3 => 99, others => 0]);
      begin
         -- Set Component_B to return an error on stage, so it won't update its value
         T.Component_B.Override_Parameter_Return (Status => Validation_Error, Length => 0);
         pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
         T.Command_T_Send (Cmd);
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- The command should fail because Component_B rejected the stage
         Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
         Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Failure));

         -- Turn off the override so Component_B can be read normally
         T.Component_B.Disable_Parameter_Return_Override;
      end;

      -- Now dump the parameters. This will fetch from both Component_A (value 99) and
      -- Component_B (value -56). The fetch will detect the mismatch.
      T.Command_T_Send (T.Commands.Dump_Parameters);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameters_Id, Status => Success));

      -- Check that the Parameter_Fetch_Value_Mismatch event was produced
      Natural_Assert.Eq (T.Parameter_Fetch_Value_Mismatch_History.Get_Count, 1);

      -- Verify the event contains the correct information:
      -- Entry_Id should be 0 (the grouped I32 parameters)
      -- First_Id should be 2 (Component_A.Parameter_I32, fetched first)
      -- Second_Id should be 4 (Component_B.Parameter_I32, fetched second with different value)
      declare
         use Parameter_Entry_Comparison.Assertion;
         Expected : constant Parameter_Entry_Comparison.T := (Entry_Id => 0, First_Id => 2, Second_Id => 4);
      begin
         Parameter_Entry_Comparison_Assert.Eq (T.Parameter_Fetch_Value_Mismatch_History.Get (1), Expected);
      end;

      -- Verify that a packet was produced with the first fetched value (99 from Component_A)
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);
      declare
         Pkt : constant Packet.T := T.Packet_T_Recv_Sync_History.Get (1);
         Table_Bytes : Test_Grouped_Params_Record.Serialization.Byte_Array :=
            Test_Grouped_Params_Record.Serialization.To_Byte_Array
               ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => 99), Component_A_Parameter_U16 => (Value => 15),
                  Component_C_The_Tick => ((1, 2), 3)));
         Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table_Bytes (Table_Bytes'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table_Bytes'Last));
      begin
         Table_Bytes (Table_Bytes'First .. Table_Bytes'First + Crc_16.Crc_16_Type'Length - 1) := Crc;
         Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Table_Bytes);
      end;
   end Test_Grouped_Fetch_Value_Mismatch;

end Parameters_Grouped_Tests.Implementation;
