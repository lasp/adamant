--------------------------------------------------------------------------------
-- Parameters Tests Body
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with Test_Parameter_Table;
with Parameters_Component_Types;
with Test_Parameter_Table_Record; use Test_Parameter_Table_Record;
with Basic_Assertions; use Basic_Assertions;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Parameter_Table_Entry;
with Parameter_Table_Entry_Id.Assertion; use Parameter_Table_Entry_Id.Assertion;
with Invalid_Parameter_Table_Entry_Length.Assertion; use Invalid_Parameter_Table_Entry_Length.Assertion;
with Packet;
with Interfaces; use Interfaces;
with Serializer_Types; use Serializer_Types;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Parameters_Memory_Region.Assertion; use Parameters_Memory_Region.Assertion;
with Parameters_Memory_Region_Release.Assertion; use Parameters_Memory_Region_Release.Assertion;
with Parameter_Enums;
with Basic_Types;
with Parameter_Operation_Status.Assertion; use Parameter_Operation_Status.Assertion;
with Invalid_Parameter_Length.Assertion; use Invalid_Parameter_Length.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Invalid_Parameters_Memory_Region_Length.Assertion; use Invalid_Parameters_Memory_Region_Length.Assertion;
with Invalid_Parameters_Memory_Region_Crc.Assertion; use Invalid_Parameters_Memory_Region_Crc.Assertion;
with Parameter_Types;
with Parameter_Table_Header;
with Crc_16;
with System.Storage_Elements; use System.Storage_Elements;

package body Parameters_Tests.Implementation is

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
      Self.Tester.Component_Instance.Init (Parameter_Table_Entries => Test_Parameter_Table.Parameter_Table_Entries'Access, Dump_Parameters_On_Change => True);

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

   overriding procedure Test_Init (Self : in out Instance) is
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;

      procedure Init_Nominal is
         -- A list of the parameter table entries for use by the component.
         Parameter_Table_Entries : aliased Parameters_Component_Types.Parameter_Table_Entry_List :=
            [(Id => 2, Entry_Id => 0, Component_Id => 1, Start_Index => 0, End_Index => 3), (Id => 5, Entry_Id => 1, Component_Id => 3, Start_Index => 4, End_Index => 15), (Id => 1, Entry_Id => 2, Component_Id => 1, Start_Index => 16, End_Index => 17),
             (Id => 3, Entry_Id => 3, Component_Id => 2, Start_Index => 18, End_Index => 19), (Id => 4, Entry_Id => 4, Component_Id => 2, Start_Index => 20, End_Index => 23)];
      begin
         T.Component_Instance.Init (Parameter_Table_Entries'Unchecked_Access, False);
      exception
         -- Not expecting exception to be thrown:
         when others =>
            Assert (False, "Nominal init failed!");
      end Init_Nominal;

      procedure Init_Nonunique_Ids is
         -- A list of the parameter table entries for use by the component.
         Parameter_Table_Entries : aliased Parameters_Component_Types.Parameter_Table_Entry_List :=
            [(Id => 2, Entry_Id => 0, Component_Id => 1, Start_Index => 0, End_Index => 3), (Id => 5, Entry_Id => 1, Component_Id => 3, Start_Index => 4, End_Index => 15), (Id => 1, Entry_Id => 2, Component_Id => 1, Start_Index => 16, End_Index => 17),
             (Id => 3, Entry_Id => 3, Component_Id => 2, Start_Index => 18, End_Index => 19), (Id => 2, Entry_Id => 4, Component_Id => 2, Start_Index => 20, End_Index => 23)];
      begin
         T.Component_Instance.Init (Parameter_Table_Entries'Unchecked_Access, False);
         Assert (False, "Unique Id init failed!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Nonunique_Ids;

      procedure Init_Bad_Component_Id is
         -- A list of the parameter table entries for use by the component.
         Parameter_Table_Entries : aliased Parameters_Component_Types.Parameter_Table_Entry_List :=
            [(Id => 2, Entry_Id => 0, Component_Id => 1, Start_Index => 0, End_Index => 3), (Id => 5, Entry_Id => 1, Component_Id => 3, Start_Index => 4, End_Index => 15), (Id => 1, Entry_Id => 2, Component_Id => 1, Start_Index => 16, End_Index => 17),
             (Id => 3, Entry_Id => 3, Component_Id => 2, Start_Index => 18, End_Index => 19), (Id => 4, Entry_Id => 4, Component_Id => 5, Start_Index => 20, End_Index => 23)];
      begin
         T.Component_Instance.Init (Parameter_Table_Entries'Unchecked_Access, False);
         Assert (False, "Component Id init failed!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Bad_Component_Id;

      procedure Init_Unconnected_Component_Id is
         -- A list of the parameter table entries for use by the component.
         Parameter_Table_Entries : aliased Parameters_Component_Types.Parameter_Table_Entry_List :=
            [(Id => 2, Entry_Id => 0, Component_Id => 1, Start_Index => 0, End_Index => 3), (Id => 5, Entry_Id => 1, Component_Id => 3, Start_Index => 4, End_Index => 15), (Id => 1, Entry_Id => 2, Component_Id => 1, Start_Index => 16, End_Index => 17),
             (Id => 3, Entry_Id => 3, Component_Id => 2, Start_Index => 18, End_Index => 19), (Id => 4, Entry_Id => 4, Component_Id => 4, Start_Index => 20, End_Index => 23)];
      begin
         Self.Tear_Down_Test;
         Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3, Parameter_Update_T_Provide_Count => 4);
         Self.Tester.Connect;
         -- Now run the test, which is to call component init with a custom parameter
         -- table entry list that causes it to access index 4, which is unconnected.
         T.Component_Instance.Init (Parameter_Table_Entries'Unchecked_Access, False);
         Assert (False, "Unconnected Component Id init failed!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Unconnected_Component_Id;

      procedure Init_Bad_Layout_1 is
         -- A list of the parameter table entries for use by the component.
         Parameter_Table_Entries : aliased Parameters_Component_Types.Parameter_Table_Entry_List :=
            [(Id => 2, Entry_Id => 0, Component_Id => 1, Start_Index => 2, End_Index => 0), (Id => 5, Entry_Id => 1, Component_Id => 3, Start_Index => 4, End_Index => 15), (Id => 1, Entry_Id => 2, Component_Id => 1, Start_Index => 16, End_Index => 17),
             (Id => 3, Entry_Id => 3, Component_Id => 2, Start_Index => 18, End_Index => 19), (Id => 4, Entry_Id => 4, Component_Id => 2, Start_Index => 20, End_Index => 23)];
      begin
         T.Component_Instance.Init (Parameter_Table_Entries'Unchecked_Access, False);
         Assert (False, "Bad Layout 2 init failed!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Bad_Layout_1;

      procedure Init_Bad_Layout_2 is
         -- A list of the parameter table entries for use by the component.
         Parameter_Table_Entries : aliased Parameters_Component_Types.Parameter_Table_Entry_List :=
            [(Id => 2, Entry_Id => 0, Component_Id => 1, Start_Index => 0, End_Index => 3), (Id => 5, Entry_Id => 1, Component_Id => 3, Start_Index => 4, End_Index => 15), (Id => 1, Entry_Id => 2, Component_Id => 1, Start_Index => 16, End_Index => 17),
             (Id => 3, Entry_Id => 3, Component_Id => 2, Start_Index => 18, End_Index => 19), (Id => 4, Entry_Id => 4, Component_Id => 2, Start_Index => 21, End_Index => 23)];
      begin
         T.Component_Instance.Init (Parameter_Table_Entries'Unchecked_Access, False);
         Assert (False, "Bad Layout 2 init failed!");
      exception
         -- Expecting exception to be thrown:
         when others =>
            null;
      end Init_Bad_Layout_2;
   begin
      -- Test different start-up scenarios:
      Init_Nominal;
      Init_Nonunique_Ids;
      Init_Bad_Component_Id;
      Init_Unconnected_Component_Id;
      Init_Bad_Layout_1;
      Init_Bad_Layout_2;
   end Test_Init;

   overriding procedure Test_Nominal_Dump_Parameters (Self : in out Instance) is
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
      Table_Bytes : Test_Parameter_Table_Record.Serialization.Byte_Array;
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
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Table_Bytes :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 15),
               Component_B_Parameter_I32 => (Value => -56)));
      Crc := Crc_16.Compute_Crc_16 (Table_Bytes (Table_Bytes'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table_Bytes'Last));
      Table_Bytes :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => Crc, Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 15),
               Component_B_Parameter_I32 => (Value => -56)));
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Table_Bytes);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);

      -- Send the command to dump the parameters again.
      T.Command_T_Send (T.Commands.Dump_Parameters);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameters_Id, Status => Success));

      -- Make sure a packet is produced.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (2);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 1);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Table_Bytes);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 2);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 2);
   end Test_Nominal_Dump_Parameters;

   overriding procedure Test_Nominal_Update_Parameters (Self : in out Instance) is
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
      Param_Entry : Parameter_Table_Entry.T := (Header => (Id => 3, Buffer_Length => 2), Buffer => [0 => 0, 1 => 17, others => 0]);
      Cmd : Command.T;
      Table_Bytes : Test_Parameter_Table_Record.Serialization.Byte_Array;
      Crc : Crc_16.Crc_16_Type;
   begin
      -- Send a command to update a parameter value:
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
      Parameter_Table_Entry_Id_Assert.Eq (T.Parameter_Update_Success_History.Get (1), (Id => 3));

      -- A packet should have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Table_Bytes :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 17),
               Component_B_Parameter_I32 => (Value => -56)));
      Crc := Crc_16.Compute_Crc_16 (Table_Bytes (Table_Bytes'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table_Bytes'Last));
      Table_Bytes :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => Crc, Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 17),
               Component_B_Parameter_I32 => (Value => -56)));
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Table_Bytes);

      -- Send another command to update a parameter value (Entry_ID 0):
      Param_Entry := (Header => (Id => 0, Buffer_Length => 4), Buffer => [0 => 0, 1 => 0, 2 => 0, 3 => 99, others => 0]);
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 2);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 2);
      Natural_Assert.Eq (T.Parameter_Update_Success_History.Get_Count, 2);
      Parameter_Table_Entry_Id_Assert.Eq (T.Parameter_Update_Success_History.Get (2), (Id => 0));

      -- A packet should have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (2);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 1);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Table_Bytes :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => 99), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 17),
               Component_B_Parameter_I32 => (Value => -56)));
      Crc := Crc_16.Compute_Crc_16 (Table_Bytes (Table_Bytes'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table_Bytes'Last));
      Table_Bytes :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => Crc, Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => 99), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 17),
               Component_B_Parameter_I32 => (Value => -56)));
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Table_Bytes);
   end Test_Nominal_Update_Parameters;

   overriding procedure Test_Nominal_Table_Upload (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Test_Parameter_Table_Record.Serialization.Byte_Array :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 1.0), Component_A_Parameter_I32 => (Value => 99), Component_C_The_Tick => ((3, 2), 8), Component_A_Parameter_U16 => (Value => 19), Component_B_Parameter_U16 => (Value => 12),
               Component_B_Parameter_I32 => (Value => -22)));
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address + Crc_16.Crc_16_Type'Length, Length => Test_Parameter_Table_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length);
      Pkt : Packet.T;
      Expected_Packet_Data : Test_Parameter_Table_Record.Serialization.Byte_Array := Table;
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
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1), Expected_Packet_Data);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));
   end Test_Nominal_Table_Upload;

   overriding procedure Test_Nominal_Table_Validate (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Test_Parameter_Table_Record.Serialization.Byte_Array :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 1.0), Component_A_Parameter_I32 => (Value => 99), Component_C_The_Tick => ((3, 2), 8), Component_A_Parameter_U16 => (Value => 19), Component_B_Parameter_U16 => (Value => 12),
               Component_B_Parameter_I32 => (Value => -22)));
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address + Crc_16.Crc_16_Type'Length, Length => Test_Parameter_Table_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length);
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
   end Test_Nominal_Table_Validate;

   overriding procedure Test_Nominal_Table_Fetch (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- A variable that contains the expected table values to compare against
      Dump : constant Test_Parameter_Table_Record.Serialization.Byte_Array :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 15),
               Component_B_Parameter_I32 => (Value => -56)));
      Table : Basic_Types.Byte_Array (0 .. Test_Parameter_Table_Record.Size_In_Bytes - Crc_16.Crc_16_Type'Length - 1) := Dump (Dump'First + Crc_16.Crc_16_Type'Length .. Dump'Last);
      -- Create a memory region that will hold the parameter table data.
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Parameter_Table_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length - 1) := [others => 0];
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
   end Test_Nominal_Table_Fetch;

   overriding procedure Test_Dump_Parameters_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
   begin
      -- Make sure no packets thrown on startup:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Set A to return a bad status:
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
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Byte_Array_Assert.Neq
         (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1),
          Test_Parameter_Table_Record.Serialization.To_Byte_Array
             ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 15),
                Component_B_Parameter_I32 => (Value => -56))));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Fetch_Failed_History.Get_Count, 2);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (1), (Operation => Fetch, Status => Validation_Error, Id => 2));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (2), (Operation => Fetch, Status => Validation_Error, Id => 1));

      -- Set A to return a bad length:
      T.Component_A.Override_Parameter_Return (Status => Success, Length => 0);

      -- Send the command to dump the parameters.
      T.Command_T_Send (T.Commands.Dump_Parameters);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameters_Id, Status => Failure));

      -- Make sure a packet is produced.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (2);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 1);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Byte_Array_Assert.Neq
         (Pkt.Buffer (0 .. Pkt.Header.Buffer_Length - 1),
          Test_Parameter_Table_Record.Serialization.To_Byte_Array
             ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 15),
                Component_B_Parameter_I32 => (Value => -56))));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 2);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 2);
      Natural_Assert.Eq (T.Parameter_Fetch_Failed_History.Get_Count, 2);
      Natural_Assert.Eq (T.Parameter_Fetch_Length_Mismatch_History.Get_Count, 2);
      Invalid_Parameter_Length_Assert.Eq (T.Parameter_Fetch_Length_Mismatch_History.Get (1), ((Id => 2, Buffer_Length => 0), Expected_Length => 4));
      Invalid_Parameter_Length_Assert.Eq (T.Parameter_Fetch_Length_Mismatch_History.Get (2), ((Id => 1, Buffer_Length => 0), Expected_Length => 2));

      -- Turn off the override.
      T.Component_A.Disable_Parameter_Return_Override;
   end Test_Dump_Parameters_Error;

   overriding procedure Test_Update_Parameters_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
      Param_Entry : Parameter_Table_Entry.T := (Header => (Id => 99, Buffer_Length => 2), Buffer => [0 => 0, 1 => 17, others => 0]);
      Cmd : Command.T;
   begin
      -- Send a command to update a parameter value with bad ID.
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Update_Id_Not_Recognized_History.Get_Count, 1);
      Parameter_Table_Entry_Id_Assert.Eq (T.Parameter_Update_Id_Not_Recognized_History.Get (1), (Id => 99));

      -- Send a command to update a parameter value with bad length.
      Param_Entry.Header.Id := 3;
      Param_Entry.Header.Buffer_Length := 30;
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Parameter_Update_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameter_Table_Entry_Length_Assert.Eq (T.Parameter_Update_Length_Mismatch_History.Get (1), (Header => Param_Entry.Header, Expected_Length => 2));

      -- No packets should have been dumped as a result of the above:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send a command to update a parameter value with good values. This time we make a component return a bad status on stage:
      Param_Entry.Header.Id := 0;
      Param_Entry.Header.Buffer_Length := 4;
      -- Set A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Validation_Error, Length => 0);
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 3);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (3), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Parameter_Stage_Failed_History.Get_Count, 1);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Stage_Failed_History.Get (1), (Operation => Stage, Status => Validation_Error, Id => 2));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send a command to update a parameter value with good values. This time we make a component return a bad status on update:
      -- Set A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Id_Error, Length => 0, Only_On_Update => True);
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 4);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (4), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Parameter_Update_Failed_History.Get_Count, 1);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Update_Failed_History.Get (1), (Operation => Update, Status => Id_Error, Id => 0));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send a command to update a parameter value with good values. This time we make a component return a bad status on fetch:
      -- Set A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Id_Error, Length => 0, Only_On_Fetch => True);
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 5);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (5), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Failure));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Finished_Dumping_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Fetch_Failed_History.Get_Count, 2);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (1), (Operation => Fetch, Status => Id_Error, Id => 2));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (2), (Operation => Fetch, Status => Id_Error, Id => 1));

      -- A packet should have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);
   end Test_Update_Parameters_Error;

   overriding procedure Test_Table_Upload_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Dump : constant Test_Parameter_Table_Record.Serialization.Byte_Array :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 1.0), Component_A_Parameter_I32 => (Value => 99), Component_C_The_Tick => ((3, 2), 8), Component_A_Parameter_U16 => (Value => 19), Component_B_Parameter_U16 => (Value => 12),
               Component_B_Parameter_I32 => (Value => -22)));
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Parameter_Table_Record.Size_In_Bytes - Crc_16.Crc_16_Type'Length - 1) := Dump (Dump'First + Crc_16.Crc_16_Type'Length .. Dump'Last);
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
      Pkt : Packet.T;
   begin
      -- Set the CRC:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));

      -- Set A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Validation_Error, Length => 0);

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
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
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Test_Parameter_Table_Record.Size_In_Bytes);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Parameter_Error));

      --
      -- Send the memory region to the component with a get request, but with bad CRC:
      --
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [6, 7], Version => 1.0));
      T.Parameters_Memory_Region_T_Send ((Region => (Address => Table'Address, Length => Table'Length), Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 10);
      Natural_Assert.Eq (T.Memory_Region_Crc_Invalid_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Crc_Assert.Eq
         (T.Memory_Region_Crc_Invalid_History.Get (1), (Parameters_Region => (Region => (Address => Table'Address, Length => Table'Length), Operation => Set), Header => (Crc_Table => [6, 7], Version => 1.0), Computed_Crc => Crc));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (2), ((Address => Table'Address, Length => Table'Length), Crc_Error));
   end Test_Table_Upload_Error;

   overriding procedure Test_Table_Validate_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Dump : constant Test_Parameter_Table_Record.Serialization.Byte_Array :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 1.0), Component_A_Parameter_I32 => (Value => 99), Component_C_The_Tick => ((3, 2), 8), Component_A_Parameter_U16 => (Value => 19), Component_B_Parameter_U16 => (Value => 12),
               Component_B_Parameter_I32 => (Value => -22)));
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Parameter_Table_Record.Size_In_Bytes - Crc_16.Crc_16_Type'Length - 1) := Dump (Dump'First + Crc_16.Crc_16_Type'Length .. Dump'Last);
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
   begin
      -- Set the CRC:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));

      -- Set A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Validation_Error, Length => 0);

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Validate));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 5);
      Natural_Assert.Eq (T.Parameter_Stage_Failed_History.Get_Count, 2);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Stage_Failed_History.Get (1), (Operation => Stage, Status => Validation_Error, Id => 2));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Stage_Failed_History.Get (2), (Operation => Stage, Status => Validation_Error, Id => 1));
      Natural_Assert.Eq (T.Parameter_Validation_Failed_History.Get_Count, 1);
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Validation_Failed_History.Get (1), (Operation => Validate, Status => Validation_Error, Id => 0));
      Natural_Assert.Eq (T.Starting_Parameter_Table_Validate_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Validate_History.Get (1), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Validate_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Validate_History.Get (1), (Region, Parameter_Error));

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Parameter_Error));

      --
      -- Send the memory region to the component with a get request, but with bad CRC:
      --
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [6, 7], Version => 1.0));
      T.Parameters_Memory_Region_T_Send ((Region => (Address => Table'Address, Length => Table'Length), Operation => Validate));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 6);
      Natural_Assert.Eq (T.Memory_Region_Crc_Invalid_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Crc_Assert.Eq
         (T.Memory_Region_Crc_Invalid_History.Get (1), (Parameters_Region => (Region => (Address => Table'Address, Length => Table'Length), Operation => Validate), Header => (Crc_Table => [6, 7], Version => 1.0), Computed_Crc => Crc));
      Natural_Assert.Eq (T.Starting_Parameter_Table_Validate_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Validate_History.Get (1), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Validate_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Validate_History.Get (1), (Region, Parameter_Error));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (2), ((Address => Table'Address, Length => Table'Length), Crc_Error));
   end Test_Table_Validate_Error;

   overriding procedure Test_Table_Fetch_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Update_Status;
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Operation_Type;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- A variable that contains the expected table values to compare against
      Table : aliased Test_Parameter_Table_Record.Serialization.Byte_Array :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => -56), Component_C_The_Tick => ((1, 2), 3), Component_A_Parameter_U16 => (Value => 15), Component_B_Parameter_U16 => (Value => 15),
               Component_B_Parameter_I32 => (Value => -56)));
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Crc_16.Crc_16_Type'Length + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      -- Create a memory region that will hold the parameter table data.
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Parameter_Table_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length - 1) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Memory'Length);
   begin
      -- Set the CRC:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 0.0));

      -- Set A to return a bad status:
      T.Component_A.Override_Parameter_Return (Status => Validation_Error, Length => 0);

      --
      -- Send the memory region to the component with a get request:
      --
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 4);
      Natural_Assert.Eq (T.Parameter_Fetch_Failed_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Parameter_Table_Fetch_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Fetch_History.Get (1), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Fetch_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Fetch_History.Get (1), (Region, Parameter_Error));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (1), (Operation => Fetch, Status => Validation_Error, Id => 2));
      Parameter_Operation_Status_Assert.Eq (T.Parameter_Fetch_Failed_History.Get (2), (Operation => Fetch, Status => Validation_Error, Id => 1));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Parameter_Error));

      -- Check the memory region contents and make sure it is correct:
      Byte_Array_Assert.Neq (Memory, Table);

      -- Set A to return a bad length:
      T.Component_A.Override_Parameter_Return (Status => Success, Length => 0);

      -- Send the memory region to the component with a get request:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);
      Natural_Assert.Eq (T.Parameter_Fetch_Failed_History.Get_Count, 2);
      Natural_Assert.Eq (T.Starting_Parameter_Table_Fetch_History.Get_Count, 2);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Fetch_History.Get (2), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Fetch_History.Get_Count, 2);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Fetch_History.Get (2), (Region, Parameter_Error));
      Natural_Assert.Eq (T.Parameter_Fetch_Length_Mismatch_History.Get_Count, 2);
      Invalid_Parameter_Length_Assert.Eq (T.Parameter_Fetch_Length_Mismatch_History.Get (1), ((Id => 2, Buffer_Length => 0), Expected_Length => 4));
      Invalid_Parameter_Length_Assert.Eq (T.Parameter_Fetch_Length_Mismatch_History.Get (2), ((Id => 1, Buffer_Length => 0), Expected_Length => 2));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (2), (Region, Parameter_Error));

      -- Check the memory region contents and make sure it is correct:
      Byte_Array_Assert.Neq (Memory, Table);

      -- Turn off the override.
      T.Component_A.Disable_Parameter_Return_Override;

      --
      -- Send the memory region to the component with a get request, but with the wrong length:
      --
      T.Parameters_Memory_Region_T_Send ((Region => (Address => Memory'Address, Length => Memory'Length + 1), Operation => Get));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 9);
      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get (1), (Parameters_Region => (Region => (Address => Memory'Address, Length => Memory'Length + 1), Operation => Get), Expected_Length => Memory'Length));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 3);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (3), ((Address => Memory'Address, Length => Memory'Length + 1), Length_Error));

      -- Check the memory region contents and make sure it is correct:
      Byte_Array_Assert.Neq (Memory, Table);
   end Test_Table_Fetch_Error;

   overriding procedure Test_No_Dump_On_Change (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Param_Entry : constant Parameter_Table_Entry.T := (Header => (Id => 3, Buffer_Length => 2), Buffer => [0 => 0, 1 => 17, others => 0]);
      Cmd : Command.T;
      -- Create a memory region that holds the parameter table data.
      Dump : constant Test_Parameter_Table_Record.Serialization.Byte_Array :=
         Test_Parameter_Table_Record.Serialization.To_Byte_Array
            ((Crc_Calculated => [0, 0], Header => (Crc_Table => [0, 0], Version => 0.0), Component_A_Parameter_I32 => (Value => 99), Component_C_The_Tick => ((3, 2), 8), Component_A_Parameter_U16 => (Value => 19), Component_B_Parameter_U16 => (Value => 12),
               Component_B_Parameter_I32 => (Value => -22)));
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Parameter_Table_Record.Size_In_Bytes - Crc_16.Crc_16_Type'Length - 1) := Dump (Dump'First + Crc_16.Crc_16_Type'Length .. Dump'Last);
      Crc : constant Crc_16.Crc_16_Type := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
   begin
      -- Set the CRC:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 0.0));

      -- Call component with dump_Parameters_On_Change => False
      T.Component_Instance.Init (Parameter_Table_Entries => Test_Parameter_Table.Parameter_Table_Entries'Access, Dump_Parameters_On_Change => False);

      -- Send a command to update a parameter value:
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Update_Parameter_Id, Status => Success));

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Update_Success_History.Get_Count, 1);
      Parameter_Table_Entry_Id_Assert.Eq (T.Parameter_Update_Success_History.Get (1), (Id => 3));

      -- A packet should NOT have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);
      Natural_Assert.Eq (T.Starting_Parameter_Table_Update_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Starting_Parameter_Table_Update_History.Get (1), Region);
      Natural_Assert.Eq (T.Finished_Parameter_Table_Update_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Finished_Parameter_Table_Update_History.Get (1), (Region, Success));

      -- A packet should NOT have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));
   end Test_No_Dump_On_Change;

   overriding procedure Test_Full_Queue (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create maximum size parameter to fill queue.
      Param_Entry : constant Parameter_Table_Entry.T := (Header => (Id => 3, Buffer_Length => Parameter_Types.Parameter_Buffer_Length_Type'Last), Buffer => [0 => 0, 1 => 17, others => 0]);
      Cmd : Command.T;
      -- Create a memory region that will hold the parameter table data.
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Parameter_Table_Record.Serialization.Serialized_Length - Crc_16.Crc_16_Type'Length - 1) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address + Crc_16.Crc_16_Type'Length, Length => Memory'Length);
   begin
      -- Send 3 commands to fill up queue.
      pragma Assert (T.Commands.Update_Parameter (Param_Entry, Cmd) = Success);
      Cmd.Header.Arg_Buffer_Length := Cmd.Arg_Buffer'Length;
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);

      -- OK the next command should overflow the queue.
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (Cmd);

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Command_Dropped_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Dropped_History.Get (1), Cmd.Header);

      -- Send the memory region to the component with a get request, should
      -- overflow queue.
      T.Expect_Parameters_Memory_Region_T_Send_Dropped := True;
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Region_Dropped_History.Get_Count, 1);
      Parameters_Memory_Region_Assert.Eq (T.Memory_Region_Dropped_History.Get (1), (Region => Region, Operation => Get));

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Dropped));
   end Test_Full_Queue;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Parameters.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Dump_Parameters;
   begin
      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 22;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameters_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Dump_Parameters_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 22]));
   end Test_Invalid_Command;

end Parameters_Tests.Implementation;
