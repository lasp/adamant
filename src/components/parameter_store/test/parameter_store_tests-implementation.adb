--------------------------------------------------------------------------------
-- Parameter_Store Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command;
with Packet;
with Interfaces; use Interfaces;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Parameters_Memory_Region.Assertion; use Parameters_Memory_Region.Assertion;
with Parameters_Memory_Region_Release.Assertion; use Parameters_Memory_Region_Release.Assertion;
with Parameter_Enums;
with Basic_Types;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Command_Header.Assertion; use Command_Header.Assertion;
with Invalid_Parameters_Memory_Region_Length.Assertion; use Invalid_Parameters_Memory_Region_Length.Assertion;
with Invalid_Parameters_Memory_Region_Crc.Assertion; use Invalid_Parameters_Memory_Region_Crc.Assertion;
with Parameter_Table_Header;
with Crc_16;
with Byte_Array_Pointer;
with Memory_Packetizer_Types;
with System;

package body Parameter_Store_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals:
   -------------------------------------------------------------------------
   -- Declare memory store data:
   Bytes : aliased Basic_Types.Byte_Array := [0 .. 99 => 0];

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);

      -- Call component init here.
      Bytes := [others => 0];
      Bytes (1) := 1;
      Bytes (2) := 2;
      Bytes (3) := 3;
      Bytes (4) := 4;
      Bytes (5) := 5;
      Self.Tester.Component_Instance.Init (Bytes => Bytes'Access, Dump_Parameters_On_Change => True);

      -- NOTE: Connect + Component_Instance.Set_Up are intentionally NOT called
      -- here because Set_Up asserts exactly-one-of Packet_T_Send /
      -- Memory_Dump_Send is connected. Each test selects its dump pathway by
      -- calling either Self.Tester.Connect (Packet.T path)
      -- or Self.Tester.Connect_Memory_Dump_Path (Memory_Dump path), then
      -- Self.Tester.Component_Instance.Set_Up.
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      -- Free component heap:
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Nominal_Dump_Parameters (Self : in out Instance) is
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Packet.T;
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Make sure no packets thrown on startup:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Send the command to dump the parameters.
      T.Command_T_Send (T.Commands.Dump_Parameter_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameter_Store_Id, Status => Success));

      -- Make sure a packet is produced.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Bytes'Length + Crc_16.Crc_16_Type'Length);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Crc_16.Crc_16_Type'Length - 1), Crc_16.Compute_Crc_16 (Bytes (Crc_16.Crc_16_Type'Length .. Bytes'Last))); -- Check crc
      Byte_Array_Assert.Eq (Pkt.Buffer (Crc_16.Crc_16_Type'Length .. Pkt.Header.Buffer_Length - 1), Bytes);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 1);

      -- Send the command to dump the parameters again.
      T.Command_T_Send (T.Commands.Dump_Parameter_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 2);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (2), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameter_Store_Id, Status => Success));

      -- Make sure a packet is produced.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 2);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (2);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Bytes'Length + Crc_16.Crc_16_Type'Length);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 1);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Byte_Array_Assert.Eq (Pkt.Buffer (Crc_16.Crc_16_Type'Length .. Pkt.Header.Buffer_Length - 1), Bytes);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 2);
   end Test_Nominal_Dump_Parameters;

   overriding procedure Test_Nominal_Table_Upload (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Basic_Types.Byte_Array := [0 .. 99 => 17];
      Crc : Crc_16.Crc_16_Type;
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
      Pkt : Packet.T;
      Expected_Packet_Data : Basic_Types.Byte_Array (0 .. 99) := Table;
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Set the version:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));
      Expected_Packet_Data (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));

      -- Set the CRC:
      Crc := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));
      Expected_Packet_Data (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Parameter_Table_Updated_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Updated_History.Get (1), Region);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 1);

      -- A packet should have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 1);

      -- Check packet length and contents:
      Pkt := T.Packet_T_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Pkt.Header.Buffer_Length, Bytes'Length + Crc_16.Crc_16_Type'Length);
      Natural_Assert.Eq (Natural (Pkt.Header.Sequence_Count), 0);
      Natural_Assert.Eq (Natural (Pkt.Header.Id), 0);

      -- Check packet contents:
      Byte_Array_Assert.Eq (Pkt.Buffer (0 .. Crc_16.Crc_16_Type'Length - 1), Crc); -- Check crc
      Byte_Array_Assert.Eq (Pkt.Buffer (Crc_16.Crc_16_Type'Length .. Pkt.Header.Buffer_Length - 1), Expected_Packet_Data);

      -- Check component's internal memory contents:
      Byte_Array_Assert.Eq (Bytes, Expected_Packet_Data);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));
   end Test_Nominal_Table_Upload;

   overriding procedure Test_Nominal_Table_Fetch (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that will hold the parameter table data.
      Memory : aliased Basic_Types.Byte_Array (0 .. 99) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Memory'Length);
      Crc : Crc_16.Crc_16_Type;
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Set the version:
      Bytes := [others => 17];
      Bytes (Bytes'First .. Bytes'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));

      -- Set the CRC:
      Crc := Crc_16.Compute_Crc_16 (Bytes (Bytes'First + Parameter_Table_Header.Crc_Section_Length .. Bytes'Last));
      Bytes (Bytes'First .. Bytes'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));

      -- Send the memory region to the component with a get request:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Table_Fetched_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Fetched_History.Get (1), Region);

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));

      -- Check the memory region contents and make sure it is correct:
      Byte_Array_Assert.Eq (Memory, Bytes);
   end Test_Nominal_Table_Fetch;

   overriding procedure Test_Table_Fetch_Pointer (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- The Get_Pointer operation ignores the caller-provided region. Pass an
      -- empty region to make that contract explicit at the test boundary.
      Empty_Region : constant Memory_Region.T := (Address => System.Null_Address, Length => 0);
      Released : Parameters_Memory_Region_Release.T;
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Plant a known pattern in the parameter store's managed bytes:
      Bytes := [others => 17];

      -- Issue Get_Pointer:
      T.Parameters_Memory_Region_T_Send ((Region => Empty_Region, Operation => Get_Pointer));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Exactly one Parameter_Table_Pointer_Fetched event fired with the returned region;
      -- the Get_Copy event must NOT have been emitted on this pathway:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Table_Pointer_Fetched_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Table_Fetched_History.Get_Count, 0);
      Memory_Region_Assert.Eq (
         T.Parameter_Table_Pointer_Fetched_History.Get (1),
         (Address => Bytes'Address, Length => Bytes'Length)
      );

      -- The release region must point at the managed bytes (zero-copy):
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Released := T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1);
      Parameters_Memory_Region_Release_Assert.Eq (
         Released,
         ((Address => Bytes'Address, Length => Bytes'Length), Success)
      );

      -- Reading through the returned region reflects the planted pattern:
      declare
         Returned_Bytes : constant Basic_Types.Byte_Array (0 .. Released.Region.Length - 1)
            with Import, Convention => Ada, Address => Released.Region.Address;
      begin
         Byte_Array_Assert.Eq (Returned_Bytes, Bytes);
      end;

      -- A Get_Pointer does not auto-dump:
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
   end Test_Table_Fetch_Pointer;

   overriding procedure Test_Table_Upload_Length_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Basic_Types.Byte_Array := [0 .. 99 => 17];
      Region : Memory_Region.T := (Address => Table'Address, Length => Table'Length - 1);
      Before_Bytes : Basic_Types.Byte_Array (0 .. 99);
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Save the current memory region so we can make sure it doesn't get changed:
      Before_Bytes := Bytes;

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get (1), (Parameters_Region => (Region => (Address => Table'Address, Length => Table'Length - 1), Operation => Set), Expected_Length => Bytes'Length));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check component's internal memory contents:
      Byte_Array_Assert.Eq (Bytes, Before_Bytes);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Length_Error));

      -- Change region to be too big:
      Region := (Address => Table'Address, Length => Table'Length + 1);

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 2);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get (2), (Parameters_Region => (Region => (Address => Table'Address, Length => Table'Length + 1), Operation => Set), Expected_Length => Bytes'Length));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check component's internal memory contents:
      Byte_Array_Assert.Eq (Bytes, Before_Bytes);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 2);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (2), (Region, Length_Error));
   end Test_Table_Upload_Length_Error;

   overriding procedure Test_Table_Upload_Crc_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Basic_Types.Byte_Array := [0 .. 99 => 17];
      Crc : Crc_16.Crc_16_Type;
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
      Before_Bytes : Basic_Types.Byte_Array (0 .. 99);
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Save the current memory region so we can make sure it doesn't get changed:
      Before_Bytes := Bytes;

      -- Calculate and set an invalid crc:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));
      Crc := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [98, 97], Version => 1.0));

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Crc_Invalid_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Crc_Assert.Eq
         (T.Memory_Region_Crc_Invalid_History.Get (1), (Parameters_Region => (Region => (Address => Table'Address, Length => Table'Length), Operation => Set), Header => (Crc_Table => [98, 97], Version => 1.0), Computed_Crc => Crc));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check component's internal memory contents:
      Byte_Array_Assert.Eq (Bytes, Before_Bytes);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Crc_Error));

   end Test_Table_Upload_Crc_Error;

   -- This unit test tests the behavior when validation of the parameter table by
   -- memory region upload fails.
   overriding procedure Test_Table_Validate_Unimplemented (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Basic_Types.Byte_Array := [0 .. 99 => 17];
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Validate));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Table_Validation_Not_Supported_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Table_Validation_Not_Supported_History.Get (1), Region);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Parameter_Error));
   end Test_Table_Validate_Unimplemented;

   overriding procedure Test_Table_Fetch_Length_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Basic_Types.Byte_Array := [0 .. 99 => 17];
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length - 1);
      Before_Bytes : Basic_Types.Byte_Array (0 .. 99);
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Save the current memory region so we can make sure it doesn't get changed:
      Before_Bytes := Bytes;

      -- Send the memory region to the component with a region that is too small:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get (1), (Parameters_Region => (Region => (Address => Table'Address, Length => Table'Length - 1), Operation => Get_Copy), Expected_Length => Bytes'Length));

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check component's internal memory contents:
      Byte_Array_Assert.Eq (Bytes, Before_Bytes);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Length_Error));
   end Test_Table_Fetch_Length_Error;

   overriding procedure Test_Table_Fetch_Oversized_Region (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that is LARGER than the parameter table.
      Memory : aliased Basic_Types.Byte_Array (0 .. 149) := [others => 55];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Memory'Length);
      -- The expected returned region should have the length of the actual table, not the oversized region.
      Expected_Returned_Region : constant Memory_Region.T := (Address => Memory'Address, Length => Bytes'Length);
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Set up Bytes with known data so we can verify the copy:
      Bytes := [others => 17];
      Bytes (Bytes'First .. Bytes'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));

      -- Send the oversized memory region to the component with a Get request:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events, should succeed, not a length mismatch:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Table_Fetched_History.Get_Count, 1);
      -- The event should report the adjusted (actual table) length, not the original oversized length:
      Memory_Region_Assert.Eq (T.Parameter_Table_Fetched_History.Get (1), Expected_Returned_Region);

      -- No length mismatch events should have been thrown:
      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 0);

      -- A packet should not have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Make sure the memory location was released with the proper status and the adjusted length:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Expected_Returned_Region, Success));

      -- Check that the first Bytes'Length bytes of the memory region were correctly filled:
      Byte_Array_Assert.Eq (Memory (0 .. Bytes'Length - 1), Bytes);

      -- Check that the remaining bytes beyond the table size were NOT modified (still 55):
      Byte_Array_Assert.Eq (Memory (Bytes'Length .. Memory'Last), [Bytes'Length .. 149 => 55]);
   end Test_Table_Fetch_Oversized_Region;

   overriding procedure Test_No_Dump_On_Change (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Create a memory region that holds the parameter table data.
      Table : aliased Basic_Types.Byte_Array := [0 .. 99 => 17];
      Crc : Crc_16.Crc_16_Type;
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
      Expected_Packet_Data : Basic_Types.Byte_Array (0 .. 99) := Table;
   begin
      -- Reinitialize the component with dump parameters on change set to false:
      Self.Tester.Component_Instance.Init (Bytes => Bytes'Access, Dump_Parameters_On_Change => False);

      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Set the version:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));
      Expected_Packet_Data (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));

      -- Set the CRC:
      Crc := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));
      Expected_Packet_Data (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) := Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));

      -- Send the memory region to the component:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Check events:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameter_Table_Updated_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Updated_History.Get (1), Region);

      -- A packet should NOT have been automatically dumped.
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Check component's internal memory contents:
      Byte_Array_Assert.Eq (Bytes, Expected_Packet_Data);

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Success));
   end Test_No_Dump_On_Change;

   overriding procedure Test_Full_Queue (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      -- Create a memory region that will hold the parameter table data.
      Memory : aliased Basic_Types.Byte_Array (0 .. 99) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Memory'Length);
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

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
      Natural_Assert.Eq (T.Command_Dropped_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Dropped_History.Get (1), Cmd.Header);

      -- Send the memory region to the component with a get request, should
      -- overflow queue.
      T.Expect_Parameters_Memory_Region_T_Send_Dropped := True;
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));

      -- Make sure event thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Memory_Region_Dropped_History.Get_Count, 1);
      Parameters_Memory_Region_Assert.Eq (T.Memory_Region_Dropped_History.Get (1), (Region => Region, Operation => Get_Copy));

      -- Make sure the memory location was released with the proper status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1), (Region, Dropped));
   end Test_Full_Queue;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Dump_Parameter_Store;
   begin
      -- Wire the Packet.T dump pathway and run Set_Up:
      T.Connect;
      T.Component_Instance.Set_Up;

      -- Make the command invalid by modifying its length.
      Cmd.Header.Arg_Buffer_Length := 22;

      -- Send bad command and expect bad response:
      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameter_Store_Id, Status => Length_Error));

      -- Make sure some events were thrown:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (T.Invalid_Command_Received_History.Get (1), (Id => T.Commands.Get_Dump_Parameter_Store_Id, Errant_Field_Number => Interfaces.Unsigned_32'Last, Errant_Field => [0, 0, 0, 0, 0, 0, 0, 22]));
   end Test_Invalid_Command;

   overriding procedure Test_Memory_Dump_Path (Self : in out Instance) is
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      Dump : Memory_Packetizer_Types.Memory_Dump;
   begin
      -- Wire the Memory_Dump dump pathway (no Packet_T_Send) and run Set_Up:
      T.Connect_Memory_Dump_Path;
      T.Component_Instance.Set_Up;

      -- No traffic on either pathway yet:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      -- Issue the dump command:
      T.Command_T_Send (T.Commands.Dump_Parameter_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (
         T.Command_Response_T_Recv_Sync_History.Get (1),
         (Source_Id => 0, Registration_Id => 0, Command_Id => T.Commands.Get_Dump_Parameter_Store_Id, Status => Success));

      -- Exactly one Memory_Dump emitted; no Packet.T on this path:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      Dump := T.Memory_Dump_Recv_Sync_History.Get (1);

      -- Stored_Parameters APID == 0 (only packet defined for this component).
      Natural_Assert.Eq (Natural (Dump.Id), 0);
      -- The Memory_Dump points at the full parameter table buffer (zero-copy):
      Natural_Assert.Eq (Byte_Array_Pointer.Length (Dump.Memory_Pointer), Bytes'Length);
      Byte_Array_Assert.Eq (Byte_Array_Pointer.To_Byte_Array (Dump.Memory_Pointer), Bytes);

      -- Dumped_Parameters event fired once:
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 1);

      -- A second dump command bumps everything by one:
      T.Command_T_Send (T.Commands.Dump_Parameter_Store);
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 2);
   end Test_Memory_Dump_Path;

   overriding procedure Test_Memory_Dump_Path_Auto_Dump_On_Change (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Store.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Build a valid (CRC-correct) parameter table:
      Table : aliased Basic_Types.Byte_Array := [0 .. 99 => 17];
      Crc : Crc_16.Crc_16_Type;
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Table'Length);
      Expected_Table_Bytes : Basic_Types.Byte_Array (0 .. 99) := Table;
      Dump : Memory_Packetizer_Types.Memory_Dump;
   begin
      -- Wire the Memory_Dump dump pathway (no Packet_T_Send) and run Set_Up:
      T.Connect_Memory_Dump_Path;
      T.Component_Instance.Set_Up;

      -- Stamp version + CRC into both the staging table and the comparison
      -- copy so the component's bytes match Expected_Table_Bytes after the Set:
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) :=
         Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => 1.0));
      Crc := Crc_16.Compute_Crc_16 (Table (Table'First + Parameter_Table_Header.Crc_Section_Length .. Table'Last));
      Table (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) :=
         Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));
      Expected_Table_Bytes (Table'First .. Table'First + Parameter_Table_Header.Size_In_Bytes - 1) :=
         Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Crc, Version => 1.0));

      -- Send the memory region (Set) -- fixture has Dump_Parameters_On_Change => True,
      -- so this should auto-dump through the Memory_Dump pathway:
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Component's bytes were updated:
      Byte_Array_Assert.Eq (Bytes, Expected_Table_Bytes);

      -- Exactly one Memory_Dump fired automatically; nothing on the Packet path:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Packet_T_Recv_Sync_History.Get_Count, 0);

      Dump := T.Memory_Dump_Recv_Sync_History.Get (1);
      Natural_Assert.Eq (Natural (Dump.Id), 0);
      Natural_Assert.Eq (Byte_Array_Pointer.Length (Dump.Memory_Pointer), Bytes'Length);
      Byte_Array_Assert.Eq (Byte_Array_Pointer.To_Byte_Array (Dump.Memory_Pointer), Expected_Table_Bytes);

      -- Release/event bookkeeping:
      Natural_Assert.Eq (T.Parameter_Table_Updated_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region, Success));
   end Test_Memory_Dump_Path_Auto_Dump_On_Change;

end Parameter_Store_Tests.Implementation;
