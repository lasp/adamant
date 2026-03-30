--------------------------------------------------------------------------------
-- Ccsds_Parameter_Table_Router Tests Body
--------------------------------------------------------------------------------

with Ada.Real_Time;
with Basic_Assertions; use Basic_Assertions;
with Basic_Types;
with Ccsds_Enums;
with Ccsds_Primary_Header;
with Ccsds_Primary_Header.Assertion; use Ccsds_Primary_Header.Assertion;
with Ccsds_Space_Packet;
with Command;
with Command_Enums; use Command_Enums.Command_Response_Status;
with Command_Response.Assertion; use Command_Response.Assertion;
with Interfaces; use Interfaces;
with Packed_U16;
with Packed_U32.Assertion; use Packed_U32.Assertion;
with Parameter_Enums; use Parameter_Enums.Parameter_Table_Update_Status;
with Parameter_Table_Id.Assertion; use Parameter_Table_Id.Assertion;
with Parameter_Table_Operation_Failure_Info.Assertion; use Parameter_Table_Operation_Failure_Info.Assertion;
with Parameter_Table_Timeout_Info.Assertion; use Parameter_Table_Timeout_Info.Assertion;
with Ccsds_Parameter_Table_Router_Types; use Ccsds_Parameter_Table_Router_Types;
with Parameter_Types;
with Parameters_Memory_Region;
with System; use System;
with Test_Assembly_Parameter_Table_Router_Table;

package body Ccsds_Parameter_Table_Router_Tests.Implementation is

   -------------------------------------------------------------------------
   -- Globals to control simulator task behavior:
   -------------------------------------------------------------------------
   Task_Send_Response : Boolean := False;
   Task_Response_Status : Parameter_Enums.Parameter_Table_Update_Status.E := Parameter_Enums.Parameter_Table_Update_Status.Success;
   Task_Send_Timeout : Boolean := False;
   Task_Responses_To_Send : Natural := 0;

   -- Simulator task parameter table byte array
   Sim_Bytes : aliased Basic_Types.Byte_Array := [0 .. 1023 => 16#AB#];

   -- Optional response schedule - If populated, the simulator reads successive
   -- statuses from this list and overrides Task_Response_Status for each response
   -- sent.
   Max_Schedule_Length : constant := 20;
   Response_Schedule : array (0 .. Max_Schedule_Length - 1) of Parameter_Enums.Parameter_Table_Update_Status.E :=
      [others => Parameter_Enums.Parameter_Table_Update_Status.Success];
   Schedule_Length : Natural := 0;
   Schedule_Index : Natural := 0;

   -------------------------------------------------------------------------
   -- Helper: sleep for a number of milliseconds:
   -------------------------------------------------------------------------
   procedure Sleep (Ms : in Natural := 5) is
      use Ada.Real_Time;
      Sleep_Time : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (Ms);
      Wake_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Sleep_Time;
   begin
      delay until Wake_Time;
   end Sleep;

   -------------------------------------------------------------------------
   -- Simulator task type for downstream components:
   -------------------------------------------------------------------------
   type Boolean_Access is access all Boolean;

   task type Simulator_Task (
      Class_Self : Class_Access;
      Task_Exit : Boolean_Access
   );

   task body Simulator_Task is
      Count : Natural := 0;
      Tick_Count : Natural := 0;
   begin
      while not Task_Exit.all and then Count < 500 loop
         Count := @ + 1;

         if Task_Send_Response and then Task_Responses_To_Send > 0 then
            Sleep (4);
            -- Use schedule if available, otherwise use Task_Response_Status:
            declare
               Status_To_Send : Parameter_Enums.Parameter_Table_Update_Status.E;
            begin
               if Schedule_Length > 0 and then Schedule_Index < Schedule_Length then
                  Status_To_Send := Response_Schedule (Schedule_Index);
                  Schedule_Index := @ + 1;
               else
                  Status_To_Send := Task_Response_Status;
               end if;
               Class_Self.all.Tester.Parameters_Memory_Region_Release_T_Send ((
                  Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length),
                  Status => Status_To_Send
               ));
            end;
            Task_Responses_To_Send := @ - 1;
            if Task_Responses_To_Send = 0 then
               Task_Send_Response := False;
            end if;
         elsif Task_Send_Timeout then
            Sleep (4);
            Class_Self.all.Tester.Timeout_Tick_Send (((0, 0), 0));
            Tick_Count := @ + 1;
            if Tick_Count > 4 then
               Tick_Count := 0;
               Task_Send_Timeout := False;
            end if;
         else
            Sleep (2);
         end if;
      end loop;
   end Simulator_Task;

   -------------------------------------------------------------------------
   -- Helper that constructs a CCSDS packet:
   -------------------------------------------------------------------------
   function Make_Packet (
      Sequence_Flag : in Ccsds_Enums.Ccsds_Sequence_Flag.E;
      Data : in Basic_Types.Byte_Array
   ) return Ccsds_Space_Packet.T is
      Pkt : Ccsds_Space_Packet.T := (
         Header => (
            Version => 0,
            Packet_Type => Ccsds_Enums.Ccsds_Packet_Type.Telemetry,
            Secondary_Header => Ccsds_Enums.Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
            Apid => 0,
            Sequence_Flag => Sequence_Flag,
            Sequence_Count => 0,
            -- CCSDS convention: Packet_Length = data_length - 1
            Packet_Length => Unsigned_16 (Data'Length) - 1
         ),
         Data => [others => 0]
      );
   begin
      Pkt.Data (0 .. Data'Length - 1) := Data;
      return Pkt;
   end Make_Packet;

   -- Helper which makes a FirstSegment or Unsegmented packet with table ID and payload.
   function Make_Table_Packet (
      Sequence_Flag : Ccsds_Enums.Ccsds_Sequence_Flag.E;
      Table_Id : Parameter_Types.Parameter_Table_Id;
      Payload : Basic_Types.Byte_Array
   ) return Ccsds_Space_Packet.T is
      Data_Len : constant Natural := 2 + Payload'Length;
      Data : Basic_Types.Byte_Array (0 .. Data_Len - 1) := [others => 0];
      -- Serialize Table ID as big-endian 2 bytes:
      Id_Bytes : constant Basic_Types.Byte_Array := Packed_U16.Serialization.To_Byte_Array ((Value => Unsigned_16 (Table_Id)));
   begin
      Data (0 .. 1) := Id_Bytes;
      if Payload'Length > 0 then
         Data (2 .. Data_Len - 1) := Payload;
      end if;
      return Make_Packet (Sequence_Flag, Data);
   end Make_Table_Packet;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      -- Reset globals:
      Task_Send_Response := False;
      Task_Response_Status := Parameter_Enums.Parameter_Table_Update_Status.Success;
      Task_Send_Timeout := False;
      Task_Responses_To_Send := 0;
      Schedule_Length := 0;
      Schedule_Index := 0;

      -- Allocate heap memory to component:
      Self.Tester.Init_Base (Parameters_Memory_Region_T_Send_Count => 5, Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 10);

      -- Make necessary connections between tester and component:
      Self.Tester.Connect;

      -- Call component init:
      Self.Tester.Component_Instance.Init (
         Table => Test_Assembly_Parameter_Table_Router_Table.Router_Table_Entries,
         Buffer_Size => 1024,
         Ticks_Until_Timeout => 3
      );

      -- Note: Set_Up is NOT called here. Only tests that specifically test
      -- Set_Up behavior call it themselves. This avoids having to account for
      -- the 5 initial data products in every other test.
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      Self.Tester.Component_Instance.Final;
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   -- Test_Init: Verify Init populates binary tree, creates staging buffer, and validates routing table.
   overriding procedure Test_Init (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Init was already called in Set_Up_Test. The fact that Init completed
      -- without assertion failure means the binary tree was populated, the
      -- staging buffer was created, and all routing table validations passed.

      -- No events or data products should have been emitted (Set_Up not called):
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
   end Test_Init;

   -- Test_Set_Up: Verify Set_Up publishes initial data product values.
   overriding procedure Test_Set_Up (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Call Set_Up explicitly for this test:
      T.Component_Instance.Set_Up;

      -- Verify 5 initial data products were published:
      Natural_Assert.Eq (T.Num_Packets_Received_History.Get_Count, 1);
      Natural_Assert.Eq (T.Num_Packets_Rejected_History.Get_Count, 1);
      Natural_Assert.Eq (T.Num_Tables_Updated_History.Get_Count, 1);
      Natural_Assert.Eq (T.Num_Tables_Invalid_History.Get_Count, 1);
      Natural_Assert.Eq (T.Last_Table_Received_History.Get_Count, 1);

      -- Verify initial values are zero:
      Packed_U32_Assert.Eq (T.Num_Packets_Received_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Num_Packets_Rejected_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Num_Tables_Updated_History.Get (1), (Value => 0));
      Packed_U32_Assert.Eq (T.Num_Tables_Invalid_History.Get (1), (Value => 0));

      -- Verify total DP count:
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);

      -- Verify no events were thrown (no load_all on setup):
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
   end Test_Set_Up;

   -- Test_Set_Up_Load_All: Init with Load_All_Parameter_Tables_On_Set_Up.
   -- Verify tables are loaded during Set_Up.
   overriding procedure Test_Set_Up_Load_All (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
   begin
      -- Re-init with Load_All_Parameter_Tables_On_Set_Up => True.
      -- Final the previous Init, then re-Init with the new flag:
      T.Component_Instance.Final;
      T.Component_Instance.Init (
         Table => Test_Assembly_Parameter_Table_Router_Table.Router_Table_Entries,
         Buffer_Size => 1024,
         Ticks_Until_Timeout => 3,
         Load_All_Parameter_Tables_On_Set_Up => True
      );

      -- The routing table has 4 entries. 3 have load_from (IDs 1, 3, 10).
      -- Table ID 4 has no load_from, so it's skipped.
      -- For each loadable table, Do_Table_Load does:
      --   1 Get (from load_from) + N Set (to non-load_from destinations)
      -- Table 10: 1 Get + 1 Set = 2 responses
      -- Table 1: 1 Get + 1 Set = 2 responses
      -- Table 3: 1 Get + 2 Set = 3 responses
      -- Total: 7 responses needed.
      Task_Responses_To_Send := 7;
      Task_Send_Response := True;

      -- Call Set_Up which should load all tables:
      T.Component_Instance.Set_Up;

      -- Verify Loading_All_Parameter_Tables event:
      Natural_Assert.Eq (T.Loading_All_Parameter_Tables_History.Get_Count, 1);
      -- Verify All_Parameter_Tables_Loaded event:
      Natural_Assert.Eq (T.All_Parameter_Tables_Loaded_History.Get_Count, 1);
      -- Verify Loading_Table events (one per loadable table, sorted by ID: 1, 3, 10):
      Natural_Assert.Eq (T.Loading_Table_History.Get_Count, 3);
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (1), (Id => 1));
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (2), (Id => 3));
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (3), (Id => 10));
      -- Verify Table_Loaded events (one per loadable table):
      Natural_Assert.Eq (T.Table_Loaded_History.Get_Count, 3);
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (1), (Id => 1));
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (2), (Id => 3));
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (3), (Id => 10));

      -- Total events: 1 Loading_All + 1 All_Loaded + 3 Loading_Table + 3 Table_Loaded = 8
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);

      Task_Exit := True;
   end Test_Set_Up_Load_All;

   -- Test_Nominal_Segmented_Upload: Send First + Continuation + Last for table ID 10.
   overriding procedure Test_Nominal_Segmented_Upload (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
      Payload : constant Basic_Types.Byte_Array (0 .. 9) := [others => 16#AA#];
      -- Table ID 10: Working_Params (idx 1, no load_from) + Primary_Param_Store (idx 2, load_from)
      -- That means 2 responses needed (1 non-load_from + 1 load_from).
   begin
      -- Send packets and dispatch:
      declare
      begin
         -- Send FirstSegment:
         T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
            Ccsds_Enums.Ccsds_Sequence_Flag.Firstsegment,
            Table_Id => 10,
            Payload => Payload
         ));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Verify Receiving_New_Table event:
         Natural_Assert.Eq (T.Receiving_New_Table_History.Get_Count, 1);
         Parameter_Table_Id_Assert.Eq (T.Receiving_New_Table_History.Get (1), (Id => 10));

         -- Send Continuation:
         T.Ccsds_Space_Packet_T_Send (Make_Packet (
            Ccsds_Enums.Ccsds_Sequence_Flag.Continuationsegment,
            Payload
         ));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- No new table-level events yet:
         Boolean_Assert.Eq (T.Table_Received_History.Is_Empty, True);

         -- Send Last segment - this completes the table and triggers distribution:
         Task_Responses_To_Send := 2;
         Task_Send_Response := True;

         T.Ccsds_Space_Packet_T_Send (Make_Packet (
            Ccsds_Enums.Ccsds_Sequence_Flag.Lastsegment,
            Payload
         ));
         Natural_Assert.Eq (T.Dispatch_All, 1);

         -- Verify Table_Received event:
         Natural_Assert.Eq (T.Table_Received_History.Get_Count, 1);
         Parameter_Table_Id_Assert.Eq (T.Table_Received_History.Get (1), (Id => 10));

         -- Verify Table_Updated event:
         Natural_Assert.Eq (T.Table_Updated_History.Get_Count, 1);
         Parameter_Table_Id_Assert.Eq (T.Table_Updated_History.Get (1), (Id => 10));

         -- Verify data products:
         -- 3 packets received:
         Natural_Assert.Eq (T.Num_Packets_Received_History.Get_Count, 3);
         Packed_U32_Assert.Eq (T.Num_Packets_Received_History.Get (3), (Value => 3));

         -- 1 table updated:
         Natural_Assert.Eq (T.Num_Tables_Updated_History.Get_Count, 1);
         Packed_U32_Assert.Eq (T.Num_Tables_Updated_History.Get (1), (Value => 1));

         -- No failures:
         Boolean_Assert.Eq (T.Table_Update_Failure_History.Is_Empty, True);
         Boolean_Assert.Eq (T.Table_Update_Timeout_History.Is_Empty, True);

         -- Total events: 1 Receiving_New_Table + 1 Table_Received + 1 Table_Updated = 3
         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 3);

         -- Total DPs: 3 Num_Packets_Received + 1 Num_Tables_Updated + 3 Last_Table_Received = 7
         Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 7);

         -- Verify routing: 2 sends (non-LF idx 1 first, then LF idx 2)
         -- First segment: 10 bytes payload stored in buffer (after stripping 2-byte ID).
         -- Continuation: 10 bytes. Last: 10 bytes. Total = 30 bytes.
         Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 2);
         declare
            use Parameter_Enums.Parameter_Table_Operation_Type;
            Region_1 : constant Parameters_Memory_Region.T := T.Parameters_Memory_Region_T_Recv_Sync_History.Get (1);
            Region_2 : constant Parameters_Memory_Region.T := T.Parameters_Memory_Region_T_Recv_Sync_History.Get (2);
         begin
            -- Both should be Set operations with correct length:
            Boolean_Assert.Eq (Region_1.Operation = Set, True);
            Natural_Assert.Eq (Region_1.Region.Length, 30);
            Boolean_Assert.Eq (Region_1.Region.Address /= Null_Address, True);
            Boolean_Assert.Eq (Region_2.Operation = Set, True);
            Natural_Assert.Eq (Region_2.Region.Length, 30);
            Boolean_Assert.Eq (Region_2.Region.Address /= Null_Address, True);
         end;
      end;

      Task_Exit := True;
   end Test_Nominal_Segmented_Upload;

   -- Test_Unsegmented_Upload: Send a single Unsegmented packet for table ID 10.
   overriding procedure Test_Unsegmented_Upload (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
      Payload : constant Basic_Types.Byte_Array (0 .. 9) := [others => 16#BB#];
   begin
      -- Table ID 10: 2 destinations (1 non-load_from + 1 load_from) = 2 responses
      Task_Responses_To_Send := 2;
      Task_Send_Response := True;

      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Table_Id => 10,
         Payload => Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Unsegmented goes directly to Complete_Table (no New_Table step),
      -- so Receiving_New_Table is NOT emitted:
      Boolean_Assert.Eq (T.Receiving_New_Table_History.Is_Empty, True);
      -- Verify Table_Received event with ID:
      Natural_Assert.Eq (T.Table_Received_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Table_Received_History.Get (1), (Id => 10));
      -- Verify Table_Updated event with ID:
      Natural_Assert.Eq (T.Table_Updated_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Table_Updated_History.Get (1), (Id => 10));

      -- Verify 1 packet received DP:
      Packed_U32_Assert.Eq (T.Num_Packets_Received_History.Get (T.Num_Packets_Received_History.Get_Count), (Value => 1));

      -- Verify 1 table updated:
      Packed_U32_Assert.Eq (T.Num_Tables_Updated_History.Get (T.Num_Tables_Updated_History.Get_Count), (Value => 1));

      -- Verify routing: 2 sends for table ID 10 (non-LF then LF).
      -- Unsegmented payload is 10 bytes, stored in buffer (no Table ID) = 10 bytes.
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 2);
      declare
         use Parameter_Enums.Parameter_Table_Operation_Type;
         Region_1 : constant Parameters_Memory_Region.T := T.Parameters_Memory_Region_T_Recv_Sync_History.Get (1);
         Region_2 : constant Parameters_Memory_Region.T := T.Parameters_Memory_Region_T_Recv_Sync_History.Get (2);
      begin
         Boolean_Assert.Eq (Region_1.Operation = Set, True);
         Natural_Assert.Eq (Region_1.Region.Length, 10);
         Boolean_Assert.Eq (Region_1.Region.Address /= Null_Address, True);
         Boolean_Assert.Eq (Region_2.Operation = Set, True);
         Natural_Assert.Eq (Region_2.Region.Length, 10);
         Boolean_Assert.Eq (Region_2.Region.Address /= Null_Address, True);
      end;

      -- Total events: 1 Table_Received + 1 Table_Updated = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 1 Num_Packets_Received + 1 Num_Tables_Updated + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      Task_Exit := True;
   end Test_Unsegmented_Upload;

   -- Test_Multi_Destination_Order: Verify non-Load_From destinations sent first, Load_From last.
   overriding procedure Test_Multi_Destination_Order (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
      Payload : constant Basic_Types.Byte_Array (0 .. 9) := [others => 16#CC#];
   begin
      -- Table ID 3: Working_Params (idx 1, no LF) + Another_Params (idx 3, no LF) + Primary_Param_Store (idx 2, LF=True)
      -- Expected order: idx 1, idx 3 (non-load_from), then idx 2 (load_from)
      -- Total: 3 responses needed
      Task_Responses_To_Send := 3;
      Task_Send_Response := True;

      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Table_Id => 3,
         Payload => Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify 3 sends occurred:
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 3);

      -- Verify Table_Received and Table_Updated events with ID:
      Natural_Assert.Eq (T.Table_Received_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Table_Received_History.Get (1), (Id => 3));
      Natural_Assert.Eq (T.Table_Updated_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Table_Updated_History.Get (1), (Id => 3));

      -- Verify the sends are Set operations with correct region length.
      -- Unsegmented payload is 10 bytes, stored in buffer = 10 bytes.
      declare
         use Parameter_Enums.Parameter_Table_Operation_Type;
         Region_1 : constant Parameters_Memory_Region.T := T.Parameters_Memory_Region_T_Recv_Sync_History.Get (1);
         Region_2 : constant Parameters_Memory_Region.T := T.Parameters_Memory_Region_T_Recv_Sync_History.Get (2);
         Region_3 : constant Parameters_Memory_Region.T := T.Parameters_Memory_Region_T_Recv_Sync_History.Get (3);
      begin
         -- All should be Set operations with correct length and non-null address:
         Boolean_Assert.Eq (Region_1.Operation = Set, True);
         Natural_Assert.Eq (Region_1.Region.Length, 10);
         Boolean_Assert.Eq (Region_1.Region.Address /= Null_Address, True);
         Boolean_Assert.Eq (Region_2.Operation = Set, True);
         Natural_Assert.Eq (Region_2.Region.Length, 10);
         Boolean_Assert.Eq (Region_2.Region.Address /= Null_Address, True);
         Boolean_Assert.Eq (Region_3.Operation = Set, True);
         Natural_Assert.Eq (Region_3.Region.Length, 10);
         Boolean_Assert.Eq (Region_3.Region.Address /= Null_Address, True);
      end;

      -- Total events: 1 Table_Received + 1 Table_Updated = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 1 Num_Packets_Received + 1 Num_Tables_Updated + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      Task_Exit := True;
   end Test_Multi_Destination_Order;

   -- Test_Packet_Ignored: Continuation and Last without prior First.
   overriding procedure Test_Packet_Ignored (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Payload : constant Basic_Types.Byte_Array (0 .. 4) := [others => 16#DD#];
   begin
      -- Send Continuation without prior First:
      T.Ccsds_Space_Packet_T_Send (Make_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Continuationsegment,
         Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Packet_Ignored_History.Get_Count, 1);
      -- Verify CCSDS header in event matches the sent packet:
      Ccsds_Primary_Header_Assert.Eq (T.Packet_Ignored_History.Get (1), (
         Version => 0, Packet_Type => Ccsds_Enums.Ccsds_Packet_Type.Telemetry,
         Secondary_Header => Ccsds_Enums.Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => 0, Sequence_Flag => Ccsds_Enums.Ccsds_Sequence_Flag.Continuationsegment,
         Sequence_Count => 0, Packet_Length => Unsigned_16 (Payload'Length) - 1
      ));
      Packed_U32_Assert.Eq (T.Num_Packets_Rejected_History.Get (T.Num_Packets_Rejected_History.Get_Count), (Value => 1));

      -- Send Last without prior First:
      T.Ccsds_Space_Packet_T_Send (Make_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Lastsegment,
         Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Packet_Ignored_History.Get_Count, 2);
      Ccsds_Primary_Header_Assert.Eq (T.Packet_Ignored_History.Get (2), (
         Version => 0, Packet_Type => Ccsds_Enums.Ccsds_Packet_Type.Telemetry,
         Secondary_Header => Ccsds_Enums.Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => 0, Sequence_Flag => Ccsds_Enums.Ccsds_Sequence_Flag.Lastsegment,
         Sequence_Count => 0, Packet_Length => Unsigned_16 (Payload'Length) - 1
      ));
      Packed_U32_Assert.Eq (T.Num_Packets_Rejected_History.Get (T.Num_Packets_Rejected_History.Get_Count), (Value => 2));

      -- Verify packet count:
      Packed_U32_Assert.Eq (T.Num_Packets_Received_History.Get (T.Num_Packets_Received_History.Get_Count), (Value => 2));

      -- Total events: 2 Packet_Ignored = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 2*(Num_Packets_Rejected + Num_Packets_Received + Last_Table_Received) = 6
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 6);
   end Test_Packet_Ignored;

   -- Test_Too_Small_Table: FirstSegment with less than 2 bytes.
   overriding procedure Test_Too_Small_Table (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Send FirstSegment with only 1 byte of data:
      T.Ccsds_Space_Packet_T_Send (Make_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Firstsegment,
         [0 => 16#FF#]
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Too_Small_Table_History.Get_Count, 1);
      -- Verify CCSDS header in event:
      Ccsds_Primary_Header_Assert.Eq (T.Too_Small_Table_History.Get (1), (
         Version => 0, Packet_Type => Ccsds_Enums.Ccsds_Packet_Type.Telemetry,
         Secondary_Header => Ccsds_Enums.Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => 0, Sequence_Flag => Ccsds_Enums.Ccsds_Sequence_Flag.Firstsegment,
         Sequence_Count => 0, Packet_Length => 0
      ));
      Packed_U32_Assert.Eq (T.Num_Packets_Rejected_History.Get (T.Num_Packets_Rejected_History.Get_Count), (Value => 1));
      Packed_U32_Assert.Eq (T.Num_Packets_Received_History.Get (T.Num_Packets_Received_History.Get_Count), (Value => 1));

      -- Total events: 1 Too_Small_Table = 1
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Total DPs: 1 Num_Packets_Rejected + 1 Num_Packets_Received + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
   end Test_Too_Small_Table;

   -- Test_Buffer_Overflow: Data exceeding buffer capacity.
   overriding procedure Test_Buffer_Overflow (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Our buffer is 1024 bytes. Send a first segment with some data,
      -- then a continuation that exceeds the buffer.
      Small_Payload : constant Basic_Types.Byte_Array (0 .. 9) := [others => 16#EE#];
      -- The continuation data plus the first data should exceed 1024 bytes.
      -- First segment contributed 2 (table ID) + 10 (payload) = 12 bytes of data to the buffer.
      -- Actually, the staging buffer stores the raw data including the table ID bytes.
      -- Send a continuation with 1020 bytes which should overflow.
      Large_Payload : constant Basic_Types.Byte_Array (0 .. 1019) := [others => 16#EE#];
   begin
      -- Send FirstSegment with small payload:
      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Firstsegment,
         Table_Id => 10,
         Payload => Small_Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Receiving_New_Table_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Receiving_New_Table_History.Get (1), (Id => 10));

      -- Send a large continuation that overflows the buffer:
      T.Ccsds_Space_Packet_T_Send (Make_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Continuationsegment,
         Large_Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Staging_Buffer_Overflow_History.Get_Count, 1);
      -- Verify CCSDS header of the overflow packet:
      Ccsds_Primary_Header_Assert.Eq (T.Staging_Buffer_Overflow_History.Get (1), (
         Version => 0, Packet_Type => Ccsds_Enums.Ccsds_Packet_Type.Telemetry,
         Secondary_Header => Ccsds_Enums.Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
         Apid => 0, Sequence_Flag => Ccsds_Enums.Ccsds_Sequence_Flag.Continuationsegment,
         Sequence_Count => 0, Packet_Length => Unsigned_16 (Large_Payload'Length) - 1
      ));
      Packed_U32_Assert.Eq (T.Num_Packets_Rejected_History.Get (T.Num_Packets_Rejected_History.Get_Count), (Value => 1));

      -- Total events: 1 Receiving_New_Table + 1 Staging_Buffer_Overflow = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: pkt1(Num_Packets_Received + Last_Table_Received) + pkt2(Num_Packets_Rejected + Num_Packets_Received + Last_Table_Received) = 2 + 3 = 5
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 5);
   end Test_Buffer_Overflow;

   -- Test_Unrecognized_Table_Id: Complete table with unknown ID.
   overriding procedure Test_Unrecognized_Table_Id (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Payload : constant Basic_Types.Byte_Array (0 .. 4) := [others => 16#99#];
   begin
      -- Send unsegmented packet with unknown table ID 999:
      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Table_Id => 999,
         Payload => Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Table_Received event (table was completed):
      Natural_Assert.Eq (T.Table_Received_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Table_Received_History.Get (1), (Id => 999));

      -- Verify Unrecognized_Table_Id event:
      Natural_Assert.Eq (T.Unrecognized_Table_Id_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Unrecognized_Table_Id_History.Get (1), (Id => 999));

      -- Verify Num_Tables_Invalid incremented:
      Packed_U32_Assert.Eq (T.Num_Tables_Invalid_History.Get (T.Num_Tables_Invalid_History.Get_Count), (Value => 1));

      -- Total events: 1 Table_Received + 1 Unrecognized_Table_Id = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 1 Num_Packets_Received + 1 Num_Tables_Invalid + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);
   end Test_Unrecognized_Table_Id;

   -- Test_Destination_Failure: Downstream returns failure status.
   overriding procedure Test_Destination_Failure (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
      Payload : constant Basic_Types.Byte_Array (0 .. 4) := [others => 16#88#];
   begin
      -- Table ID 10: Working_Params (idx 1, no LF) is sent first.
      -- Simulator returns Parameter_Error for first destination.
      Task_Response_Status := Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error;
      Task_Responses_To_Send := 1;
      Task_Send_Response := True;

      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Table_Id => 10,
         Payload => Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Table_Update_Failure event with correct info:
      Natural_Assert.Eq (T.Table_Update_Failure_History.Get_Count, 1);
      Parameter_Table_Operation_Failure_Info_Assert.Eq (T.Table_Update_Failure_History.Get (1), (
         Table_Id => 10,
         Connector_Index => 1,
         Release => (Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length), Status => Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error)
      ));

      -- Verify no Table_Updated event (it failed):
      Boolean_Assert.Eq (T.Table_Updated_History.Is_Empty, True);

      -- Verify Num_Tables_Invalid incremented:
      Packed_U32_Assert.Eq (T.Num_Tables_Invalid_History.Get (T.Num_Tables_Invalid_History.Get_Count), (Value => 1));

      -- Only 1 send occurred (failed on first, didn't continue):
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 1);

      -- Total events: 1 Table_Received + 1 Table_Update_Failure = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 1 Num_Packets_Received + 1 Num_Tables_Invalid + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      Task_Exit := True;
   end Test_Destination_Failure;

   -- Test_Destination_Timeout: Downstream does not respond.
   overriding procedure Test_Destination_Timeout (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
      Payload : constant Basic_Types.Byte_Array (0 .. 4) := [others => 16#77#];
   begin
      -- Send timeout ticks instead of response:
      Task_Send_Timeout := True;

      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Table_Id => 10,
         Payload => Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Table_Update_Timeout event with correct info:
      Natural_Assert.Eq (T.Table_Update_Timeout_History.Get_Count, 1);
      Parameter_Table_Timeout_Info_Assert.Eq (T.Table_Update_Timeout_History.Get (1), (
         Table_Id => 10,
         Connector_Index => 1
      ));

      -- Verify no Table_Updated event:
      Boolean_Assert.Eq (T.Table_Updated_History.Is_Empty, True);

      -- Verify Num_Tables_Invalid incremented:
      Packed_U32_Assert.Eq (T.Num_Tables_Invalid_History.Get (T.Num_Tables_Invalid_History.Get_Count), (Value => 1));

      -- Total events: 1 Table_Received + 1 Table_Update_Timeout = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 1 Num_Packets_Received + 1 Num_Tables_Invalid + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      Task_Exit := True;
   end Test_Destination_Timeout;

   -- Test_Partial_Failure_Stops: Multi-destination table where first destination fails.
   overriding procedure Test_Partial_Failure_Stops (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
      Payload : constant Basic_Types.Byte_Array (0 .. 4) := [others => 16#66#];
   begin
      -- Table ID 3 has 3 destinations: Working_Params (idx 1), Another_Params (idx 3), Primary_Param_Store (idx 2, LF)
      -- First non-load_from fails. Remaining destinations not sent to.
      Task_Response_Status := Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error;
      Task_Responses_To_Send := 1;
      Task_Send_Response := True;

      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Table_Id => 3,
         Payload => Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify only 1 send occurred (stopped after first failure):
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 1);

      -- Verify failure event with correct info:
      Natural_Assert.Eq (T.Table_Update_Failure_History.Get_Count, 1);
      Parameter_Table_Operation_Failure_Info_Assert.Eq (T.Table_Update_Failure_History.Get (1), (
         Table_Id => 3,
         Connector_Index => 1,
         Release => (Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length), Status => Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error)
      ));

      -- Verify no Table_Updated event:
      Boolean_Assert.Eq (T.Table_Updated_History.Is_Empty, True);

      -- Verify Num_Tables_Invalid incremented:
      Packed_U32_Assert.Eq (T.Num_Tables_Invalid_History.Get (T.Num_Tables_Invalid_History.Get_Count), (Value => 1));

      -- Total events: 1 Table_Received + 1 Table_Update_Failure = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 1 Num_Packets_Received + 1 Num_Tables_Invalid + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      Task_Exit := True;
   end Test_Partial_Failure_Stops;

   -- Test_Load_Table_Nominal: Load table from load_from source.
   overriding procedure Test_Load_Table_Nominal (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
   begin
      -- Table ID 10: 1 Get from Primary_Param_Store (idx 2, LF) + 1 Set to Working_Params (idx 1) = 2 responses
      Task_Response_Status := Parameter_Enums.Parameter_Table_Update_Status.Success;
      Task_Responses_To_Send := 2;
      Task_Send_Response := True;

      T.Command_T_Send (T.Commands.Load_Parameter_Table ((Id => 10)));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Loading_Table event:
      Natural_Assert.Eq (T.Loading_Table_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (1), (Id => 10));

      -- Verify Table_Loaded event:
      Natural_Assert.Eq (T.Table_Loaded_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (1), (Id => 10));

      -- Verify command success:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_Parameter_Table_Id,
         Status => Success
      ));

      -- Verify 2 sends: Get then Set:
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 2);
      declare
         use Parameter_Enums.Parameter_Table_Operation_Type;
      begin
         Boolean_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get (1).Operation = Get, True);
         Boolean_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get (2).Operation = Set, True);
      end;

      -- Total events: 1 Loading_Table + 1 Table_Loaded = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Task_Exit := True;
   end Test_Load_Table_Nominal;

   -- Test_Load_Table_No_Load_Source: Load command for table with no load_from.
   overriding procedure Test_Load_Table_No_Load_Source (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Table ID 4 has no load_from:
      T.Command_T_Send (T.Commands.Load_Parameter_Table ((Id => 4)));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify No_Load_Source event:
      Natural_Assert.Eq (T.No_Load_Source_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.No_Load_Source_History.Get (1), (Id => 4));

      -- Verify command failure:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_Parameter_Table_Id,
         Status => Failure
      ));

      -- No sends should have occurred:
      Boolean_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Is_Empty, True);

      -- Total events: 1 No_Load_Source = 1
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
   end Test_Load_Table_No_Load_Source;

   -- Test_Load_Table_Unrecognized: Load command for unknown table ID.
   overriding procedure Test_Load_Table_Unrecognized (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      T.Command_T_Send (T.Commands.Load_Parameter_Table ((Id => 777)));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Unrecognized_Table_Id event:
      Natural_Assert.Eq (T.Unrecognized_Table_Id_History.Get_Count, 1);
      Parameter_Table_Id_Assert.Eq (T.Unrecognized_Table_Id_History.Get (1), (Id => 777));

      -- Verify command failure:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_Parameter_Table_Id,
         Status => Failure
      ));

      -- Total events: 1 Unrecognized_Table_Id = 1
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
   end Test_Load_Table_Unrecognized;

   -- Test_Load_Table_Failure: Load fails during Get or Set.
   overriding procedure Test_Load_Table_Failure (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
   begin
      -- Test 1: Get failure (load from source fails)
      -- Table ID 10: Get from Primary_Param_Store (idx 2, LF)
      Task_Response_Status := Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error;
      Task_Responses_To_Send := 1;
      Task_Send_Response := True;

      T.Command_T_Send (T.Commands.Load_Parameter_Table ((Id => 10)));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Table_Load_Failure event (Is_Load path) with correct info:
      Natural_Assert.Eq (T.Table_Load_Failure_History.Get_Count, 1);
      Parameter_Table_Operation_Failure_Info_Assert.Eq (T.Table_Load_Failure_History.Get (1), (
         Table_Id => 10,
         Connector_Index => 2,
         Release => (Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length), Status => Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error)
      ));

      -- Verify command failure:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_Parameter_Table_Id,
         Status => Failure
      ));

      -- Only 1 send (Get, no Set since Get failed):
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 1);

      -- Note: The Get-succeeds-but-Set-fails path is covered by
      -- Test_Load_Command_Set_Failure using the response schedule.

      -- Total events: 1 Loading_Table + 1 Table_Load_Failure = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Task_Exit := True;
   end Test_Load_Table_Failure;

   -- Test_Load_All_Nominal: Load all tables.
   overriding procedure Test_Load_All_Nominal (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
   begin
      -- 3 tables have load_from (IDs 10, 1, 3). Table 4 is skipped.
      -- Table 10: 1 Get + 1 Set = 2
      -- Table 1: 1 Get + 1 Set = 2
      -- Table 3: 1 Get + 2 Set = 3
      -- Total: 7 responses needed
      Task_Response_Status := Parameter_Enums.Parameter_Table_Update_Status.Success;
      Task_Responses_To_Send := 7;
      Task_Send_Response := True;

      T.Command_T_Send (T.Commands.Load_All_Parameter_Tables);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Loading_All_Parameter_Tables event:
      Natural_Assert.Eq (T.Loading_All_Parameter_Tables_History.Get_Count, 1);

      -- Verify All_Parameter_Tables_Loaded event:
      Natural_Assert.Eq (T.All_Parameter_Tables_Loaded_History.Get_Count, 1);

      -- Verify Loading_Table events (sorted by ID: 1, 3, 10):
      Natural_Assert.Eq (T.Loading_Table_History.Get_Count, 3);
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (1), (Id => 1));
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (2), (Id => 3));
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (3), (Id => 10));

      -- Verify Table_Loaded events:
      Natural_Assert.Eq (T.Table_Loaded_History.Get_Count, 3);
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (1), (Id => 1));
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (2), (Id => 3));
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (3), (Id => 10));

      -- Verify command success:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_All_Parameter_Tables_Id,
         Status => Success
      ));

      -- Verify 7 sends occurred:
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Recv_Sync_History.Get_Count, 7);

      -- Total events: 1 Loading_All + 3 Loading_Table + 3 Table_Loaded + 1 All_Loaded = 8
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Task_Exit := True;
   end Test_Load_All_Nominal;

   -- Test_Load_All_Partial_Failure: One table fails during load all.
   overriding procedure Test_Load_All_Partial_Failure (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
   begin
      -- Tree is sorted by ID: 1, 3, 4, 10. Table 4 has no Load_From (skipped).
      -- First table (ID 1) Get will fail. The component continues to remaining tables.
      -- Schedule: 1 failure (table 1 Get) + 5 successes (table 3: Get+2*Set, table 10: Get+Set)
      -- Total: 6 responses
      Schedule_Length := 6;
      Schedule_Index := 0;
      Response_Schedule (0) := Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error;
      for I in 1 .. 5 loop
         Response_Schedule (I) := Parameter_Enums.Parameter_Table_Update_Status.Success;
      end loop;
      Task_Responses_To_Send := 6;
      Task_Send_Response := True;

      T.Command_T_Send (T.Commands.Load_All_Parameter_Tables);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify Loading_All_Parameter_Tables event:
      Natural_Assert.Eq (T.Loading_All_Parameter_Tables_History.Get_Count, 1);

      -- Verify All_Parameter_Tables_Loaded event (always emitted even on partial failure):
      Natural_Assert.Eq (T.All_Parameter_Tables_Loaded_History.Get_Count, 1);

      -- Verify Table_Load_Failure for the first table (ID 1, Get from idx 2 fails):
      Natural_Assert.Eq (T.Table_Load_Failure_History.Get_Count, 1);
      Parameter_Table_Operation_Failure_Info_Assert.Eq (T.Table_Load_Failure_History.Get (1), (
         Table_Id => 1,
         Connector_Index => 2,
         Release => (Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length), Status => Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error)
      ));

      -- Verify Loading_Table events for all 3 loadable tables:
      Natural_Assert.Eq (T.Loading_Table_History.Get_Count, 3);
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (1), (Id => 1));
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (2), (Id => 3));
      Parameter_Table_Id_Assert.Eq (T.Loading_Table_History.Get (3), (Id => 10));

      -- Verify the other 2 tables loaded successfully (IDs 3 and 10):
      Natural_Assert.Eq (T.Table_Loaded_History.Get_Count, 2);
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (1), (Id => 3));
      Parameter_Table_Id_Assert.Eq (T.Table_Loaded_History.Get (2), (Id => 10));

      -- Verify command failure (because one table failed):
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_All_Parameter_Tables_Id,
         Status => Failure
      ));

      -- Total events: 1 Loading_All + 3 Loading_Table + 1 Table_Load_Failure + 2 Table_Loaded + 1 All_Loaded = 8
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 8);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Task_Exit := True;
   end Test_Load_All_Partial_Failure;

   -- Test_Packet_Dropped: Overflow CCSDS packet queue.
   overriding procedure Test_Packet_Dropped (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : Ccsds_Space_Packet.T;
   begin
      -- Fill queue with max-size packets. Queue size = max_element_size * 10.
      -- Send enough to fill and overflow:
      Pkt.Header.Packet_Length := Unsigned_16 (Pkt.Data'Length) - 1;
      Pkt.Header.Sequence_Flag := Ccsds_Enums.Ccsds_Sequence_Flag.Continuationsegment;

      for I in 1 .. 10 loop
         T.Ccsds_Space_Packet_T_Send (Pkt);
      end loop;

      -- Next one should overflow:
      T.Expect_Ccsds_Space_Packet_T_Send_Dropped := True;
      T.Ccsds_Space_Packet_T_Send (Pkt);

      -- Verify Packet_Dropped event:
      Natural_Assert.Eq (T.Packet_Dropped_History.Get_Count, 1);

      -- Total events: 1 Packet_Dropped = 1
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Dispatch the queued packets to clear them:
      Natural_Assert.Eq (T.Dispatch_All, 10);
   end Test_Packet_Dropped;

   -- Test_Command_Dropped: Overflow command queue.
   overriding procedure Test_Command_Dropped (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Pkt : constant Ccsds_Space_Packet.T := (
         Header => (
            Version => 0,
            Packet_Type => Ccsds_Enums.Ccsds_Packet_Type.Telemetry,
            Secondary_Header => Ccsds_Enums.Ccsds_Secondary_Header_Indicator.Secondary_Header_Not_Present,
            Apid => 0,
            Sequence_Flag => Ccsds_Enums.Ccsds_Sequence_Flag.Continuationsegment,
            Sequence_Count => 0,
            Packet_Length => Unsigned_16 (Ccsds_Space_Packet.Ccsds_Data_Type'Length) - 1
         ),
         Data => [others => 0]
      );
   begin
      -- Fill queue with max-size CCSDS packets:
      for I in 1 .. 10 loop
         T.Ccsds_Space_Packet_T_Send (Pkt);
      end loop;

      -- Next command should overflow:
      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (T.Commands.Load_All_Parameter_Tables);

      -- Verify Command_Dropped event:
      Natural_Assert.Eq (T.Command_Dropped_History.Get_Count, 1);

      -- Total events: 1 Command_Dropped = 1
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Dispatch queued packets to clear:
      declare
         Ignore : constant Natural := T.Dispatch_All;
      begin
         null;
      end;
   end Test_Command_Dropped;

   -- Test_Invalid_Command: Send command with corrupted arguments.
   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Load_Parameter_Table ((Id => 10));
   begin
      -- Corrupt the arg buffer length to trigger invalid command handling:
      Cmd.Header.Arg_Buffer_Length := 22;

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Verify command response with Length_Error:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_Parameter_Table_Id,
         Status => Length_Error
      ));

      -- Verify Invalid_Command_Received event:
      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
   end Test_Invalid_Command;

   -- Test_Load_From_Destination_Failure: Upload table where non-Load_From succeeds
   -- but Load_From (sent last) fails. Covers Send_Table_To_Destinations Load_From failure path.
   overriding procedure Test_Load_From_Destination_Failure (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
      Payload : constant Basic_Types.Byte_Array (0 .. 9) := [others => 16#AA#];
   begin
      -- Table ID 10: Working_Params (idx 1, no LF) + Primary_Param_Store (idx 2, LF)
      -- Schedule: first send (non-LF) succeeds, second send (LF) fails.
      Schedule_Length := 2;
      Schedule_Index := 0;
      Response_Schedule (0) := Parameter_Enums.Parameter_Table_Update_Status.Success;
      Response_Schedule (1) := Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error;
      Task_Responses_To_Send := 2;
      Task_Send_Response := True;

      -- Send unsegmented table for quick complete:
      T.Ccsds_Space_Packet_T_Send (Make_Table_Packet (
         Ccsds_Enums.Ccsds_Sequence_Flag.Unsegmented,
         Table_Id => 10,
         Payload => Payload
      ));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Table_Received should fire:
      Natural_Assert.Eq (T.Table_Received_History.Get_Count, 1);

      -- Table_Updated should NOT fire (Load_From failed):
      Boolean_Assert.Eq (T.Table_Updated_History.Is_Empty, True);

      -- Table_Update_Failure should fire for the Load_From destination (idx 2):
      Natural_Assert.Eq (T.Table_Update_Failure_History.Get_Count, 1);
      Parameter_Table_Operation_Failure_Info_Assert.Eq (T.Table_Update_Failure_History.Get (1), (
         Table_Id => 10,
         Connector_Index => 2,
         Release => (Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length), Status => Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error)
      ));

      -- Invalid table count incremented:
      Packed_U32_Assert.Eq (T.Num_Tables_Invalid_History.Get (T.Num_Tables_Invalid_History.Get_Count), (Value => 1));

      -- Total events: 1 Table_Received + 1 Table_Update_Failure = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 1 Num_Packets_Received + 1 Num_Tables_Invalid + 1 Last_Table_Received = 3
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 3);

      Task_Exit := True;
   end Test_Load_From_Destination_Failure;

   -- Test_Load_Command_Set_Failure: Load command where Get succeeds but Set fails.
   -- Covers Do_Table_Load Set failure path.
   overriding procedure Test_Load_Command_Set_Failure (Self : in out Instance) is
      T : Component.Ccsds_Parameter_Table_Router.Implementation.Tester.Instance_Access renames Self.Tester;
      Task_Exit : aliased Boolean := False;
      Sim_Task : Simulator_Task (Self'Unchecked_Access, Task_Exit'Unchecked_Access);
   begin
      -- Table ID 10: Get from Primary_Param_Store (LF, idx 2) + Set to Working_Params (idx 1)
      -- Schedule: Get succeeds, Set fails.
      Schedule_Length := 2;
      Schedule_Index := 0;
      Response_Schedule (0) := Parameter_Enums.Parameter_Table_Update_Status.Success;
      Response_Schedule (1) := Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error;
      Task_Responses_To_Send := 2;
      Task_Send_Response := True;

      T.Command_T_Send (T.Commands.Load_Parameter_Table ((Id => 10)));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Loading_Table event should fire:
      Natural_Assert.Eq (T.Loading_Table_History.Get_Count, 1);

      -- Table_Loaded should NOT fire (Set failed):
      Boolean_Assert.Eq (T.Table_Loaded_History.Is_Empty, True);

      -- Table_Update_Failure should fire for the Set destination (idx 1, Working_Params):
      Natural_Assert.Eq (T.Table_Update_Failure_History.Get_Count, 1);
      Parameter_Table_Operation_Failure_Info_Assert.Eq (T.Table_Update_Failure_History.Get (1), (
         Table_Id => 10,
         Connector_Index => 1,
         Release => (Region => (Address => Sim_Bytes'Address, Length => Sim_Bytes'Length), Status => Parameter_Enums.Parameter_Table_Update_Status.Parameter_Error)
      ));

      -- Command should fail:
      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get (1), (
         Source_Id => 0,
         Registration_Id => 0,
         Command_Id => T.Commands.Get_Load_Parameter_Table_Id,
         Status => Failure
      ));

      -- Total events: 1 Loading_Table + 1 Table_Update_Failure = 2
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);

      -- Total DPs: 0 (Set_Up not called)
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Task_Exit := True;
   end Test_Load_Command_Set_Failure;

end Ccsds_Parameter_Table_Router_Tests.Implementation;
