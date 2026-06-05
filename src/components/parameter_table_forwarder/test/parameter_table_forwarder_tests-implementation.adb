--------------------------------------------------------------------------------
-- Parameter_Table_Forwarder Tests Body
--------------------------------------------------------------------------------

with Basic_Assertions; use Basic_Assertions;
with Command;
with Command_Response.Assertion; use Command_Response.Assertion;
with Command_Enums; use Command_Enums;
with Command_Header.Assertion; use Command_Header.Assertion;
with Byte_Array_Pointer;
with Memory_Packetizer_Types;
with Memory_Region;
with Memory_Region.Assertion; use Memory_Region.Assertion;
with Parameters_Memory_Region;
with Parameters_Memory_Region.Assertion; use Parameters_Memory_Region.Assertion;
with Parameters_Memory_Region_Release;
with Parameters_Memory_Region_Release.Assertion; use Parameters_Memory_Region_Release.Assertion;
with Parameter_Enums;
with Parameter_Enums.Assertion; use Parameter_Enums.Assertion;
with Parameter_Table_Header;
with Invalid_Parameters_Memory_Region_Length.Assertion; use Invalid_Parameters_Memory_Region_Length.Assertion;
with Invalid_Parameters_Memory_Region_Crc.Assertion; use Invalid_Parameters_Memory_Region_Crc.Assertion;
with Invalid_Command_Info.Assertion; use Invalid_Command_Info.Assertion;
with Packed_Table_Operation_Status;
with Packed_Table_Operation_Status.Assertion; use Packed_Table_Operation_Status.Assertion;
with Crc_16;
with Basic_Types;
with Interfaces; use Interfaces;
with System.Storage_Elements;

package body Parameter_Table_Forwarder_Tests.Implementation is

   -- Table size used across all tests. Chosen small but with enough room for
   -- the 6-byte header plus 14 data bytes.
   Test_Table_Size : constant Natural := 20;

   -- Build the payload-only region the forwarder is expected to slice from a
   -- given upstream region (used to verify forwarded-region histories).
   function Expected_Payload_Region (Full : in Memory_Region.T) return Memory_Region.T is
      use System.Storage_Elements;
   begin
      return (
         Address => Full.Address + Storage_Offset (Parameter_Table_Header.Size_In_Bytes),
         Length => Full.Length - Parameter_Table_Header.Size_In_Bytes
      );
   end Expected_Payload_Region;

   -------------------------------------------------------------------------
   -- Fixtures:
   -------------------------------------------------------------------------

   overriding procedure Set_Up_Test (Self : in out Instance) is
   begin
      Self.Tester.Init_Base (Queue_Size => Self.Tester.Component_Instance.Get_Max_Queue_Element_Size * 3);
      Self.Tester.Connect;
      Self.Tester.Component_Instance.Init (Table_Size => Test_Table_Size, Dump_Parameters_On_Change => False);
      Self.Tester.Component_Instance.Set_Up;
   end Set_Up_Test;

   overriding procedure Tear_Down_Test (Self : in out Instance) is
   begin
      Self.Tester.Final_Base;
   end Tear_Down_Test;

   -------------------------------------------------------------------------
   -- Helpers:
   -------------------------------------------------------------------------

   -- Build a valid parameter table buffer of Test_Table_Size bytes with the
   -- given version and data fill. Computes and writes the CRC into the header.
   procedure Build_Valid_Table
      (Buffer : out Basic_Types.Byte_Array;
       Version : in Short_Float := 1.0;
       Data_Fill : in Basic_Types.Byte := 16#5A#)
   is
      Computed_Crc : Crc_16.Crc_16_Type;
   begin
      pragma Assert (Buffer'Length = Test_Table_Size);
      Buffer := [others => Data_Fill];
      -- Write header with zero CRC first so the CRC computation skips itself:
      Buffer (Buffer'First .. Buffer'First + Parameter_Table_Header.Size_In_Bytes - 1) :=
         Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => [0, 0], Version => Version));
      -- Compute CRC over (Version + Data):
      Computed_Crc := Crc_16.Compute_Crc_16 (Buffer (Buffer'First + Parameter_Table_Header.Crc_Section_Length .. Buffer'Last));
      -- Patch in the computed CRC:
      Buffer (Buffer'First .. Buffer'First + Parameter_Table_Header.Size_In_Bytes - 1) :=
         Parameter_Table_Header.Serialization.To_Byte_Array ((Crc_Table => Computed_Crc, Version => Version));
   end Build_Valid_Table;

   -- Read back the header bytes from a fully-built table buffer.
   function Header_Of (Buffer : in Basic_Types.Byte_Array) return Parameter_Table_Header.T is
   begin
      return Parameter_Table_Header.Serialization.From_Byte_Array (
         Buffer (Buffer'First .. Buffer'First + Parameter_Table_Header.Size_In_Bytes - 1));
   end Header_Of;

   -------------------------------------------------------------------------
   -- Tests:
   -------------------------------------------------------------------------

   overriding procedure Test_Set_Success (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Region : Memory_Region.T;
   begin
      Build_Valid_Table (Table);
      Region := (Address => Table'Address, Length => Test_Table_Size);
      T.Mock_Set_Release_Status := Success;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Downstream contacted exactly once with the payload-only region
      -- (the header is stripped by the forwarder):
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 1);
      Parameters_Memory_Region_Assert.Eq (
         T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get (1),
         (Region => Expected_Payload_Region (Region), Operation => Set)
      );

      -- Release upstream with Success, reporting the full table region:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Success)
      );

      -- Exactly one event (Parameter_Table_Updated) and one data product
      -- (Table_Status) are emitted on a successful Set. The raw counts
      -- catch any unintended extra emission; the typed history below
      -- pins down the value.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Parameter_Table_Updated_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Updated_History.Get (1), Region);

      -- Table_Status DP carries the freshly-applied header (the uploaded
      -- version + computed CRC) and a Last_Table_Operation_Status of
      -- Success. Active_Table_Update_Time was set from Sys_Time at the
      -- moment of Set (the tester's default Sys_Time is (0, 0), so the
      -- seconds field is 0 here).
      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 1.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => Header_Of (Table).Crc_Table,
          Last_Table_Operation_Status => Success)
      );

      -- No dump auto-emitted (Dump_On_Change is False):
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
   end Test_Set_Success;

   overriding procedure Test_Set_Length_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1) := [others => 0];
      Region : constant Memory_Region.T := (Address => Table'Address, Length => Test_Table_Size - 1);
   begin
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Downstream NOT contacted:
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 0);

      -- Upstream release reports Length_Error:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Length_Error)
      );

      -- Exactly one event (Memory_Region_Length_Mismatch) and one
      -- Table_Status DP. The raw counts ensure nothing else fired.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (
         T.Memory_Region_Length_Mismatch_History.Get (1),
         (Parameters_Region => (Region => Region, Operation => Set), Expected_Length => Test_Table_Size)
      );

      -- Table_Status DP fires for every Set (success or failure). Stored_Header
      -- is still at its defaults because no Set has succeeded yet.
      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 0.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => [0, 0],
          Last_Table_Operation_Status => Length_Error)
      );
   end Test_Set_Length_Error;

   overriding procedure Test_Set_Crc_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Region : Memory_Region.T;
      Planted_Header : Parameter_Table_Header.T;
      Corrupt_Header : Parameter_Table_Header.T;
   begin
      Build_Valid_Table (Table);
      -- Snapshot the valid CRC before corruption so we can compute what
      -- the forwarder will report as the "computed" CRC in the event.
      Planted_Header := Header_Of (Table);
      -- Corrupt the stored CRC after Build_Valid_Table:
      Table (Table'First .. Table'First + 1) := [16#DE#, 16#AD#];
      Corrupt_Header := Header_Of (Table);
      Region := (Address => Table'Address, Length => Test_Table_Size);

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 0);
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Crc_Error)
      );

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Memory_Region_Crc_Invalid_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Crc_Assert.Eq (
         T.Memory_Region_Crc_Invalid_History.Get (1),
         (Parameters_Region => (Region => Region, Operation => Set),
          Header => Corrupt_Header,
          Computed_Crc => Planted_Header.Crc_Table)
      );

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 0.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => [0, 0],
          Last_Table_Operation_Status => Crc_Error)
      );
   end Test_Set_Crc_Error;

   overriding procedure Test_Set_Downstream_Rejects (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Region : Memory_Region.T;
   begin
      Build_Valid_Table (Table);
      Region := (Address => Table'Address, Length => Test_Table_Size);
      T.Mock_Set_Release_Status := Parameter_Error;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 1);
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Downstream_Component_Rejected_Update_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Downstream_Component_Rejected_Update_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 0.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => [0, 0],
          Last_Table_Operation_Status => Parameter_Error)
      );
   end Test_Set_Downstream_Rejects;

   overriding procedure Test_Validate_Success (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Region : Memory_Region.T;
   begin
      Build_Valid_Table (Table);
      Region := (Address => Table'Address, Length => Test_Table_Size);
      T.Mock_Validate_Release_Status := Success;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Validate));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 1);
      Parameters_Memory_Region_Assert.Eq (
         T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get (1),
         (Region => Expected_Payload_Region (Region), Operation => Validate)
      );
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Success)
      );

      -- Exactly one event (Parameter_Table_Validated) + one DP. Validation
      -- does NOT update Stored_Header, so the DP carries default header
      -- values, NOT the validated table's version/CRC.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Parameter_Table_Validated_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Validated_History.Get (1), Region);

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 0.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => [0, 0],
          Last_Table_Operation_Status => Success)
      );
   end Test_Validate_Success;

   overriding procedure Test_Validate_Length_Crc_Errors (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Short_Region : constant Memory_Region.T := (Address => Table'Address, Length => Test_Table_Size - 1);
   begin
      -- Validate with wrong length:
      T.Parameters_Memory_Region_T_Send ((Region => Short_Region, Operation => Validate));
      Natural_Assert.Eq (T.Dispatch_All, 1);
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 0);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (
         T.Memory_Region_Length_Mismatch_History.Get (1),
         (Parameters_Region => (Region => Short_Region, Operation => Validate),
          Expected_Length => Test_Table_Size)
      );
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Short_Region, Status => Length_Error)
      );

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 0.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => [0, 0],
          Last_Table_Operation_Status => Length_Error)
      );

      -- Validate with bad CRC:
      Build_Valid_Table (Table);
      declare
         Planted_Header : constant Parameter_Table_Header.T := Header_Of (Table);
         Corrupt_Header : Parameter_Table_Header.T;
         Region : constant Memory_Region.T := (Address => Table'Address, Length => Test_Table_Size);
      begin
         Table (Table'First .. Table'First + 1) := [16#DE#, 16#AD#];
         Corrupt_Header := Header_Of (Table);

         T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Validate));
         Natural_Assert.Eq (T.Dispatch_All, 1);
         Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 0);

         Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
         Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 2);

         Natural_Assert.Eq (T.Memory_Region_Crc_Invalid_History.Get_Count, 1);
         Invalid_Parameters_Memory_Region_Crc_Assert.Eq (
            T.Memory_Region_Crc_Invalid_History.Get (1),
            (Parameters_Region => (Region => Region, Operation => Validate),
             Header => Corrupt_Header,
             Computed_Crc => Planted_Header.Crc_Table)
         );
         Parameters_Memory_Region_Release_Assert.Eq (
            T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (2),
            (Region => Region, Status => Crc_Error)
         );

         Natural_Assert.Eq (T.Table_Status_History.Get_Count, 2);
         Packed_Table_Operation_Status_Assert.Eq (
            T.Table_Status_History.Get (2),
            (Active_Table_Version_Number => 0.0,
             Active_Table_Update_Time    => 0,
             Active_Table_Crc            => [0, 0],
             Last_Table_Operation_Status => Crc_Error)
         );
      end;
   end Test_Validate_Length_Crc_Errors;

   overriding procedure Test_Validate_Downstream_Rejects (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Region : Memory_Region.T;
   begin
      Build_Valid_Table (Table);
      Region := (Address => Table'Address, Length => Test_Table_Size);
      T.Mock_Validate_Release_Status := Parameter_Error;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Validate));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Downstream_Component_Rejected_Validation_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Downstream_Component_Rejected_Validation_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 0.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => [0, 0],
          Last_Table_Operation_Status => Parameter_Error)
      );
   end Test_Validate_Downstream_Rejects;

   overriding procedure Test_Get_Success (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Test_Table_Size);
      -- Expected upstream bytes: stored header (zeros, since no Set has run)
      -- in the first Header.Size_In_Bytes bytes, then the downstream fill
      -- pattern in the payload portion.
      Expected_Header : constant Basic_Types.Byte_Array :=
         Parameter_Table_Header.Serialization.To_Byte_Array (
            (Crc_Table => [0, 0], Version => 0.0));
      Expected : Basic_Types.Byte_Array (0 .. Test_Table_Size - 1) :=
         [others => 16#AA#];
   begin
      T.Mock_Get_Release_Status := Success;
      T.Mock_Get_Fill_Pattern := 16#AA#;
      Expected (0 .. Parameter_Table_Header.Size_In_Bytes - 1) := Expected_Header;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Downstream contacted with the payload-only region (header offset):
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 1);
      Parameters_Memory_Region_Assert.Eq (
         T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get (1),
         (Region => Expected_Payload_Region (Region), Operation => Get_Copy)
      );

      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Success)
      );

      -- Exactly one event (Parameter_Table_Fetched). Get_Copy does NOT
      -- advance Table_Status bookkeeping, so the DP history is empty.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Parameter_Table_Fetched_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Fetched_History.Get (1), Region);

      -- Upstream memory now holds [stored header][payload from downstream]:
      Byte_Array_Assert.Eq (Memory, Expected);
   end Test_Get_Success;

   overriding procedure Test_Get_Length_Error (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 2) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Test_Table_Size - 1);
   begin
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 0);
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Length_Error)
      );

      -- One event, no DP (Get_Copy does not emit Table_Status):
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (
         T.Memory_Region_Length_Mismatch_History.Get (1),
         (Parameters_Region => (Region => Region, Operation => Get_Copy),
          Expected_Length => Test_Table_Size)
      );
   end Test_Get_Length_Error;

   overriding procedure Test_Get_Downstream_Rejects (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Test_Table_Size);
   begin
      T.Mock_Get_Release_Status := Parameter_Error;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Downstream_Component_Rejected_Fetch_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Downstream_Component_Rejected_Fetch_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );
   end Test_Get_Downstream_Rejects;

   overriding procedure Test_Get_Pointer_Rejected (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      -- Get_Pointer ignores the caller's region; pass an empty region to make
      -- that contract explicit at the test boundary.
      Empty_Region : constant Memory_Region.T := (Address => System.Null_Address, Length => 0);
   begin
      T.Parameters_Memory_Region_T_Send ((Region => Empty_Region, Operation => Get_Pointer));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Forwarder rejects without contacting the downstream:
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 0);

      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Empty_Region, Status => Parameter_Error)
      );

      -- Exactly one event (Get_Pointer_Not_Supported), no DP:
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Get_Pointer_Not_Supported_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Get_Pointer_Not_Supported_History.Get (1), Empty_Region);
   end Test_Get_Pointer_Rejected;

   overriding procedure Test_Dump_Command_Success (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      First_Dump, Second_Dump : Memory_Packetizer_Types.Memory_Dump;
   begin
      T.Connect_Memory_Dump;
      T.Mock_Get_Release_Status := Success;

      T.Command_T_Send (T.Commands.Dump_Parameter_Table);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Internal Get_Pointer to downstream happened exactly once. The mock
      -- echoes back Arg.Region (empty) since it has no real buffer; the
      -- forwarder relays that as the payload region of the second Memory_Dump.
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 1);
      Parameter_Table_Operation_Type_Assert.Eq (
         T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get (1).Operation,
         Get_Pointer);

      -- Two Memory_Dump records emitted under the same Active_Parameters
      -- packet ID: the first carries the forwarder's stored header buffer
      -- (Crc_Table + Version), the second carries the downstream payload
      -- region returned by Get_Pointer.
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 2);
      First_Dump := T.Memory_Dump_Recv_Sync_History.Get (1);
      Second_Dump := T.Memory_Dump_Recv_Sync_History.Get (2);
      -- Both records carry the same Active_Parameters packet ID. (The
      -- specific numeric value depends on Id_Base wiring; we just check
      -- consistency across the two records of a single dump.)
      Natural_Assert.Eq (Natural (First_Dump.Id), Natural (Second_Dump.Id));
      -- Header dump covers exactly the 6-byte Parameter_Table_Header buffer.
      Natural_Assert.Eq (
         Byte_Array_Pointer.Length (First_Dump.Memory_Pointer),
         Parameter_Table_Header.Size_In_Bytes);

      -- And the bytes are the wire-format serialization of Stored_Header.
      -- No Set has run, so Stored_Header is at its default (Crc=[0,0], Version=0.0).
      -- This pins down the in-memory == wire-format assumption for
      -- Parameter_Table_Header.T (a regression in the packed-record layout
      -- would surface here).
      declare
         Emitted_Header : constant Basic_Types.Byte_Array :=
            Byte_Array_Pointer.To_Byte_Array (First_Dump.Memory_Pointer);
         Expected_Header : constant Basic_Types.Byte_Array :=
            Parameter_Table_Header.Serialization.To_Byte_Array (
               (Crc_Table => [0, 0], Version => 0.0));
      begin
         Byte_Array_Assert.Eq (Emitted_Header, Expected_Header);
      end;

      -- Exactly one event (Dumped_Parameters). No DP -- the dump command
      -- does not advance Table_Status bookkeeping.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get (1), 0);

      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (
         T.Command_Response_T_Recv_Sync_History.Get (1),
         (Source_Id => 0, Registration_Id => 0,
          Command_Id => T.Commands.Get_Dump_Parameter_Table_Id,
          Status => Command_Response_Status.Success)
      );
   end Test_Dump_Command_Success;

   overriding procedure Test_Dump_Command_Failure (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      T.Connect_Memory_Dump;
      T.Mock_Get_Release_Status := Parameter_Error;

      T.Command_T_Send (T.Commands.Dump_Parameter_Table);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);

      -- Exactly one event (Dump_Failed with downstream's status). No DP.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Dump_Failed_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Dump_Failed_History.Get (1),
         (Region => (Address => System.Null_Address, Length => Test_Table_Size),
          Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (
         T.Command_Response_T_Recv_Sync_History.Get (1),
         (Source_Id => 0, Registration_Id => 0,
          Command_Id => T.Commands.Get_Dump_Parameter_Table_Id,
          Status => Command_Response_Status.Failure)
      );
   end Test_Dump_Command_Failure;

   overriding procedure Test_Dump_Command_No_Dump_Connector (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Update_Status;
      use System.Storage_Elements;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
   begin
      -- Note: Connect_Memory_Dump is intentionally NOT called here, so the
      -- Memory_Dump_Send connector is unconnected.
      T.Command_T_Send (T.Commands.Dump_Parameter_Table);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- No dump pathway, so nothing is forwarded or dumped:
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 0);
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);

      -- Exactly one event (Dump_Failed with the sentinel region) and no DP.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Dump_Failed_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Dump_Failed_History.Get (1),
         (Region => (Address => To_Address (16#DEAD_BEEF#), Length => 0),
          Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (
         T.Command_Response_T_Recv_Sync_History.Get (1),
         (Source_Id => 0, Registration_Id => 0,
          Command_Id => T.Commands.Get_Dump_Parameter_Table_Id,
          Status => Command_Response_Status.Failure)
      );
   end Test_Dump_Command_No_Dump_Connector;

   overriding procedure Test_Dump_On_Change (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Region : Memory_Region.T;
   begin
      T.Connect_Memory_Dump;
      -- Re-init with Dump_On_Change enabled.
      T.Component_Instance.Init (Table_Size => Test_Table_Size, Dump_Parameters_On_Change => True);

      Build_Valid_Table (Table);
      Region := (Address => Table'Address, Length => Test_Table_Size);
      T.Mock_Set_Release_Status := Success;
      T.Mock_Get_Release_Status := Success;
      T.Mock_Get_Fill_Pattern := 16#42#;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- One Set + one internal Get were forwarded:
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 2);
      -- Two Memory_Dump records per dump (header + payload):
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 2);

      -- Two events (Parameter_Table_Updated + Dumped_Parameters), one DP.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Parameter_Table_Updated_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Updated_History.Get (1), Region);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get_Count, 1);
      Natural_Assert.Eq (T.Dumped_Parameters_History.Get (1), 0);

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 1.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => Header_Of (Table).Crc_Table,
          Last_Table_Operation_Status => Success)
      );
   end Test_Dump_On_Change;

   overriding procedure Test_Dump_On_Change_Failure (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1);
      Region : Memory_Region.T;
   begin
      T.Connect_Memory_Dump;
      -- Re-init with Dump_On_Change enabled.
      T.Component_Instance.Init (Table_Size => Test_Table_Size, Dump_Parameters_On_Change => True);

      Build_Valid_Table (Table);
      Region := (Address => Table'Address, Length => Test_Table_Size);
      -- Set succeeds, but the internal Get_Pointer that drives the auto-dump fails.
      T.Mock_Set_Release_Status := Success;
      T.Mock_Get_Release_Status := Parameter_Error;

      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      -- Both forwards happened (Set + the failing Get_Pointer):
      Natural_Assert.Eq (T.Parameters_Memory_Region_T_Forwarded_Reciprocal_History.Get_Count, 2);
      -- No Memory_Dump records emitted because the internal Get_Pointer failed before either send:
      Natural_Assert.Eq (T.Memory_Dump_Recv_Sync_History.Get_Count, 0);
      -- The Set's upstream release reports the dump failure, not Success:
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );

      -- Two events (Parameter_Table_Updated -- Set landed at downstream --
      -- followed by Dump_Failed for the failing auto-dump). One DP, whose
      -- Last_Table_Operation_Status reflects the dump failure rather than
      -- the underlying Set success.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 2);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Parameter_Table_Updated_History.Get_Count, 1);
      Memory_Region_Assert.Eq (T.Parameter_Table_Updated_History.Get (1), Region);
      Natural_Assert.Eq (T.Dump_Failed_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Dump_Failed_History.Get (1),
         (Region => Region, Status => Parameter_Error)
      );

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 1.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => Header_Of (Table).Crc_Table,
          Last_Table_Operation_Status => Parameter_Error)
      );
   end Test_Dump_On_Change_Failure;

   overriding procedure Test_Command_Dropped (Self : in out Instance) is
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
   begin
      Cmd.Header.Arg_Buffer_Length := Cmd.Arg_Buffer'Length;
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);

      T.Expect_Command_T_Send_Dropped := True;
      T.Command_T_Send (Cmd);

      -- Exactly one event (Command_Dropped) and no DPs. The three accepted
      -- commands are bogus headers that fall through Execute_Command's
      -- "unknown id" branch -- they do not exercise the dump path and do
      -- not emit any forwarder events.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Command_Dropped_History.Get_Count, 1);
      Command_Header_Assert.Eq (T.Command_Dropped_History.Get (1), Cmd.Header);
   end Test_Command_Dropped;

   overriding procedure Test_Memory_Region_Dropped (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T;
      Memory : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 1) := [others => 0];
      Region : constant Memory_Region.T := (Address => Memory'Address, Length => Test_Table_Size);
   begin
      -- Fill the queue with 3 commands to leave no room for the region.
      Cmd.Header.Arg_Buffer_Length := Cmd.Arg_Buffer'Length;
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);
      T.Command_T_Send (Cmd);

      -- The region send should now overflow the queue.
      T.Expect_Parameters_Memory_Region_T_Send_Dropped := True;
      T.Parameters_Memory_Region_T_Send ((Region => Region, Operation => Get_Copy));

      Natural_Assert.Eq (T.Memory_Region_Dropped_History.Get_Count, 1);
      Parameters_Memory_Region_Assert.Eq (
         T.Memory_Region_Dropped_History.Get (1),
         (Region => Region, Operation => Get_Copy)
      );

      -- The dropped region was released with Dropped status:
      Natural_Assert.Eq (T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get_Count, 1);
      Parameters_Memory_Region_Release_Assert.Eq (
         T.Parameters_Memory_Region_Release_T_Recv_Sync_History.Get (1),
         (Region => Region, Status => Parameter_Enums.Parameter_Table_Update_Status.Dropped)
      );

      -- Exactly one event (Memory_Region_Dropped); no DPs (the region was
      -- never processed, so no Table_Status update).
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);
   end Test_Memory_Region_Dropped;

   overriding procedure Test_Initial_Table_Status (Self : in out Instance) is
      use Parameter_Enums.Parameter_Table_Operation_Type;
      use Parameter_Enums.Parameter_Table_Update_Status;
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Table : aliased Basic_Types.Byte_Array (0 .. Test_Table_Size - 2) := [others => 0];
      Short_Region : constant Memory_Region.T := (Address => Table'Address, Length => Test_Table_Size - 1);
   begin
      -- Set_Up_Test does Init + Set_Up but no operation has been performed
      -- yet, so the forwarder has not emitted a Table_Status DP.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 0);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      -- Drive a known-failing Set (length error -- never contacts downstream)
      -- so the very first Table_Status DP fires while Stored_Header is still
      -- at its initial defaults. This pins down what an operator would see
      -- when telemetry first arrives for a freshly-initialized forwarder.
      T.Parameters_Memory_Region_T_Send ((Region => Short_Region, Operation => Set));
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 1);

      Natural_Assert.Eq (T.Memory_Region_Length_Mismatch_History.Get_Count, 1);
      Invalid_Parameters_Memory_Region_Length_Assert.Eq (
         T.Memory_Region_Length_Mismatch_History.Get (1),
         (Parameters_Region => (Region => Short_Region, Operation => Set),
          Expected_Length => Test_Table_Size)
      );

      Natural_Assert.Eq (T.Table_Status_History.Get_Count, 1);
      Packed_Table_Operation_Status_Assert.Eq (
         T.Table_Status_History.Get (1),
         (Active_Table_Version_Number => 0.0,
          Active_Table_Update_Time    => 0,
          Active_Table_Crc            => [0, 0],
          Last_Table_Operation_Status => Length_Error)
      );
   end Test_Initial_Table_Status;

   overriding procedure Test_Invalid_Command (Self : in out Instance) is
      T : Component.Parameter_Table_Forwarder.Implementation.Tester.Instance_Access renames Self.Tester;
      Cmd : Command.T := T.Commands.Dump_Parameter_Table;
   begin
      Cmd.Header.Arg_Buffer_Length := 22;

      T.Command_T_Send (Cmd);
      Natural_Assert.Eq (T.Dispatch_All, 1);

      Natural_Assert.Eq (T.Command_Response_T_Recv_Sync_History.Get_Count, 1);
      Command_Response_Assert.Eq (
         T.Command_Response_T_Recv_Sync_History.Get (1),
         (Source_Id => 0, Registration_Id => 0,
          Command_Id => T.Commands.Get_Dump_Parameter_Table_Id,
          Status => Command_Response_Status.Length_Error)
      );

      -- Exactly one event (Invalid_Command_Received), no DP.
      Natural_Assert.Eq (T.Event_T_Recv_Sync_History.Get_Count, 1);
      Natural_Assert.Eq (T.Data_Product_T_Recv_Sync_History.Get_Count, 0);

      Natural_Assert.Eq (T.Invalid_Command_Received_History.Get_Count, 1);
      Invalid_Command_Info_Assert.Eq (
         T.Invalid_Command_Received_History.Get (1),
         (Id => T.Commands.Get_Dump_Parameter_Table_Id,
          Errant_Field_Number => Interfaces.Unsigned_32'Last,
          Errant_Field => [0, 0, 0, 0, 0, 0, 0, 22])
      );
   end Test_Invalid_Command;

end Parameter_Table_Forwarder_Tests.Implementation;
