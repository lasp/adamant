--------------------------------------------------------------------------------
-- Parameter_Table_Forwarder Tests Spec
--------------------------------------------------------------------------------

-- This is a unit test suite for the Parameter Table Forwarder component.
package Parameter_Table_Forwarder_Tests.Implementation is

   -- Test data and state:
   type Instance is new Parameter_Table_Forwarder_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- A valid Set memory region is forwarded to the downstream which acks Success.
   -- Bookkeeping updates, Parameter_Table_Updated event fires.
   overriding procedure Test_Set_Success (Self : in out Instance);
   -- Set with wrong region length is rejected without contacting the downstream.
   overriding procedure Test_Set_Length_Error (Self : in out Instance);
   -- Set with mismatched CRC is rejected without contacting the downstream.
   overriding procedure Test_Set_Crc_Error (Self : in out Instance);
   -- Downstream returns Parameter_Error on Set. Forwarder propagates and does not
   -- update Stored_Crc.
   overriding procedure Test_Set_Downstream_Rejects (Self : in out Instance);
   -- Validate forwarded; downstream acks Success. Stored_Crc is NOT updated.
   overriding procedure Test_Validate_Success (Self : in out Instance);
   -- Validate forwarded; downstream returns Parameter_Error.
   overriding procedure Test_Validate_Downstream_Rejects (Self : in out Instance);
   -- Validate rejected for length/CRC mismatches without contacting the downstream.
   overriding procedure Test_Validate_Length_Crc_Errors (Self : in out Instance);
   -- Get forwarded; downstream writes bytes and acks. Forwarder propagates release
   -- upstream.
   overriding procedure Test_Get_Success (Self : in out Instance);
   -- Get region smaller than Table_Size is rejected without contacting the
   -- downstream.
   overriding procedure Test_Get_Length_Error (Self : in out Instance);
   -- Downstream returns Parameter_Error on Get.
   overriding procedure Test_Get_Downstream_Rejects (Self : in out Instance);
   -- An external Get_Pointer request is rejected by the forwarder without
   -- contacting the downstream; emits Get_Pointer_Not_Supported event and
   -- returns Parameter_Error.
   overriding procedure Test_Get_Pointer_Rejected (Self : in out Instance);
   -- Dump command issues internal Get; downstream writes bytes; Active_Parameters
   -- packet emitted with CRC prefix.
   overriding procedure Test_Dump_Command_Success (Self : in out Instance);
   -- Downstream rejects internal Get during Dump; Dump_Failed event fires and
   -- command response is Failure.
   overriding procedure Test_Dump_Command_Failure (Self : in out Instance);
   -- Dump command with no Memory_Dump_Send connector wired: Dump_Failed event
   -- fires with a zero-length sentinel region and the command response is
   -- Failure, without contacting the downstream.
   overriding procedure Test_Dump_Command_No_Dump_Connector (Self : in out Instance);
   -- After a successful Set with Dump_On_Change=True, an Active_Parameters packet is
   -- auto-emitted.
   overriding procedure Test_Dump_On_Change (Self : in out Instance);
   -- After a successful Set with Dump_On_Change=True, the auto-dump's internal
   -- Get_Pointer is rejected; Dump_Failed event fires and the Set's upstream
   -- release reports Parameter_Error.
   overriding procedure Test_Dump_On_Change_Failure (Self : in out Instance);
   -- Command queue full triggers Command_Dropped event.
   overriding procedure Test_Command_Dropped (Self : in out Instance);
   -- Memory region queue full triggers Memory_Region_Dropped event and a Dropped
   -- release upstream.
   overriding procedure Test_Memory_Region_Dropped (Self : in out Instance);
   -- Before any operation the Table_Status DP reports Uninitialized.
   overriding procedure Test_Initial_Table_Status (Self : in out Instance);
   -- A command with bad length triggers an Invalid_Command_Received event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);

   -- Test data and state:
   type Instance is new Parameter_Table_Forwarder_Tests.Base_Instance with record
      null;
   end record;
end Parameter_Table_Forwarder_Tests.Implementation;
