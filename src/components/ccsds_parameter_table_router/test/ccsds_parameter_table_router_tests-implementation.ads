--------------------------------------------------------------------------------
-- Ccsds_Parameter_Table_Router Tests Spec
--------------------------------------------------------------------------------

-- Unit tests for the Parameter Table Router component.
package Ccsds_Parameter_Table_Router_Tests.Implementation is

   -- Test data and state:
   type Instance is new Ccsds_Parameter_Table_Router_Tests.Base_Instance with private;
   type Class_Access is access all Instance'Class;

private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- Verify Init populates binary tree, creates staging buffer, and validates
   -- routing table.
   overriding procedure Test_Init (Self : in out Instance);
   -- Verify Set_Up publishes initial data product values.
   overriding procedure Test_Set_Up (Self : in out Instance);
   -- Init with Load_All_Parameter_Tables_On_Set_Up. Verify tables are loaded during
   -- Set_Up.
   overriding procedure Test_Set_Up_Load_All (Self : in out Instance);
   -- Send First + Continuation + Last for a known table ID. Verify events and data
   -- products.
   overriding procedure Test_Nominal_Segmented_Upload (Self : in out Instance);
   -- Send a single Unsegmented packet for a known table ID. Verify events and data
   -- products.
   overriding procedure Test_Unsegmented_Upload (Self : in out Instance);
   -- Verify non-Load_From destinations sent first in YAML order, Load_From last.
   overriding procedure Test_Multi_Destination_Order (Self : in out Instance);
   -- Continuation and Last without prior First produce Packet_Ignored events.
   overriding procedure Test_Packet_Ignored (Self : in out Instance);
   -- FirstSegment with less than 2 bytes produces Too_Small_Table event.
   overriding procedure Test_Too_Small_Table (Self : in out Instance);
   -- Data exceeding buffer capacity produces Staging_Buffer_Overflow event.
   overriding procedure Test_Buffer_Overflow (Self : in out Instance);
   -- Complete table with unknown ID produces Unrecognized_Table_Id event and
   -- increments invalid counter.
   overriding procedure Test_Unrecognized_Table_Id (Self : in out Instance);
   -- Downstream returns failure status. Verify Table_Update_Failure event with
   -- correct info.
   overriding procedure Test_Destination_Failure (Self : in out Instance);
   -- Downstream does not respond. Verify Table_Update_Timeout event.
   overriding procedure Test_Destination_Timeout (Self : in out Instance);
   -- Multi-destination table where first destination fails. Remaining destinations
   -- not sent to.
   overriding procedure Test_Partial_Failure_Stops (Self : in out Instance);
   -- Load a table from its load_from source. Verify Loading_Table and Table_Loaded
   -- events.
   overriding procedure Test_Load_Table_Nominal (Self : in out Instance);
   -- Load command for table with no load_from. Verify No_Load_Source event and
   -- command failure.
   overriding procedure Test_Load_Table_No_Load_Source (Self : in out Instance);
   -- Load command for unknown table ID. Verify event and command failure.
   overriding procedure Test_Load_Table_Unrecognized (Self : in out Instance);
   -- Load fails during Get or Set. Verify failure events and command failure.
   overriding procedure Test_Load_Table_Failure (Self : in out Instance);
   -- Load all tables. Verify start/end events and per-table events.
   overriding procedure Test_Load_All_Nominal (Self : in out Instance);
   -- One table fails during load all. Verify command failure and other tables still
   -- attempted.
   overriding procedure Test_Load_All_Partial_Failure (Self : in out Instance);
   -- Overflow CCSDS packet queue. Verify Packet_Dropped event.
   overriding procedure Test_Packet_Dropped (Self : in out Instance);
   -- Overflow command queue. Verify Command_Dropped event.
   overriding procedure Test_Command_Dropped (Self : in out Instance);
   -- Send command with corrupted arguments. Verify Invalid_Command_Received event.
   overriding procedure Test_Invalid_Command (Self : in out Instance);
   -- Upload table where non-Load_From succeeds but Load_From (sent last) fails.
   overriding procedure Test_Load_From_Destination_Failure (Self : in out Instance);
   -- Load command where Get succeeds but subsequent Set to non-Load_From fails.
   overriding procedure Test_Load_Command_Set_Failure (Self : in out Instance);

   -- Test data and state:
   type Instance is new Ccsds_Parameter_Table_Router_Tests.Base_Instance with record
      null;
   end record;
end Ccsds_Parameter_Table_Router_Tests.Implementation;
