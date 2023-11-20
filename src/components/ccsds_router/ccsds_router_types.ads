with Ccsds_Primary_Header;
with Connector_Types;

package Ccsds_Router_Types is

   -- Destination table, contains array of destinations which are indexes into the output
   -- connector.
   type Destination_Table is array (Natural range <>) of Connector_Types.Connector_Index_Type;
   type Destination_Table_Access is access all Destination_Table;

   -- A mode type which stores what kind of action to take regarding sequence counts
   -- for each APID:
   --
   --    No_Check       - sequence counts are not tracked or checked
   --    Warn             - sequence counts are tracked and checked. Out of order or duplicate sequence
   --                           counts are reported as events.
   --    Drop_Dupes    - sequence counts are tracked and checked. Consecutive packets with identical
   --                           sequence counts are dropped and reported as events. Out of order packets are
   --                           also reported in the same way as the "Warn" mode
   --
   type Sequence_Count_Mode_Type is (No_Check, Warn, Drop_Dupes);

   -- A router table entry contains an apid and a list of the destination to which
   -- packets of that APID must be forwarded:
   type Router_Table_Entry is record
      Apid : Ccsds_Primary_Header.Ccsds_Apid_Type := 0;
      Destinations : Destination_Table_Access := null;
      Sequence_Count_Mode : Sequence_Count_Mode_Type := No_Check;
   end record;
   type Router_Table_Entry_Array is array (Natural range <>) of Router_Table_Entry;

end Ccsds_Router_Types;
