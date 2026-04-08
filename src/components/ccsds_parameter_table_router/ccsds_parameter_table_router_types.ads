with Parameter_Types;
with Connector_Types;

package Ccsds_Parameter_Table_Router_Types is

   -- A single destination entry with metadata:
   type Destination_Entry is record
      Connector_Index : Connector_Types.Connector_Index_Type := Connector_Types.Connector_Index_Type'First;
      Load_From : Boolean := False;
   end record;

   -- Destination table: array of destination entries for a single table ID:
   type Destination_Table is array (Natural range <>) of Destination_Entry;
   type Destination_Table_Access is access all Destination_Table;

   -- A router table entry maps a parameter table ID to its destinations:
   type Router_Table_Entry is record
      Table_Id : Parameter_Types.Parameter_Table_Id := 0;
      Destinations : Destination_Table_Access := null;
   end record;

   -- The full router table passed to Init:
   type Router_Table is array (Natural range <>) of Router_Table_Entry;

end Ccsds_Parameter_Table_Router_Types;
