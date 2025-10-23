with Parameter_Types;
with Connector_Types;

package Parameters_Component_Types is

   -- Parameter entry definition.
   type Parameter_Table_Entry is record
      -- Id of parameter.
      Id : Parameter_Types.Parameter_Id := Parameter_Types.Parameter_Id'First;
      -- Id of parameter table entry (multiple parameters can share the same entry for grouped parameters).
      Entry_Id : Parameter_Types.Parameter_Table_Entry_Id := Parameter_Types.Parameter_Table_Entry_Id'First;
      -- Arrayed connector index that corresponds to parameter's destination
      -- component.
      Component_Id : Connector_Types.Connector_Index_Type := Connector_Types.Connector_Index_Type'First;
      -- Index location of parameter in table.
      Start_Index : Natural := 0;
      End_Index : Natural := 0;
   end record;

   -- List of parameter entries:
   type Parameter_Table_Entry_List is array (Natural range <>) of Parameter_Table_Entry;
   type Parameter_Table_Entry_List_Access is access all Parameter_Table_Entry_List;

end Parameters_Component_Types;
