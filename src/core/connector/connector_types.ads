package Connector_Types is
   -- Arrayed connector definitions:
   type Connector_Count_Type is new Natural range 0 .. 65_535;
   subtype Connector_Index_Type is Connector_Count_Type range 1 .. Connector_Count_Type'Last;

   -- Queued connector enumerations.
   type Connector_Status is (Success, Message_Dropped);
   type Full_Queue_Action is (Drop, Wait);
end Connector_Types;
