-- This package stores task watchdog connector state and configuration entries.
with Task_Watchdog_Types;
with Task_Watchdog_Enums;
with Connector_Types;
with Fault_Types;

package Watchdog_List is
   use Task_Watchdog_Types;
   use Task_Watchdog_Enums;
   type Instance is tagged limited private;

   type Check_Status is (Disable, Petting, Warn_Failure, Fault_Failure, Repeat_Failure);
   --
   -- Initialization/destruction functions:
   --
   -- Init Parameters:
   -- Task_Watchdog_Entry_Init_List : Task_Watchdog_Types.Task_Watchdog_Init_List_Access - The list of components that have a watchdog to pet that need to be tracked by the task watchdog.
   --
   procedure Init (Self : in out Instance; Task_Watchdog_Entry_Init_List : in Task_Watchdog_Init_List);

   -- Procedures to set data in our data structure given an index
   procedure Reset_Pet_Count (Self : in Instance; Index : in Connector_Types.Connector_Index_Type);
   procedure Set_Pet_Limit (Self : in Instance; Index : in Connector_Types.Connector_Index_Type; New_Limit : in Missed_Pet_Limit_Type);
   procedure Set_Pet_Action (Self : in Instance; Index : in Connector_Types.Connector_Index_Type; New_Action : in Watchdog_Action_State.E);
   -- Function to check the watchdog petting status and counters. Returns if the pets are still being received or not.
   function Check_Watchdog_Pets (Self : in Instance; Index : in Connector_Types.Connector_Index_Type; Is_Critical : out Boolean) return Check_Status;
   -- Functions for the component to fetch the static critical and action items.
   function Get_Pet_Criticality (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Boolean;
   function Get_Pet_Action (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Watchdog_Action_State.E;
   function Get_Pet_Action_Id (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Fault_Types.Fault_Id;
   function Get_Pet_Has_Fault (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Boolean;
   -- Getters for the commands to make sure that the item was set.
   function Get_Pet_Count_Limit (Self : in Instance; Index : in Connector_Types.Connector_Index_Type) return Missed_Pet_Limit_Type;

private

   type Task_Watchdog_Entry is record
      -- The current limit count.
      Missed_Pet_Limit : Missed_Pet_Limit_Type := Missed_Pet_Limit_Type'First;
      -- The current count of ticks since the last pet received for this connector.
      Missed_Pet_Count : Missed_Pet_Count_Type := Missed_Pet_Count_Type'First;
      -- Flag to indicate if this tasks pets are critical to the software running.
      Critical : Boolean := True;
      -- Fault action to take for the entry if it fails
      Action : Watchdog_Action_State.E := Watchdog_Action_State.Error_Fault;
      -- Fault id of the action if applicable
      Action_Id : Fault_Types.Fault_Id := 0;
      -- Flag to indicate if the petter contains a fault
      Petter_Has_Fault : Boolean := False;
   end record;

   -- List of product entries of each apid:
   type Task_Watchdog_Pet_List is array (Connector_Types.Connector_Index_Type range <>) of Task_Watchdog_Entry;
   type Task_Watchdog_Pet_List_Access is access all Task_Watchdog_Pet_List;

   type Instance is tagged limited record
      -- Task list for monitoring the pet connections
      Task_Watchdog_Pet_Connections : Task_Watchdog_Pet_List_Access := null;
   end record;

end Watchdog_List;
