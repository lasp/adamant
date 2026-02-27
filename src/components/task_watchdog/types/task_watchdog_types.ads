with Connector_Types;
with Fault_Types;
with Task_Watchdog_Enums;

package Task_Watchdog_Types is
   use Task_Watchdog_Enums;
   -- Counter types for compatibility and limits
   type Missed_Pet_Count_Type is new Natural range Natural'First .. 65535;
   subtype Missed_Pet_Limit_Type is Missed_Pet_Count_Type range Missed_Pet_Count_Type'First + 1 .. Missed_Pet_Count_Type'Last - 1;

   -- Define internal task watchdog entry:
   type Task_Watchdog_Input_Entry is record
      -- The maximum number of ticks since receiving a pet before sending the specified action
      Max_Missed_Pet_Limit : Missed_Pet_Limit_Type := Missed_Pet_Limit_Type'First;
      -- Flag to indicate if this tasks pets are critical to the software running.
      Critical : Boolean := True;
      -- Fault action to take for the entry
      Action : Watchdog_Action_State.E := Watchdog_Action_State.Error_Fault;
      -- Fault id if applicable
      Action_Id : Fault_Types.Fault_Id := 0;
      -- Flag to indicate if the petter contains a fault
      Petter_Has_Fault : Boolean := False;
   end record;

   -- List of product entries of each apid:
   type Task_Watchdog_Init_List is array (Connector_Types.Connector_Index_Type range <>) of Task_Watchdog_Input_Entry;
   type Task_Watchdog_Init_List_Access is access all Task_Watchdog_Init_List;

   -- Padding bits for status packet:
   type Two_Bit_Padding_Type is mod 2**2;

end Task_Watchdog_Types;
