with Command;
with Fault_Types;
with Fault_Correction_Enums;

package Fault_Correction_Types is

   --
   -- A fault response configuration contains the following information:
   --
   --    Fault ID - the fault ID that we are responding to
   --    Latching Flag - is the fault latching or non-latching. A latching fault only fires a single time
   --                            until unlatched from the ground.
   --    Startup_Status - Disabled, Enabled on startup
   --    Command - The command to send as response when the fault is received.
   --
   type Fault_Response_Config is record
      Id : Fault_Types.Fault_Id := Fault_Types.Fault_Id'First;
      Latching : Fault_Correction_Enums.Latching_Type.E := Fault_Correction_Enums.Latching_Type.Non_Latching;
      Startup_State : Fault_Correction_Enums.Startup_Status_Type.E := Fault_Correction_Enums.Startup_Status_Type.Enabled;
      Command_Response : Command.T;
   end record;

   -- Fault response array types:
   type Fault_Response_Config_List is array (Natural range <>) of Fault_Response_Config;
   type Fault_Response_Config_List_Access is access all Fault_Response_Config_List;

   -- Padding bits for status packet:
   type Two_Bit_Padding_Type is mod 2**2;

end Fault_Correction_Types;
