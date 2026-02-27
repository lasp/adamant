with Return_Connector;
with In_Connector;
with In_Return_Connector;
with In_Out_Connector;
with Sys_Time;
with Event;
with Command;
with Command_Response;
with Data_Product;
with Packet;
with Fault;
with Tick;
with Ccsds_Space_Packet;
with Parameter_Update;
with Data_Product_Fetch;
with Data_Product_Return;
with Control_Input;
with Control_Output;
with Pet;

package Common_Connectors is

   -- This package contains definitions for commonly employed connectors in the
   -- Adamant system. The purpose of this package is to consolidate instantiation
   -- of these generic packages into a single place that can be used by multiple
   -- components. Doing this provides a huge savings on the resulting code size
   -- of the compiled binary, by not duplicating generic connector instantiations.
   package Sys_Time_T_Return_Connector is new Return_Connector (Sys_Time.T);
   package Data_Product_T_In_Connector is new In_Connector (Data_Product.T);
   package Command_T_In_Connector is new In_Connector (Command.T);
   package Command_Response_T_In_Connector is new In_Connector (Command_Response.T);
   package Event_T_In_Connector is new In_Connector (Event.T);
   package Tick_T_In_Connector is new In_Connector (Tick.T);
   package Packet_T_In_Connector is new In_Connector (Packet.T);
   package Ccsds_Space_Packet_T_In_Connector is new In_Connector (Ccsds_Space_Packet.T);
   package Parameter_Update_T_In_Out_Connector is new In_Out_Connector (Parameter_Update.T);
   package Control_Input_U_In_Connector is new In_Connector (Control_Input.U);
   package Control_Output_U_In_Connector is new In_Connector (Control_Output.U);
   package Pet_T_In_Connector is new In_Connector (Pet.T);
   package Fault_T_In_Connector is new In_Connector (Fault.T);
   package Data_Product_Fetch_T_In_Return_Connector is new In_Return_Connector (Data_Product_Fetch.T, Data_Product_Return.T);

end Common_Connectors;
