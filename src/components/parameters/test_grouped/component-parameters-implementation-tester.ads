--------------------------------------------------------------------------------
-- Parameters Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Parameters_Reciprocal;
with Printable_History;
with Parameter_Update.Representation;
with Command_Response.Representation;
with Parameters_Memory_Region_Release.Representation;
with Packet.Representation;
with Event.Representation;
with Data_Product.Representation;
with Sys_Time.Representation;
with Event;
with Parameter_Table_Entry_Id.Representation;
with Parameter_Operation_Status.Representation;
with Invalid_Parameter_Length.Representation;
with Parameter_Entry_Comparison.Representation;
with Invalid_Parameter_Table_Entry_Length.Representation;
with Invalid_Parameters_Memory_Region_Length.Representation;
with Invalid_Parameters_Memory_Region_Crc.Representation;
with Memory_Region.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;
with Parameters_Memory_Region.Representation;
with Data_Product;
with Packed_Table_Operation_Status.Representation;
with Component.Test_Component_1.Implementation;
with Component.Test_Component_2.Implementation;

-- The Parameters Component is responsible for staging, updating, and reporting
-- the values of the "active" parameters being used in the system. The component
-- does not contain a parameter table itself. Instead, it acts as an interface for
-- the rest of the system to the components' internal staged parameters. The component
-- allows the staging and updating of parameters through a table upload (via
-- Memory_Region_T_Recv_Async) or updating of individual parameter values by
-- command. The component also provides a command to fetch all of the parameters
-- held within components and produce a packet with the fetched values. The
-- component can be configured to produce this packet automatically any time a
-- parameter change is requested.
package Component.Parameters.Implementation.Tester is

   use Component.Parameters_Reciprocal;
   -- Invoker connector history packages:
   package Parameter_Update_T_Modify_History_Package is new Printable_History (Parameter_Update.T, Parameter_Update.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Parameters_Memory_Region_Release_T_Recv_Sync_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Parameter_Update_Success_History_Package is new Printable_History (Parameter_Table_Entry_Id.T, Parameter_Table_Entry_Id.Representation.Image);
   package Parameter_Update_Id_Not_Recognized_History_Package is new Printable_History (Parameter_Table_Entry_Id.T, Parameter_Table_Entry_Id.Representation.Image);
   package Parameter_Stage_Failed_History_Package is new Printable_History (Parameter_Operation_Status.T, Parameter_Operation_Status.Representation.Image);
   package Parameter_Update_Failed_History_Package is new Printable_History (Parameter_Operation_Status.T, Parameter_Operation_Status.Representation.Image);
   package Parameter_Validation_Failed_History_Package is new Printable_History (Parameter_Operation_Status.T, Parameter_Operation_Status.Representation.Image);
   package Parameter_Fetch_Failed_History_Package is new Printable_History (Parameter_Operation_Status.T, Parameter_Operation_Status.Representation.Image);
   package Parameter_Fetch_Length_Mismatch_History_Package is new Printable_History (Invalid_Parameter_Length.T, Invalid_Parameter_Length.Representation.Image);
   package Parameter_Fetch_Value_Mismatch_History_Package is new Printable_History (Parameter_Entry_Comparison.T, Parameter_Entry_Comparison.Representation.Image);
   package Parameter_Update_Length_Mismatch_History_Package is new Printable_History (Invalid_Parameter_Table_Entry_Length.T, Invalid_Parameter_Table_Entry_Length.Representation.Image);
   package Memory_Region_Length_Mismatch_History_Package is new Printable_History (Invalid_Parameters_Memory_Region_Length.T, Invalid_Parameters_Memory_Region_Length.Representation.Image);
   package Memory_Region_Crc_Invalid_History_Package is new Printable_History (Invalid_Parameters_Memory_Region_Crc.T, Invalid_Parameters_Memory_Region_Crc.Representation.Image);
   package Dumping_Parameters_History_Package is new Printable_History (Natural, Natural'Image);
   package Finished_Dumping_Parameters_History_Package is new Printable_History (Natural, Natural'Image);
   package Starting_Parameter_Table_Update_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Finished_Parameter_Table_Update_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Starting_Parameter_Table_Validate_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Finished_Parameter_Table_Validate_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Starting_Parameter_Table_Fetch_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Finished_Parameter_Table_Fetch_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Memory_Region_Dropped_History_Package is new Printable_History (Parameters_Memory_Region.T, Parameters_Memory_Region.Representation.Image);

   -- Data product history packages:
   package Table_Status_History_Package is new Printable_History (Packed_Table_Operation_Status.T, Packed_Table_Operation_Status.Representation.Image);

   -- Packet history packages:
   package Active_Parameters_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Parameters_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Parameters.Implementation.Instance;
      -- Connector histories:
      Parameter_Update_T_Modify_History : Parameter_Update_T_Modify_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Parameters_Memory_Region_Release_T_Recv_Sync_History : Parameters_Memory_Region_Release_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Parameter_Update_Success_History : Parameter_Update_Success_History_Package.Instance;
      Parameter_Update_Id_Not_Recognized_History : Parameter_Update_Id_Not_Recognized_History_Package.Instance;
      Parameter_Stage_Failed_History : Parameter_Stage_Failed_History_Package.Instance;
      Parameter_Update_Failed_History : Parameter_Update_Failed_History_Package.Instance;
      Parameter_Validation_Failed_History : Parameter_Validation_Failed_History_Package.Instance;
      Parameter_Fetch_Failed_History : Parameter_Fetch_Failed_History_Package.Instance;
      Parameter_Fetch_Length_Mismatch_History : Parameter_Fetch_Length_Mismatch_History_Package.Instance;
      Parameter_Fetch_Value_Mismatch_History : Parameter_Fetch_Value_Mismatch_History_Package.Instance;
      Parameter_Update_Length_Mismatch_History : Parameter_Update_Length_Mismatch_History_Package.Instance;
      Memory_Region_Length_Mismatch_History : Memory_Region_Length_Mismatch_History_Package.Instance;
      Memory_Region_Crc_Invalid_History : Memory_Region_Crc_Invalid_History_Package.Instance;
      Dumping_Parameters_History : Dumping_Parameters_History_Package.Instance;
      Finished_Dumping_Parameters_History : Finished_Dumping_Parameters_History_Package.Instance;
      Starting_Parameter_Table_Update_History : Starting_Parameter_Table_Update_History_Package.Instance;
      Finished_Parameter_Table_Update_History : Finished_Parameter_Table_Update_History_Package.Instance;
      Starting_Parameter_Table_Validate_History : Starting_Parameter_Table_Validate_History_Package.Instance;
      Finished_Parameter_Table_Validate_History : Finished_Parameter_Table_Validate_History_Package.Instance;
      Starting_Parameter_Table_Fetch_History : Starting_Parameter_Table_Fetch_History_Package.Instance;
      Finished_Parameter_Table_Fetch_History : Finished_Parameter_Table_Fetch_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Command_Dropped_History : Command_Dropped_History_Package.Instance;
      Memory_Region_Dropped_History : Memory_Region_Dropped_History_Package.Instance;
      -- Data product histories:
      Table_Status_History : Table_Status_History_Package.Instance;
      -- Packet histories:
      Active_Parameters_History : Active_Parameters_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Parameters_Memory_Region_T_Send_Dropped : Boolean := False;
      Parameters_Memory_Region_T_Send_Dropped_Count : Natural := 0;
      --
      -- Some custom items. We actually want to store our three destination
      -- components here, so that we can send parameter updates to them. This
      -- has the benefit of testing the autocoded logic inside those components
      -- as well.
      --
      Component_A : aliased Component.Test_Component_1.Implementation.Instance;
      Component_B : aliased Component.Test_Component_1.Implementation.Instance;
      Component_C : aliased Component.Test_Component_2.Implementation.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Parameter_Update_T_Provide_Count : in Connector_Count_Type; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The arrayed parameter request connector. Parameters stages, updates, and
   -- fetches are sent out this connector and a status is returned.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T);
   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- After a memory region is received on the Memory_Region_T_Recv_Async connector
   -- and then processed, it is released via a call to this connector. A status is
   -- also returned, so the downstream component can determine if the parameter
   -- update was successful or not.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- The parameter packet connector. A copy of the active parameters is dumped via
   -- this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Parameters_Memory_Region_T_Send message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Send_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A parameter table entry was updated.
   overriding procedure Parameter_Update_Success (Self : in out Instance; Arg : in Parameter_Table_Entry_Id.T);
   -- A parameter table entry could not be updated because the Entry ID is not
   -- recognized.
   overriding procedure Parameter_Update_Id_Not_Recognized (Self : in out Instance; Arg : in Parameter_Table_Entry_Id.T);
   -- A parameter value could not be updated.
   overriding procedure Parameter_Stage_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T);
   -- A parameter value could not be updated.
   overriding procedure Parameter_Update_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T);
   -- A parameter value could not be validated.
   overriding procedure Parameter_Validation_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T);
   -- A parameter value could not be updated.
   overriding procedure Parameter_Fetch_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T);
   -- A parameter was fetched but contained an unexpected length.
   overriding procedure Parameter_Fetch_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameter_Length.T);
   -- Multiple parameters in a grouped entry were fetched and contained different
   -- values. Using the first fetched value.
   overriding procedure Parameter_Fetch_Value_Mismatch (Self : in out Instance; Arg : in Parameter_Entry_Comparison.T);
   -- A parameter table entry command was received to update a parameter but it
   -- contained an unexpected length.
   overriding procedure Parameter_Update_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameter_Table_Entry_Length.T);
   -- A memory region was received with an invalid length. The length of the region
   -- must be the same size as the parameter table.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Length.T);
   -- A memory region parameter table was received with an invalid CRC. The computed
   -- CRC does not match the CRC found in the header.
   overriding procedure Memory_Region_Crc_Invalid (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Crc.T);
   -- Producing a packet with the currently staged parameter values contained within
   -- connected components.
   overriding procedure Dumping_Parameters (Self : in out Instance);
   -- Done dumping the parameters.
   overriding procedure Finished_Dumping_Parameters (Self : in out Instance);
   -- Starting updating of the parameters from a received memory region.
   overriding procedure Starting_Parameter_Table_Update (Self : in out Instance; Arg : in Memory_Region.T);
   -- Done updating the parameters from a received memory region with following
   -- status.
   overriding procedure Finished_Parameter_Table_Update (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- Starting validation of the parameters from a received memory region.
   overriding procedure Starting_Parameter_Table_Validate (Self : in out Instance; Arg : in Memory_Region.T);
   -- Done validating the parameters from a received memory region with following
   -- status.
   overriding procedure Finished_Parameter_Table_Validate (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- Starting updating of the parameters from a received memory region.
   overriding procedure Starting_Parameter_Table_Fetch (Self : in out Instance; Arg : in Memory_Region.T);
   -- Done updating the parameters from a received memory region with following
   -- status.
   overriding procedure Finished_Parameter_Table_Fetch (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A memory region was dropped due to a full queue.
   overriding procedure Memory_Region_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Parameters component
   -- The status of the last parameter table operation including version, update
   -- time, CRC, and operation status.
   overriding procedure Table_Status (Self : in out Instance; Arg : in Packed_Table_Operation_Status.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Parameters Component.
   -- This packet contains a copy of all the active parameters managed by this
   -- component.
   overriding procedure Active_Parameters (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Parameters.Implementation.Tester;
