--------------------------------------------------------------------------------
-- Product_Database Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Product_Database_Reciprocal;
with Sys_Time;
with Printable_History;
with Event.Representation;
with Command_Response.Representation;
with Data_Product.Representation;
with Packet.Representation;
with Sys_Time.Representation;
with Data_Product;
with Data_Product_Poly_Type.Representation;
with Packed_Enable_Disable_Type.Representation;
with Event;
with Data_Product_Id.Representation;
with Data_Product_Header.Representation;
with Data_Product_Poly_Extract.Representation;
with Data_Product_Poly_Event.Representation;
with Invalid_Command_Info.Representation;

-- The product database component maintains a database of data product items. Only the latest single copy of each data product item is stored, and that value can be updated or fetched by ID via connectors. The component is configured by passing the minimum and maximum data product ID that the database can accept. The component allocates memory on the heap to store a maximum sized data product for every ID in range from the minimum to maximum ID provided. Invalid IDs received during requests are reported as events. The lookup algorithm is extremely fast, using the data product ID itself as a direct index into the database.
--
-- Note that IDs stored in this database should come from a compact ID space for most efficient memory usage. If you are manually setting the data product ID bases in your assembly model and creating a sparse ID set then this database component should not be used, as it could waste an enormous amount of memory. This component is designed to work best with the default, Adamant-allocated ID space for data products which spans from 1 to number of data products used in the system.
package Component.Product_Database.Implementation.Tester is

   use Component.Product_Database_Reciprocal;
   -- Invoker connector history packages:
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Data_Product_Update_Id_Out_Of_Range_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Fetch_Id_Out_Of_Range_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Fetch_Id_Not_Available_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Override_Cleared_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Override_Cleared_For_All_History_Package is new Printable_History (Natural, Natural'Image);
   package Data_Product_Overridden_History_Package is new Printable_History (Data_Product_Header.T, Data_Product_Header.Representation.Image);
   package Data_Product_Override_Serialization_Failure_History_Package is new Printable_History (Data_Product_Header.T, Data_Product_Header.Representation.Image);
   package Data_Product_Override_Id_Out_Of_Range_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Clear_Override_Id_Out_Of_Range_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Dump_Id_Not_Available_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Dump_Id_Out_Of_Range_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Dumped_History_Package is new Printable_History (Data_Product_Header.T, Data_Product_Header.Representation.Image);
   package Dumping_Data_Product_Poly_Type_History_Package is new Printable_History (Data_Product_Poly_Extract.T, Data_Product_Poly_Extract.Representation.Image);
   package Dumped_Data_Product_Poly_Type_History_Package is new Printable_History (Data_Product_Poly_Event.T, Data_Product_Poly_Event.Representation.Image);
   package Data_Product_Dump_Poly_Id_Not_Available_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Dump_Poly_Id_Out_Of_Range_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Poly_Type_Extraction_Failed_History_Package is new Printable_History (Data_Product_Header.T, Data_Product_Header.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Data_Product_Poly_Type_Dump_History_Package is new Printable_History (Data_Product_Poly_Type.T, Data_Product_Poly_Type.Representation.Image);
   package Database_Override_History_Package is new Printable_History (Packed_Enable_Disable_Type.T, Packed_Enable_Disable_Type.Representation.Image);

   -- Packet history packages:
   package Dump_Packet_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Product_Database_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Product_Database.Implementation.Instance;
      -- Connector histories:
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Data_Product_Update_Id_Out_Of_Range_History : Data_Product_Update_Id_Out_Of_Range_History_Package.Instance;
      Data_Product_Fetch_Id_Out_Of_Range_History : Data_Product_Fetch_Id_Out_Of_Range_History_Package.Instance;
      Data_Product_Fetch_Id_Not_Available_History : Data_Product_Fetch_Id_Not_Available_History_Package.Instance;
      Override_Cleared_History : Override_Cleared_History_Package.Instance;
      Override_Cleared_For_All_History : Override_Cleared_For_All_History_Package.Instance;
      Data_Product_Overridden_History : Data_Product_Overridden_History_Package.Instance;
      Data_Product_Override_Serialization_Failure_History : Data_Product_Override_Serialization_Failure_History_Package.Instance;
      Data_Product_Override_Id_Out_Of_Range_History : Data_Product_Override_Id_Out_Of_Range_History_Package.Instance;
      Data_Product_Clear_Override_Id_Out_Of_Range_History : Data_Product_Clear_Override_Id_Out_Of_Range_History_Package.Instance;
      Data_Product_Dump_Id_Not_Available_History : Data_Product_Dump_Id_Not_Available_History_Package.Instance;
      Data_Product_Dump_Id_Out_Of_Range_History : Data_Product_Dump_Id_Out_Of_Range_History_Package.Instance;
      Data_Product_Dumped_History : Data_Product_Dumped_History_Package.Instance;
      Dumping_Data_Product_Poly_Type_History : Dumping_Data_Product_Poly_Type_History_Package.Instance;
      Dumped_Data_Product_Poly_Type_History : Dumped_Data_Product_Poly_Type_History_Package.Instance;
      Data_Product_Dump_Poly_Id_Not_Available_History : Data_Product_Dump_Poly_Id_Not_Available_History_Package.Instance;
      Data_Product_Dump_Poly_Id_Out_Of_Range_History : Data_Product_Dump_Poly_Id_Out_Of_Range_History_Package.Instance;
      Data_Product_Poly_Type_Extraction_Failed_History : Data_Product_Poly_Type_Extraction_Failed_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Data_Product_Poly_Type_Dump_History : Data_Product_Poly_Type_Dump_History_Package.Instance;
      Database_Override_History : Database_Override_History_Package.Instance;
      -- Packet histories:
      Dump_Packet_History : Dump_Packet_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- This connector is used to register and respond to the component's commands. This does not need to be connected if the command for this component will not be used.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Send a packet of data - used to dump database items.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A data product update was received with an ID that was out of range.
   overriding procedure Data_Product_Update_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product fetch was received with an ID that was out of range.
   overriding procedure Data_Product_Fetch_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product fetch was received with an ID that has not yet been stored in the database.
   overriding procedure Data_Product_Fetch_Id_Not_Available (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- Override condition cleared for the data product of the provided ID.
   overriding procedure Override_Cleared (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- Override condition cleared for all data products.
   overriding procedure Override_Cleared_For_All (Self : in out Instance);
   -- Data product overridden by command.
   overriding procedure Data_Product_Overridden (Self : in out Instance; Arg : in Data_Product_Header.T);
   -- Data product override could not be completed due to a serialization error.
   overriding procedure Data_Product_Override_Serialization_Failure (Self : in out Instance; Arg : in Data_Product_Header.T);
   -- A data product override command was received with an ID that was out of range.
   overriding procedure Data_Product_Override_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product clear override command was received with an ID that was out of range.
   overriding procedure Data_Product_Clear_Override_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product dump command was received with an ID that has not yet been stored in the database.
   overriding procedure Data_Product_Dump_Id_Not_Available (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product dump command was received with an ID that was out of range.
   overriding procedure Data_Product_Dump_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- Data product dumped into a packet by command.
   overriding procedure Data_Product_Dumped (Self : in out Instance; Arg : in Data_Product_Header.T);
   -- Data product poly type dumped into a packet by command.
   overriding procedure Dumping_Data_Product_Poly_Type (Self : in out Instance; Arg : in Data_Product_Poly_Extract.T);
   -- Data product poly type dumped into a packet by command.
   overriding procedure Dumped_Data_Product_Poly_Type (Self : in out Instance; Arg : in Data_Product_Poly_Event.T);
   -- A data product dump poly command was received with an ID that has not yet been stored in the database.
   overriding procedure Data_Product_Dump_Poly_Id_Not_Available (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product dump poly command was received with an ID that was out of range.
   overriding procedure Data_Product_Dump_Poly_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product dump poly command failed because the extraction could not succeed with the provided parameters.
   overriding procedure Data_Product_Poly_Type_Extraction_Failed (Self : in out Instance; Arg : in Data_Product_Header.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Product Database component.
   -- Data product poly type dumped into a data product by command.
   overriding procedure Data_Product_Poly_Type_Dump (Self : in out Instance; Arg : in Data_Product_Poly_Type.T);
   -- If set to Enabled then the database contains at least one data product that has been overridden by command.
   overriding procedure Database_Override (Self : in out Instance; Arg : in Packed_Enable_Disable_Type.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Product Database.
   -- This packet contains dumped data products.
   overriding procedure Dump_Packet (Self : in out Instance; Arg : in Data_Product.T);

end Component.Product_Database.Implementation.Tester;
