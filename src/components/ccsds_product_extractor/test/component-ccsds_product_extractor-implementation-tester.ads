--------------------------------------------------------------------------------
-- Ccsds_Product_Extractor Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Product_Extractor_Reciprocal;
with Sys_Time;
with Printable_History;
with Event.Representation;
with Sys_Time.Representation;
with Data_Product.Representation;
with Event;
with Invalid_Product_Data.Representation;
with Invalid_Product_Length.Representation;
with Data_Product;
with Packed_Byte.Representation;

-- The product extractor is a component that extracts data from an incoming packet which it then creates into a data product and sends as its own data product for other component use. This is performed by using a list of types that include the offset and the corresponding APID to know which packets to extract from. The data is verified against the respective type at which point it will either send it on or create an event indicating there was an error. All of this information is derived from a user defined YAML input model that contains the information for each data product and the residing packet. See the generator documentation for more information.
package Component.Ccsds_Product_Extractor.Implementation.Tester is

   use Component.Ccsds_Product_Extractor_Reciprocal;
   -- Invoker connector history packages:
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);

   -- Event history packages:
   package Invalid_Extracted_Product_Data_History_Package is new Printable_History (Invalid_Product_Data.T, Invalid_Product_Data.Representation.Image);
   package Invalid_Extracted_Product_Length_History_Package is new Printable_History (Invalid_Product_Length.T, Invalid_Product_Length.Representation.Image);

   -- Data product history packages:
   package Dummy_History_Package is new Printable_History (Packed_Byte.T, Packed_Byte.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Ccsds_Product_Extractor_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Product_Extractor.Implementation.Instance;
      -- Connector histories:
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Invalid_Extracted_Product_Data_History : Invalid_Extracted_Product_Data_History_Package.Instance;
      Invalid_Extracted_Product_Length_History : Invalid_Extracted_Product_Length_History_Package.Instance;
      -- Data product histories:
      Dummy_History : Dummy_History_Package.Instance;
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
   -- The Event connector for sending events
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- The connector for data products
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Event that is issued when the defined extracted product does not match the data that was read.
   overriding procedure Invalid_Extracted_Product_Data (Self : in out Instance; Arg : in Invalid_Product_Data.T);
   -- The length and offset of the extracted product exceeded the length of the incoming packet.
   overriding procedure Invalid_Extracted_Product_Length (Self : in out Instance; Arg : in Invalid_Product_Length.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Ccsds Product Extractor component.
   -- A dummy data product since this component doesnt have its own data products, this provides a base to start from. This will be removed and replaced with the extracted products that the user defines in the extracted_products YAML file.
   overriding procedure Dummy (Self : in out Instance; Arg : in Packed_Byte.T);

   -----------------------------------------------
   -- Custom functions
   -----------------------------------------------

   -- Override the reciprocal component base Dispatch_Data_Product procedure with our own. Since
   -- the data products are generated by the model, we need to allow data products to be produced that would
   -- otherwise cause the unit test to crash due to an out of range ID.
   overriding procedure Dispatch_Data_Product (Self : in out Instance; Dp : in Data_Product.T);

end Component.Ccsds_Product_Extractor.Implementation.Tester;
