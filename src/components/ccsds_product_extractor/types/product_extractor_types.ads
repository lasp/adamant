with Ccsds_Primary_Header;
with Ccsds_Space_Packet;
with Data_Product;
with Data_Product_Types;
with Sys_Time;
with Invalid_Product_Data;

package Product_Extractor_Types is

   type Product_Status is (Success, Invalid_Data, Length_Error);

   -- Extraction function list for each product to extract in a single packet
   type Extract_And_Validate is not null access function (Pkt : in Ccsds_Space_Packet.T; Id_Base : in Data_Product_Types.Data_Product_Id; Timestamp : in Sys_Time.T; Dp : out Data_Product.T; Invalid_Data_Product : out Invalid_Product_Data.T) return Product_Status;
   type Extractor_List is array (Natural range <>) of Extract_And_Validate;
   type Extractor_List_Access is access all Extractor_List;

   -- List of ids that associate to each data product. This has to be filled in at init
   type Product_Id_List is array (Natural range <>) of Data_Product_Types.Data_Product_Id;
   type Product_Id_List_Access is access all Product_Id_List;

   -- Product list for each apid
   type Ccsds_Product_Apid_List is record
      -- Apid of the packet that the data product(s) resides in
      Apid : Ccsds_Primary_Header.Ccsds_Apid_Type := Ccsds_Primary_Header.Ccsds_Apid_Type'First;

      -- List of all data products associated with a single apid
      Extract_List : Extractor_List_Access := null;
   end record;

   type Extracted_Product_List is array (Natural range <>) of Ccsds_Product_Apid_List;
   type Extracted_Product_List_Access is access all Extracted_Product_List;

end Product_Extractor_Types;
