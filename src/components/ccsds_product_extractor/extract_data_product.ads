
-- Standard includes:
with Ccsds_Space_Packet;
with Sys_Time;
with Data_Product_Types;
with Data_Product;

package Extract_Data_Product is

   type Extract_Status is (Success, Length_Overflow);

   -- General function to extract a data product from a ccsds space packet given the location in the packet of the extracted product
   function Extract_Data_Product (Pkt : in Ccsds_Space_Packet.T; Offset : in Natural; Length : in Natural; Id : in Data_Product_Types.Data_Product_Id; Timestamp : in Sys_Time.T; Dp : out Data_Product.T) return Extract_Status;

end Extract_Data_Product;
