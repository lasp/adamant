
package body Extract_Data_Product is

   -- General function to extract a data product from a ccsds space packet given the location in the packet of the extracted product
   function Extract_Data_Product (Pkt : in Ccsds_Space_Packet.T; Offset : in Natural; Length : in Natural; Id : in Data_Product_Types.Data_Product_Id; Timestamp : in Sys_Time.T; Dp : out Data_Product.T) return Extract_Status is
   begin
      -- Initialize out parameters:
      Dp := (
         Header => (
            Time => Timestamp,
            Id => Id,
            Buffer_Length => Length
         ),
         Buffer => [others => 0]
      );

      -- Dont try to read if its going to overflow out of the packet.
      if Offset + Length - 1 <= Natural (Pkt.Header.Packet_Length) then
         Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Length - 1) := Pkt.Data (Offset .. Offset + Length - 1);
         return Success;
      else
         return Length_Overflow;
      end if;
   end Extract_Data_Product;

end Extract_Data_Product;
