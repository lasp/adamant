-- Standard includes:
with Basic_Types;
with Byte_Array_Util;
with Ccsds_Primary_Header; use Ccsds_Primary_Header;
with Interfaces; use Interfaces;
with Extract_Data_Product; use Extract_Data_Product;
{% for item in includes %}
with {{ item }};
with {{ item }}.Validation;
{% endfor %}

package body {{ name }} is

   -- The implementation for each extract and validate of each extracted data product
{% for apid, products in apids.items() %}
{% for data_product in products %}
   function Extract_And_Validate_{{data_product.name}} (Pkt : in Ccsds_Space_Packet.T; Id_Base : in Data_Product_Types.Data_Product_Id; Timestamp : in Sys_Time.T; Dp : out Data_Product.T; Invalid_Data_Product : out Invalid_Product_Data.T) return Product_Status is
      Local_Id : constant Data_Product_Types.Data_Product_Id := {{ data_product.local_id }};
      Id : constant Data_Product_Types.Data_Product_Id := Id_Base + Local_Id;
      Extraction_Status : Extract_Status;
{% if data_product.time_type == "packet_time" %}
      Ignore : Sys_Time.T renames Timestamp;
{% endif %}
   begin
      pragma Assert (Pkt.Header.Apid = {{apid}});

      -- Initialize out parameters:
      Invalid_Data_Product := (
         Id => Data_Product_Types.Data_Product_Id'First,
         Errant_Field_Number => 0,
         Errant_Field => [others => 0]
      );

      -- Use the generic extraction function using the autocoded values from the YAML file to get the information needed from the packet
{% if data_product.time_type == "packet_time" %}
      Extraction_Status := Extract_Data_Product.Extract_Data_Product (Pkt => Pkt, Offset => {{data_product.offset}}, Length => {{ data_product.product_type }}.Size_In_Bytes, Id => Id, Timestamp => Sys_Time.Serialization.From_Byte_Array (Pkt.Data (Pkt.Data'First .. Pkt.Data'First + Sys_Time.Serialization.Serialized_Length - 1)), Dp => Dp);
{% else %}
      Extraction_Status := Extract_Data_Product.Extract_Data_Product (Pkt => Pkt, Offset => {{data_product.offset}}, Length => {{ data_product.product_type }}.Size_In_Bytes, Id => Id, Timestamp => Timestamp, Dp => Dp);
{% endif %}

      -- Make sure the extraction was successful, at this point the only failure should be the length
      case Extraction_Status is
         when Length_Overflow => return Length_Error;
         when Success => null;
      end case;

      -- Now make sure the data product is valid for the type that we are extracting using the validation from the packed type generation
      declare
         Ef : Unsigned_32;
{% if "T_Le" in data_product.product_endian %}
         pragma Warnings (Off, "overlay changes scalar storage order");
         Overlay : {{ data_product.product_type }}.{{ data_product.product_endian }} with Import, Convention => Ada, Address => Dp.Buffer'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
{% else %}
         Overlay : {{ data_product.product_type }}.{{ data_product.product_endian }} with Import, Convention => Ada, Address => Dp.Buffer'Address;
{% endif %}
         Validation : constant Boolean := {{ data_product.product_type }}.Validation.Valid (R => Overlay, Errant_Field => Ef);
      begin
         case Validation is
            when True =>
               return Success;
            when False =>
               -- When there is a validation error, fill in a data structure with the relevant information for the component to use to send an event.
               declare
                  P_Type : Basic_Types.Poly_Type := [others => 0];
               begin
                  -- Copy extracted value into poly type
                  Byte_Array_Util.Safe_Right_Copy (P_Type, Pkt.Data ({{data_product.offset}} .. {{data_product.offset}} + {{ data_product.product_type }}.Size_In_Bytes - 1));
                  Invalid_Data_Product.Id := Id;
                  Invalid_Data_Product.Errant_Field_Number := Ef;
                  Invalid_Data_Product.Errant_Field := P_Type;
               end;
               return Invalid_Data;
         end case;
      end;
   end Extract_And_Validate_{{data_product.name}};

{% endfor %}
{% endfor %}
end {{ name }};
