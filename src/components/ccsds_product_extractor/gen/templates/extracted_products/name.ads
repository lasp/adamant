-- Standard includes:
with Product_Extractor_Types; use Product_Extractor_Types;
with Data_Product;
with Data_Product_Types; use Data_Product_Types;
with Ccsds_Space_Packet;
with Invalid_Product_Data;
with Sys_Time;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is
{% for apid, products in apids.items() %}
{% for data_product in products %}

{% if data_product.description %}
{{ printMultiLine(data_product.description, '   -- ') }}
{% endif %}
   function Extract_And_Validate_{{data_product.name}} (Pkt : in Ccsds_Space_Packet.T; Id_Base : in Data_Product_Types.Data_Product_Id; Timestamp : in Sys_Time.T; Dp : out Data_Product.T; Invalid_Data_Product : out Invalid_Product_Data.T) return Product_Status;
{% endfor %}
{% endfor %}
{% for apid, products in apids.items() %}
   Extract_Products_{{apid}} : aliased Extractor_List := [
{% for data_product in products %}
      {{ loop.index0 }} => Extract_And_Validate_{{data_product.name}}'Access{{ "," if not loop.last }}
{% endfor %}
   ];
{% endfor %}

   -- Initial product extraction list containing the information to extract all the products requested by each apid
   Data_Product_Extraction_List : aliased Extracted_Product_List := [
{% for apid, products in apids.items() %}
      {{ loop.index0 }} => (
         Apid => {{ apid }},
         Extract_List => Extract_Products_{{apid}}'Access
      ){{ "," if not loop.last }}
{% endfor %}
   ];
   Data_Product_Extraction_List_Access : constant Extracted_Product_List_Access := Data_Product_Extraction_List'Access;

end {{ name }};
