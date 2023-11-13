--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Data_Product;
with Data_Product_Types;
with Sys_Time;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }}.Representation is

   -----------------------------------------------
   -- Data product to string functions:
   -----------------------------------------------
{% for dp in data_products %}
   function {{ dp.name }}_Image (Timestamp : in Sys_Time.T; Id : in Data_Product_Types.Data_Product_Id; Item : in {{ dp.type }}; Instance_Name : in String := "") return String;
   function {{ dp.name }}_Image (Dp : in Data_Product.T; Instance_Name : in String := "") return String;

{% endfor %}
end {{ name }}.Representation;
