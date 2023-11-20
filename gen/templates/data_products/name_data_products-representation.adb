--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
{% if basic_types %}
with Serializer;
{% endif %}
with String_Util; use String_Util;
with Sys_Time.Pretty;

{% if complex_type_includes %}
-- Data Product Type Includes
{% for include in complex_type_includes %}
with {{ include }}.Representation;
{% endfor %}

{% endif %}
package body {{ name }}.Representation is

{% if preamble %}
   -- Preamble code:
   {{ comp.preamble }}

{% endif %}
   -----------------------------------------------
   -- Data product to string functions:
   -----------------------------------------------
{% for dp in data_products %}
   function {{ dp.name }}_Image (Timestamp : in Sys_Time.T; Id : in Data_Product_Types.Data_Product_Id; Item : in {{ dp.type }}; Instance_Name : in String := "") return String is
   begin
      return Sys_Time.Pretty.Image (Timestamp, 10) & " - " & Instance_Name & ".{{ dp.name }} " & "(0x" & Natural_2_Hex_String (Natural (Id), 8) & ") : " & Trim_Both ({% if dp.type_package %}{{ dp.type_package }}.Representation.To_Tuple_String (Item){% else %}{{ dp.type }}'Image (Item){% endif %});
   end {{ dp.name }}_Image;

   function {{ dp.name }}_Image (Dp : in Data_Product.T; Instance_Name : in String := "") return String is
{% if dp.type_model %}
      package Data_Product_Deserializer renames {{ dp.type_package }}.Serialization;
{% else %}
      package Data_Product_Deserializer is new Serializer ({{ dp.type }});
{% endif %}
   begin
      return {{ dp.name }}_Image (Dp.Header.Time, Dp.Header.Id, Data_Product_Deserializer.From_Byte_Array (Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Data_Product_Deserializer.Serialized_Length - 1)), Instance_Name);
   end {{ dp.name }}_Image;

{% endfor %}
end {{ name }}.Representation;
