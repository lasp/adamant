--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

with String_Util; use String_Util;

package body {{ name }}.Representation is

{% for enum in enums.values() %}
   -- {{ enum.name }} representation functions:
   function {{ enum.name }}_Image (Enum : in {{ name }}.{{ enum.name }}.E) return String is
   begin
      return {{ enum.name }}_Image_With_Prefix (Enum, "");
   end {{ enum.name }}_Image;

   function {{ enum.name }}_Image_With_Prefix (Enum : in {{ name }}.{{ enum.name }}.E; Prefix : String := "") return String is
   begin
      return Prefix & Trim_Both ({{ name }}.{{ enum.name }}.E'Image (Enum))   & " (" & Trim_Both (Integer'Image ({{ name }}.{{ enum.name }}.E'Enum_Rep (Enum))) & ")";
   end {{ enum.name }}_Image_With_Prefix;

{% endfor %}
end {{ name }}.Representation;
