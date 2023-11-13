--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- String representation package for {{ name }}
package {{ name }}.Representation is

{% for enum in enums.values() %}
   -- {{ enum.name }} representation functions:
   function {{ enum.name }}_Image (Enum : in {{ name }}.{{ enum.name }}.E) return String;
   function {{ enum.name }}_To_Tuple_String (Enum : in {{ name }}.{{ enum.name }}.E) return String renames {{ name }}.{{ enum.name }}.E'Image;
   function {{ enum.name }}_Image_With_Prefix (Enum : in {{ name }}.{{ enum.name }}.E; Prefix : String := "") return String;

{% endfor %}
end {{ name }}.Representation;
