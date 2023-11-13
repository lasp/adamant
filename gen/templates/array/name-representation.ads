--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if not is_volatile_type %}
-- Standard includes:
with String_Util;
{% if modeled_type_includes %}

-- Array element Includes:
{% for include in modeled_type_includes %}
with {{ include }}.Representation;
{% endfor %}
{% endif %}
{% endif %}

-- String representation package for {{ name }}
package {{ name }}.Representation is

{% if is_volatile_type %}
   -- Representation not supported for volatile record. Convert to a regular record for
   -- a validation checking function.
   procedure Dummy_Image;
{% else %}
   -------------------------------------------------
   -- Common to string functions:
   -------------------------------------------------
   -- Return string showing bytes in array:
   function To_Byte_String is new String_Util.To_Byte_String (T);
   function To_Byte_String is new String_Util.To_Byte_String (T_Le);
   function To_Byte_String is new String_Util.To_Byte_String (U);

   -- Display array as string:
   function Image (R : in T) return String;
   function Image (R : in T_Le) return String;
   function Image (R : in U) return String;

   -------------------------------------------------
   -- Less commonly used to string functions:
   -------------------------------------------------
   -- Return string representation of array components in form (element 1, element 2, etc.)
   function To_Tuple_String (R : in T) return String;
   function To_Tuple_String (R : in T_Le) return String;
   function To_Tuple_String (R : in U) return String;

   -- Return string representation of array elements and bytes
   function Image_With_Prefix (R : in T; Prefix : in String) return String;
   function Image_With_Prefix (R : in T_Le; Prefix : in String) return String;
   function Image_With_Prefix (R : in U; Prefix : in String) return String;

   -------------------------------------------------
   -- To string functions for array element type:
   -------------------------------------------------
{% if element.is_packed_type %}
   function Element_To_Byte_String (R : in {{ element.type }}) return String renames {{ element.type_package }}.Representation.To_Byte_String;
   function Element_Image (R : in {{ element.type }}) return String renames {{ element.type_package }}.Representation.Image;
   function Element_To_Tuple_String (R : in {{ element.type }}) return String renames {{ element.type_package }}.Representation.To_Tuple_String;
   function Element_Image_With_Prefix (R : in {{ element.type }}; Prefix : in String := "") return String renames {{ element.type_package }}.Representation.Image_With_Prefix;
{% elif element.is_enum %}
   function Element_Image (R : in {{ element.type }}) return String renames {{ element.type_package }}.Representation.{{ element.type_model.name }}_Image;
   function Element_To_Tuple_String (R : in {{ element.type }}) return String renames {{ element.type_package }}.Representation.{{ element.type_model.name }}_To_Tuple_String;
   function Element_Image_With_Prefix (R : in {{ element.type }}; Prefix : in String := "") return String renames {{ element.type_package }}.Representation.{{ element.type_model.name }}_Image_With_Prefix;
{% else %}
   function Element_To_Byte_String is new String_Util.To_Byte_String ({{ element.type }});
{% if element.byte_image or (element.format and element.format.unit_size > 64) or (element.format and element.format.length) %}
   function Element_Image (R : in {{ element.type }}) return String renames Element_To_Byte_String;
   function Element_To_Tuple_String (R : in {{ element.type }}) return String renames Element_To_Byte_String;
   function Element_Image_With_Prefix (R : in {{ element.type }}) return String renames Element_To_Byte_String;
{% else %}
   function Element_Image (R : in {{ element.type }}) return String renames {{ element.type }}'Image;
   function Element_To_Tuple_String (R : in {{ element.type }}) return String renames {{ element.type }}'Image;
   function Element_Image_With_Prefix (R : in {{ element.type }}) return String renames {{ element.type }}'Image;
{% endif %}
{% endif %}
{% endif %}

end {{ name }}.Representation;
