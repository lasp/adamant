--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if not is_volatile_type %}
-- Standard includes:
with String_Util; use String_Util;
{% if modeled_type_includes %}

-- Record Field Includes:
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
   -- Return string showing bytes in record:
   function To_Byte_String is new String_Util.To_Byte_String (T);
   function To_Byte_String is new String_Util.To_Byte_String (T_Le);
   function To_Byte_String is new String_Util.To_Byte_String (U);

   -- Display record as string:
   function Image (R : in T) return String;
   function Image (R : in T_Le) return String;
   function Image (R : in U) return String;

   -------------------------------------------------
   -- Less commonly used to string functions:
   -------------------------------------------------
   -- Return string representation of record fields in form (field 1, field 2, etc.)
   function To_Tuple_String (R : in T) return String;
   function To_Tuple_String (R : in T_Le) return String;
   function To_Tuple_String (R : in U) return String;

   -- Return string representation of record fields and bytes
   function Image_With_Prefix (R : in T; Prefix : in String := "") return String;
   function Image_With_Prefix (R : in T_Le; Prefix : in String := "") return String;
   function Image_With_Prefix (R : in U; Prefix : in String := "") return String;

   -------------------------------------------------
   -- To string functions for record field type:
   -------------------------------------------------
{% for field in fields.values() %}
{% if field.is_packed_type %}
   function {{ field.name }}_To_Byte_String (R : in {{ field.type }}) return String renames {{ field.type_package }}.Representation.To_Byte_String;
   function {{ field.name }}_Image (R : in {{ field.type }}) return String renames {{ field.type_package }}.Representation.Image;
   function {{ field.name }}_To_Tuple_String (R : in {{ field.type }}) return String renames {{ field.type_package }}.Representation.To_Tuple_String;
   function {{ field.name }}_Image_With_Prefix (R : in {{ field.type }}; Prefix : in String := "") return String renames {{ field.type_package }}.Representation.Image_With_Prefix;
{% elif field.is_enum %}
   function {{ field.name }}_Image (R : in {{ field.type }}) return String renames {{ field.type_package }}.Representation.{{ field.type_model.name }}_Image;
   function {{ field.name }}_To_Tuple_String (R : in {{ field.type }}) return String renames {{ field.type_package }}.Representation.{{ field.type_model.name }}_To_Tuple_String;
   function {{ field.name }}_Image_With_Prefix (R : in {{ field.type }}; Prefix : in String := "") return String renames {{ field.type_package }}.Representation.{{ field.type_model.name }}_Image_With_Prefix;
{% else %}
   function {{ field.name }}_To_Byte_String is new String_Util.To_Byte_String ({{ field.type }});
{% if field.byte_image or (field.format and field.format.unit_size > 64) or (field.format and field.format.length) %}
   function {{ field.name }}_Image (R : in {{ field.type }}) return String renames {{ field.name }}_To_Byte_String;
   function {{ field.name }}_To_Tuple_String (R : in {{ field.type }}) return String renames {{ field.name }}_To_Byte_String;
   function {{ field.name }}_Image_With_Prefix (R : in {{ field.type }}) return String renames {{ field.name }}_To_Byte_String;
{% else %}
   function {{ field.name }}_Image (R : in {{ field.type }}) return String renames {{ field.type }}'Image;
   function {{ field.name }}_To_Tuple_String (R : in {{ field.type }}) return String renames {{ field.type }}'Image;
   function {{ field.name }}_Image_With_Prefix (R : in {{ field.type }}) return String renames {{ field.type }}'Image;
{% endif %}
{% endif %}

{% endfor %}
{% endif %}
end {{ name }}.Representation;
