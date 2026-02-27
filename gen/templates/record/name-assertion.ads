--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if not is_volatile_type %}

-- Standard includes:
with Smart_Assert;
with {{ name }}.Representation;
{% if variable_length or has_float %}
with GNAT.Source_Info;
{% endif %}
{% if modeled_type_includes %}

-- Record Field Includes:
{% for include in modeled_type_includes %}
with {{ include }}.Assertion;
{% endfor %}
{% endif %}
{% endif %}

package {{ name }}.Assertion is

{% if variable_length or has_float %}
   package Sinfo renames GNAT.Source_Info;

   -- Special assertion package for the variable length type. This package
   -- does assertions, only comparing used data, ignoring data that is not
   -- being used. For example, a type with a buffer that is only half filled
   -- with valid data, as prescribed by that types variable length field, would
   -- only be compared up to that length. Unused data in the buffer is ignored.
   package {{ name }}_U_Assert is
      procedure Eq (T1 : in U; T2 : in U;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in U; T2 : in U;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_U_Assert;

{% if endianness in ["either", "big"] %}
   package {{ name }}_Assert is
      procedure Eq (T1 : in T; T2 : in T;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T; T2 : in T;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_Assert;

{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_Le_Assert is
      procedure Eq (T1 : in T_Le; T2 : in T_Le;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T_Le; T2 : in T_Le;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_Le_Assert;

{% endif %}
{% if variable_length %}
   -- This package compares all data in the variable length type, even data
   -- that is "out of bounds", i.e. past the variable type's length
{% else %}
   -- This package compares data without any Epsilon, using the
   -- Smart_Assert.Basic package.
{% endif %}
   package {{ name }}_U_Assert_All is new Smart_Assert.Basic ({{ name }}.U, {{ name }}.Representation.Image);
{% if endianness in ["either", "big"] %}
   package {{ name }}_Assert_All is new Smart_Assert.Basic ({{ name }}.T, {{ name }}.Representation.Image);
{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_Le_Assert_All is new Smart_Assert.Basic ({{ name }}.T_Le, {{ name }}.Representation.Image);
{% endif %}
{% else %}
   -- Basic assertion package for the packed type:
   package {{ name }}_U_Assert is new Smart_Assert.Basic ({{ name }}.U, {{ name }}.Representation.Image);
{% if endianness in ["either", "big"] %}
   package {{ name }}_Assert is new Smart_Assert.Basic ({{ name }}.T, {{ name }}.Representation.Image);
{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_Le_Assert is new Smart_Assert.Basic ({{ name }}.T_Le, {{ name }}.Representation.Image);
{% endif %}

   -- We need this to force an adb to get built and compile without error.
   package Dummy is
      procedure Dumb;
   end Dummy;
{% endif %}

   -- Specialized smart assert package for the fields in this record:
{% for field in fields.values() %}
{% if field.is_packed_type %}
   package {{ field.name }}_Assertion renames {{ field.type_package }}.Assertion;
{% if endianness in ["either", "big"] %}
   package {{ field.name }}_Assert renames {{ field.name }}_Assertion.{{ field.type_package }}_Assert;
{% else %}
   package {{ field.name }}_Assert renames {{ field.name }}_Assertion.{{ field.type_package }}_Le_Assert;
{% endif %}
{% elif field.is_enum %}
   package {{ field.name }}_Assertion renames {{ field.type_package }}.Assertion;
   package {{ field.name }}_Assert renames {{ field.name }}_Assertion.{{ field.type_model.name }}_Assert;
{% elif field.has_float %}
   package {{ field.name }}_Assert is new Smart_Assert.Float ({{ field.type }}, {{ name }}.Representation.{{ field.name }}_Image);
{% else %}
   package {{ field.name }}_Assert is new Smart_Assert.Basic ({{ field.type }}, {{ name }}.Representation.{{ field.name }}_Image);
{% endif %}
{% endfor %}

end {{ name }}.Assertion;
