--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} C/C++ Interface Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if type_includes %}

-- Record Component Includes:
{% for include in packed_type_includes %}
with {{ include }}.C;
{% endfor %}
{% endif %}

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }}.C is

   -- Unpacked C/C++ compatible type:
   type U_C is record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
{% if field.is_packed_type %}
      {{ field.name }} : aliased {{ field.type_package }}.C.U_C{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% else %}
      {{ field.name }} : aliased {{ field.type }}{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% endif %}
{% endfor %}
   end record
      with Convention => C_Pass_By_Copy;

   -- Access type for U_C.
   type U_C_Access is access all U_C;

   -- Functions for converting between the Ada and C version of the
   -- unpacked and packed types:
   function To_Ada (Src : in U_C) return U;
   function To_C (Src : in U) return U_C;
{% if endianness in ["either", "big"] %}
   function Pack (Src : in U_C) return T;
   function Unpack (Src : in T) return U_C;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U_C) return T_Le;
   function Unpack (Src : in T_Le) return U_C;
{% endif %}

end {{ name }}.C;
