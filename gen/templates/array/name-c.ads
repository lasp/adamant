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
{% if element.is_packed_type %}
   type Unconstrained_C is array (Unconstrained_Index_Type range <>) of aliased {{ element.type_package }}.C.U_C
      with Convention => C;
{% else %}
   type Unconstrained_C is array (Unconstrained_Index_Type range <>) of aliased {{ element.type }}
      with Convention => C;
{% endif %}

   -- Unpacked array type:
   subtype U_C is Unconstrained_C{% if length %} (Constrained_Index_Type){% endif %};

   -- Access type for U
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
