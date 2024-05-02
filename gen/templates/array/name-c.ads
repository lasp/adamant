--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} C/C++ Interface Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Ada.Unchecked_Conversion;
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
   subtype U_C is Unconstrained{% if length %} (Constrained_Index_Type){% endif %};

   -- Access type for U
   type U_C_Access is access all U_C;

   -- Functions for converting between the Ada and C version of the packed type:
   function To_Ada is new Ada.Unchecked_Conversion (Source => U_C, Target => U);
   function To_C is new Ada.Unchecked_Conversion (Source => U, Target => U_C);

   -- The .C package is not supported for all Adamant packed records. We do not allow compilation in
   -- these cases.
   pragma Compile_Time_Error ({{ name }}.U'Size /= U_C'Size, "C type size not compatible with Ada type size.");

end {{ name }}.C;
