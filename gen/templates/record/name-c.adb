--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} C/C++ Interface Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

package body {{ name }}.C is

   function To_Ada (Src : in U_C) return U is
   begin
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.C.To_Ada (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
   end To_Ada;

   function To_C (Src : in U) return U_C is
   begin
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.C.To_C (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
   end To_C;

{% if endianness in ["either", "big"] %}
   function Pack (Src : in U_C) return T is
   begin
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.C.Pack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
   end Pack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U_C) return T_Le is
   begin
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.C.Pack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
   end Pack;

{% endif %}
{% if endianness in ["either", "big"] %}
   function Unpack (Src : in T) return U_C is
   begin
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.C.Unpack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
   end Unpack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Unpack (Src : in T_Le) return U_C is
   begin
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.C.Unpack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
   end Unpack;
{% endif %}

end {{ name }}.C;
