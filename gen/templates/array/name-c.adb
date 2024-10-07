--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} C/C++ Interface Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

package body {{ name }}.C is

   function To_Ada (Src : in U_C) return U is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.To_Ada (Src (J))];
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end To_Ada;

   function To_C (Src : in U) return U_C is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.To_C (Src (J))];
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end To_C;

{% if endianness in ["either", "big"] %}
   function Pack (Src : in U_C) return T is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Pack (Src (J))];
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U_C) return T_Le is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Pack (Src (J))];
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "big"] %}
   function Unpack (Src : in T) return U_C is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Unpack (Src (J))];
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end Unpack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Unpack (Src : in T_Le) return U_C is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Unpack (Src (J))];
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end Unpack;

{% endif %}

end {{ name }}.C;
