--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

package body {{ name }} is

{% if endianness in ["either", "big"] %}
   function Pack (Src : in U) return T is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.Pack (Src (J))];
{% else %}
      return T (Src);
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U) return T_Le is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.Pack (Src (J))];
{% else %}
      return T_Le (Src);
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "big"] %}
   function Unpack (Src : in T) return U is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.Unpack (Src (J))];
{% else %}
      return U (Src);
{% endif %}
   end Unpack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Unpack (Src : in T_Le) return U is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.Unpack (Src (J))];
{% else %}
      return U (Src);
{% endif %}
   end Unpack;

{% endif %}
{% if endianness in ["either"] %}
   function Swap_Endianness (Src : in T) return T_Le is
      Unpacked : constant U := Unpack (Src);
   begin
      return Pack (Unpacked);
   end Swap_Endianness;

   function Swap_Endianness (Src : in T_Le) return T is
      Unpacked : constant U := Unpack (Src);
   begin
      return Pack (Unpacked);
   end Swap_Endianness;

{% endif %}
end {{ name }};
