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
      pragma Annotate (GNATSAS, False_Positive, "precondition",
         "The element conversion preconditions require each source component to be initialized. Callers initialize the source before this call, typically on the other side of the foreign-function boundary, which the analyzer cannot trace.");
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end To_Ada;

   function To_C (Src : in U) return U_C is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.To_C (Src (J))];
      pragma Annotate (GNATSAS, False_Positive, "precondition",
         "The element conversion preconditions require each source component to be initialized. Callers initialize the source before this call, typically on the other side of the foreign-function boundary, which the analyzer cannot trace.");
{% else %}
      return [for J in Src'Range => Src (J)];
{% endif %}
   end To_C;

{% if endianness in ["either", "big"] %}
   function Pack (Src : in U_C) return T is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Pack (Src (J))];
      pragma Annotate (GNATSAS, False_Positive, "precondition",
         "The element conversion preconditions require each source component to be initialized. Callers initialize the source before this call, typically on the other side of the foreign-function boundary, which the analyzer cannot trace.");
{% else %}
      return {{ name }}.Pack (To_Ada (Src));
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U_C) return T_Le is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Pack (Src (J))];
      pragma Annotate (GNATSAS, False_Positive, "precondition",
         "The element conversion preconditions require each source component to be initialized. Callers initialize the source before this call, typically on the other side of the foreign-function boundary, which the analyzer cannot trace.");
{% else %}
      return {{ name }}.Pack (To_Ada (Src));
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "big"] %}
   function Unpack (Src : in T) return U_C is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Unpack (Src (J))];
      pragma Annotate (GNATSAS, False_Positive, "precondition",
         "The element conversion preconditions require each source component to be initialized. Callers initialize the source before this call, typically on the other side of the foreign-function boundary, which the analyzer cannot trace.");
{% else %}
      return To_C ({{ name }}.Unpack (Src));
{% endif %}
   end Unpack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Unpack (Src : in T_Le) return U_C is
   begin
{% if element.is_packed_type %}
      return [for J in Src'Range => {{ element.type_package }}.C.Unpack (Src (J))];
      pragma Annotate (GNATSAS, False_Positive, "precondition",
         "The element conversion preconditions require each source component to be initialized. Callers initialize the source before this call, typically on the other side of the foreign-function boundary, which the analyzer cannot trace.");
{% else %}
      return To_C ({{ name }}.Unpack (Src));
{% endif %}
   end Unpack;

{% endif %}

end {{ name }}.C;
