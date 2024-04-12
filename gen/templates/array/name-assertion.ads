--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if not is_volatile_type %}
-- Standard includes:
with Smart_Assert;
with {{ name }}.Representation;
{% if element.type_model %}
with {{ element.type_package }}.Assertion;
{% endif %}
{% endif %}

package {{ name }}.Assertion is

   -- Basic assertion packages for packed array:
   package {{ name }}_U_Assert is new Smart_Assert.Basic ({{ name }}.U, {{ name }}.Representation.Image);
{% if endianness in ["either", "big"] %}
   package {{ name }}_Assert is new Smart_Assert.Basic ({{ name }}.T, {{ name }}.Representation.Image);
{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_Le_Assert is new Smart_Assert.Basic ({{ name }}.T_Le, {{ name }}.Representation.Image);
{% endif %}

   -- Specialized smart assert package for the element in this array type:
{% if element.is_packed_type %}
   package Element_Assertion renames {{ element.type_package }}.Assertion;
   package Element_Assert renames Element_Assertion.{{ element.type_package }}_Assert;
{% elif element.is_enum %}
   package Element_Assertion renames {{ element.type_package }}.Assertion;
   package Element_Assert renames Element_Assertion.{{ element.type_model.name }}_Assert;
{% else %}
   package Element_Assert is new Smart_Assert.Basic ({{ element.type }}, {{ name }}.Representation.Element_Image);
{% endif %}

end {{ name }}.Assertion;
