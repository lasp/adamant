--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard includes:
with Smart_Assert;
with {{ name }}.Representation;

package {{ name }}.Assertion is

{% for enum in enums.values() %}
   -- Basic assertion packages for enumeration {{ enum.name }}:
   package {{ enum.name }}_Assert is new Smart_Assert.Basic ({{ name }}.{{ enum.name }}.E, {{ name }}.Representation.{{ enum.name }}_Image);

{% endfor %}
end {{ name }}.Assertion;
