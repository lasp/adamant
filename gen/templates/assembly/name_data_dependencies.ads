--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if data_dependencies %}

with Data_Product_Types; use Data_Product_Types;
{% endif %}

-- Data dependency related constants for the {{ name }} assembly.
package {{ name }}_Data_Dependencies is

{% if data_dependencies %}
   -- Assembly-wide constants:
   Number_Of_Data_Dependencies : constant Natural := {{ num_data_dependencies }};
   Number_Of_Components_With_Data_Dependencies : constant Natural := {{ component_kind_dict['data_dependencies']|length }};

   -- List of data dependency ids. These have been resolved and refer to data product IDs in the system:
{% for id, dd_list in data_dependencies.items() %}
{% for dd in dd_list %}
   {{ dd.suite.component.instance_name }}_{{ dd.name }} : constant Data_Product_Id := {{ dd.id }}; -- 0x{{ '%04x' % id }}
{% endfor %}
{% endfor %}
{% else %}
   -- No data dependencies in assembly...
{% endif %}

end {{ name }}_Data_Dependencies;
