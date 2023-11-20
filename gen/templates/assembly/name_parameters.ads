--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if parameters %}

with Parameter_Types; use Parameter_Types;
{% endif %}

-- Parameter related constants for the {{ name }} assembly.
package {{ name }}_Parameters is

{% if parameters %}
   -- Assembly-wide constants:
{% for id, param in parameters.items() %}
{% if loop.first %}
   Minimum_Parameter_Id : constant Parameter_Id := {{ id }};
{% endif %}
{% endfor %}
{% for id, param in parameters.items() %}
{% if loop.last %}
   Maximum_Parameter_Id : constant Parameter_Id := {{ id }};
{% endif %}
{% endfor %}
   Number_Of_Parameters : constant Natural := {{ parameters|length }};
   Number_Of_Components_With_Parameters : constant Natural := {{ component_kind_dict['parameters']|length }};

   -- List of Parameter ids:
{% for id, param in parameters.items() %}
   {{ param.suite.component.instance_name }}_{{ param.name }} : constant Parameter_Id := {{ param.id }}; -- 0x{{ '%04x' % id }}
{% endfor %}
{% else %}
   -- No parameters in assembly...
{% endif %}

end {{ name }}_Parameters;
