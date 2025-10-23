-- Standard includes:
with Parameters_Component_Types; use Parameters_Component_Types;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name[0]|upper }}{{ name[1:] }} is

   -- The size of the parameter table (in bytes). This includes the size of the parameter
   -- table header (Parameter_Table_Header.T) plus the size of all the parameter table data.
   Parameter_Table_Size_In_Bytes : constant Natural := {{ size }};

   -- A list of the parameter table entries for use by the {{ parameters_instance_name }} component.
   Parameter_Table_Entries : aliased Parameter_Table_Entry_List := [
{% set param_index = namespace(value=0) %}
{% for table_entry in parameters.values() %}
{% for param in table_entry.parameters %}
      -- Parameter {{ param.name }}, size of {{ (table_entry.size/8)|int }} byte(s), Entry_ID {{ table_entry.entry_id }}.
      {{ param_index.value }} => (
         Id => {{ param.parameter.id }},
         Entry_Id => {{ table_entry.entry_id }},
         Component_Id => {{ param.component_id }},
         Start_Index => {{ table_entry.start_index}},
         End_Index => {{ table_entry.end_index}}
      ){{ "," if param_index.value < total_component_parameters - 1 }}
{% set param_index.value = param_index.value + 1 %}
{% endfor %}
{% endfor %}
   ];

end {{ name[0]|upper }}{{ name[1:] }};
