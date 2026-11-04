-------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if commands %}

with Command_Types; use Command_Types;
{% endif %}

-- Command related constants for the {{ name }} assembly.
package {{ name }}_Commands is

{% if commands %}
   -- Assembly-wide constants:
{% for id, command in commands.items() %}
{% if loop.first %}
   Minimum_Command_Id : constant Command_Id := {{ id }};
{% endif %}
{% endfor %}
{% for id, command in commands.items() %}
{% if loop.last %}
   Maximum_Command_Id : constant Command_Id := {{ id }};
{% endif %}
{% endfor %}
   Number_Of_Commands : constant Natural := {{ commands|length }};
   Number_Of_Components_With_Commands : constant Natural := {{ component_kind_dict['commands']|length }};

   -- The command id base of each component:
{% for component in component_kind_dict['commands'] %}
   {{ component.instance_name }}_Command_Id_Base : constant Command_Id := {{ component.commands.id_base }};
{% endfor %}

   -- List of command ids:
{% for id, command in commands.items() %}
   {{ command.suite.component.instance_name }}_{{ command.name }} : constant Command_Id := {{ command.id }}; -- 0x{{ '%04x' % id }}
{% endfor %}
{% else %}
   -- No commands in assembly...
{% endif %}

end {{ name }}_Commands;
