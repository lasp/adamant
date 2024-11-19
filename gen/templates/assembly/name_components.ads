--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Components Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if components_ads_includes %}
-- Includes:
{% for include in components_ads_includes %}
with {{ include }};
{% endfor %}
{% for include in includes %}
{% if include not in components_ads_includes %}
pragma Warnings (Off, "unit ""{{ include }}"" is not referenced");
with {{ include }};
pragma Warnings (On, "unit ""{{ include }}"" is not referenced");
{% endif %}
{% endfor %}

{% endif %}
{% if prepreamble %}
-- Pre-Preamble code:
{{ printMultiLine(prepreamble, '', 10000) }}
{% endif %}
{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }}_Components is
{% if preamble %}

   -- Preamble code:
{{ printMultiLine(preamble, '   ', 10000) }}
{% endif %}
{% if components %}

   -----------------------------------
   -- Component Instances:
   -----------------------------------
   -- Remove some reference style checking to deal with incorrect capitalization in system packages.
   pragma Style_Checks ("-rn");

{% for component in components.values() %}
{% if component.instance_description %}
{{ printMultiLine(component.instance_description, '   -- ') }}
{% endif %}
{% macro capfirst(text) %}{{ text[0]|upper}}{{text[1:] }}{% endmacro %}
{% if component.generic %}
   package {{ capfirst(component.instance_name) }}_Base_Package is new Component.{{ component.name }} ({{ component.generic.resolved_formal_parameter_call_string() }});
   package {{ capfirst(component.instance_name) }}_Package is new {{ capfirst(component.instance_name) }}_Base_Package.Implementation;
{% endif %}
   {{ component.instance_name }} : aliased {% if component.generic %}{{ capfirst(component.instance_name) }}_Package{% else %}Component.{{ component.name }}.Implementation{% endif %}.Instance{% if component.discriminant.parameters %} ({{ component.discriminant.parameter_call_string() }}){% endif %};
{% endfor %}

{% endif %}
   -- Remove some reference style checking to deal with incorrect capitalization in system packages.
   pragma Style_Checks ("+rn");

{% if task_list %}
   -----------------------------------
   -- Task Creation:
   -----------------------------------
{% for task in task_list %}
   -- Instantiation of the {{ task.component_name }} component:
   {{ task.component_name }}_{{ task.name }}_Task_Info : aliased Task_Types.Task_Info := (
      Number => {{ task.number }},
      Id => Ada.Task_Identification.Null_Task_Id,
      -- The following is initialized by the component itself.
      Priority => 0,
      Stack_Address => System.Null_Address,
      Stack_Size => 0,
      Secondary_Stack_Address => System.Null_Address,
      Secondary_Stack_Size => 0,
      Secondary_Stack_Max_Usage => 0
   );
   {{ task.component_name }}_{{ task.name }}_Task_Signal : aliased Ada.Synchronous_Task_Control.Suspension_Object;
   {{ task.component_name }}_{{ task.name }}_Task : Component.{% if task.name != "Active" %}{{ task.component_type }}.{% endif %}{{ task.name }}_Task (
      Task_Data => {{ task.component_name }}_{{ task.name }}_Task_Info'Access,
      Class_Self => {{ task.component_name }}'Access,
      Signal => {{ task.component_name }}_{{ task.name }}_Task_Signal'Access,
      Pri => {{ task.priority }},
      Stack_Size => {{ task.stack_size }},
      Secondary_Stack_Size => {{ task.secondary_stack_size }}
   );

{% endfor %}
   -- List of task infos for all tasks:
   Task_List : aliased Task_Types.Task_Info_List := [
{% for task in task_list %}
      -- {{ task.component_name }}.{{ task.name }}:
      {{ task.number }} => {{ task.component_name }}_{{ task.name }}_Task_Info'Access{{ "," if not loop.last }}
{% endfor %}
   ];

{% else %}
   -- List of task infos for all tasks:
   Task_List : aliased Task_Types.Task_Info_List := [1 .. 0 => null]; -- empty

{% endif %}
{% if interrupt_list %}
   -- Remove some reference style checking to deal with incorrect capitalization in system packages.
   pragma Style_Checks ("-rn");

   -- List of all interrupts used in the system:
   Interrupt_List : aliased Interrupt_Types.Interrupt_Id_List := [
{% for interrupt in interrupt_list %}
      -- {{ interrupt.component_name }}.{{ interrupt.name }}:
      {{ loop.index0 }} => {{ interrupt.id }}{{ "," if not loop.last }}
{% endfor %}
   ];

   -- Remove some reference style checking to deal with incorrect capitalization in system packages.
   pragma Style_Checks ("+rn");

{% else %}
   -- List of all interrupts used in the system:
   Interrupt_List : aliased Interrupt_Types.Interrupt_Id_List := [1 .. 0 => 0]; -- empty

{% endif %}
{% if component_kind_dict["queued"] %}
   -- List of all components with positive queue sizes in the system:
   Queued_Component_List : aliased Component.Component_List := [
{% for component in component_kind_dict["queued"] %}
      {{ loop.index0 }} => {{ component.instance_name }}'Access{{ "," if not loop.last }}
{% endfor %}
   ];

{% else %}
   -- List of all components with positive queue sizes in the system:
   Queued_Component_List : aliased Component.Component_List := [1 .. 0 => null]; -- empty

{% endif %}
end {{ name }}_Components;
