-- Standard includes:
with Task_Watchdog_Types; use Task_Watchdog_Types;
with Task_Watchdog_Enums; use Task_Watchdog_Enums;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is
   -- The initial list for the task watchdog component
   Task_Watchdog_Entry_Init_List : aliased Task_Watchdog_Init_List := [
{% for petter in watchdog_list.values() %}
   {% if petter.description %}
{{ printMultiLine(petter.description, '   -- ') }}
   {% endif %}
{{ printMultiLine(petter.component_name + "." + petter.connector_name, '   -- ') }}
{% if petter.name %}
      -- User defined name: {{ petter.name }}
{% endif %}
      {{ petter.connector_id[0] }} => (
         Max_Missed_Pet_Limit => {{ petter.tick_limit }},
         Critical => {{ petter.critical}},
         Action => Watchdog_Action_State.{{ petter.action }},
         Action_Id => {{ petter.fault_id }},
         Petter_Has_Fault => {{ petter.has_fault }}
      ){{ "," if not loop.last }}
{% endfor %}
   ];

end {{ name }};
