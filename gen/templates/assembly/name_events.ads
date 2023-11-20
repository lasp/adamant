--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if events %}

with Event_Types; use Event_Types;
{% endif %}

-- Event related constants for the {{ name }} assembly.
package {{ name }}_Events is

{% if events %}
   -- Assembly-wide constants:
{% for id, evt in events.items() %}
{% if loop.first %}
   Minimum_Event_Id : constant Event_Id := {{ id }};
{% endif %}
{% endfor %}
{% for id, evt in events.items() %}
{% if loop.last %}
   Maximum_Event_Id : constant Event_Id := {{ id }};
{% endif %}
{% endfor %}
   Number_Of_Events : constant Natural := {{ events|length }};
   Number_Of_Components_With_Events : constant Natural := {{ component_kind_dict['events']|length }};

   -- List of event ids:
{% for id, evt in events.items() %}
   {{ evt.suite.component.instance_name }}_{{ evt.name }} : constant Event_Id := {{ evt.id }}; -- 0x{{ '%04x' % id }}
{% endfor %}
{% else %}
   -- No events in assembly...
{% endif %}

end {{ name }}_Events;
