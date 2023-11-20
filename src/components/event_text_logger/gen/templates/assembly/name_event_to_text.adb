-- Event representation includes:
with Event_Types;
{% for t, components in component_types_dict.items() %}
{% if components[0].events %}
with {{ components[0].events.name }}.Representation;
{% endif %}
{% endfor %}

-- Assembly event to text implementation for the
-- "{{ name }}" assembly.
package body {{ name }}_Event_To_Text is
{% if components %}
   -----------------------------------
   -- Public Subprograms:
   -----------------------------------
   function Event_To_Text (The_Event : in Event.T) return String is
   begin
      case The_Event.Header.Id is
{% for component in components.values() %}
{% if component.events %}
{% for event in component.events %}
         when {{ event.id }} => return {{ component.events.name }}.Representation.{{ event.name }}_Image (The_Event, "{{ component.instance_name }}");
{% endfor %}
{% endif %}
{% endfor %}
         when others => return "Unrecognized event received with Id: " & Event_Types.Event_Id'Image (The_Event.Header.Id);
      end case;
   end Event_To_Text;

{% endif %}
end {{ name }}_Event_To_Text;
