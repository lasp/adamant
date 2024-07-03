-- Standard includes:
with Memory_Region;
with Component.Sequence_Store;
{% for include in includes %}
{% if include not in ["Memory_Region", "Component.Sequence_Store"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% if description %}

{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   --
   -- A list of the slots for the {{ sequence_store_instance_name }} component.
   --
{% for slot in slots %}
   Slot_{{ slot.number }} : constant Memory_Region.T := (
      Address => {{ slot.address }},
      Length => Natural ({{ slot.length }})
   );

{% endfor %}
   --
   -- The slots type which is a list of all the slots:
   --

   Slots : aliased Component.Sequence_Store.Sequence_Slot_Array := [
{% for slot in slots %}
      {{ loop.index0 }} => Slot_{{ slot.number }}{{ "," if not loop.last }}
{% endfor %}
   ];

   Slots_Access : constant Component.Sequence_Store.Sequence_Slot_Array_Access
      := Slots'Access;

end {{ name }};
