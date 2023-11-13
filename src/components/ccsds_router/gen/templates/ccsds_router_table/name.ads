-- Standard includes:
with Ccsds_Router_Types; use Ccsds_Router_Types;

-- Ccsds Router Table for component instance: {{ ccsds_router_instance_name }}
{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   -- APID destination tables:
{% for apid,entry in table.items() %}
   -- Destination table for APID: {{ apid }}
{% if entry.destination_indexes %}
   destination_Table_{{ apid }} : aliased Destination_Table := ({% for dest in entry.destination_indexes %}{{ loop.index0 }} => {{ dest }}{{ ", " if not loop.last }}{% endfor %});
{% else %}
   -- Ignore this APID, there is no destination for it.
{% endif %}
{% endfor %}

   -- Router table entries:
   Router_Table : constant Router_Table_Entry_Array := (
{% for apid,entry in table.items() %}
      -- Table entry for APID: {{ apid }}
{% if entry.destination_indexes %}
      {{ loop.index0 }} => (Apid => {{ apid }}, Destinations => destination_Table_{{ apid }}'Access, Sequence_Count_Mode => {{ entry.sequence_check }}){{ "," if not loop.last }}
{% else %}
      {{ loop.index0 }} => (Apid => {{ apid }}, Destinations => null, Sequence_Count_Mode => {{ entry.sequence_check }}){{ "," if not loop.last }}
{% endif %}
{% endfor %}
   );

end {{ name }};
