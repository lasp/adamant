-- Standard includes:
with Ccsds_Parameter_Table_Router_Types; use Ccsds_Parameter_Table_Router_Types;

-- Parameter Table Router Table for component instance: {{ parameter_table_router_instance_name }}
{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   -- Destination tables per table ID:
{% for table_id, entry in table.items() %}
{% if entry.original_table_id != table_id %}
   -- Destination table for Table ID: {{ table_id }} ({{ entry.original_table_id }})
{% else %}
   -- Destination table for Table ID: {{ table_id }}
{% endif %}
{% if entry.resolved_destinations %}
   Destination_Table_{{ table_id }} : aliased Destination_Table := [{% for dest in entry.resolved_destinations %}{{ loop.index0 }} => (Connector_Index => {{ dest.connector_index }}, Load_From => {{ dest.load_from }}){{ ", " if not loop.last }}{% endfor %}];
{% else %}
   -- No destinations for this table ID.
{% endif %}
{% endfor %}

   -- Router table entries:
   Router_Table_Entries : constant Router_Table := [
{% for table_id, entry in table.items() %}
{% if entry.original_table_id != table_id %}
      -- Table entry for Table ID: {{ table_id }} ({{ entry.original_table_id }})
{% else %}
      -- Table entry for Table ID: {{ table_id }}
{% endif %}
{% if entry.resolved_destinations %}
      {{ loop.index0 }} => (Table_Id => {{ table_id }}, Destinations => Destination_Table_{{ table_id }}'Access){{ "," if not loop.last }}
{% else %}
      {{ loop.index0 }} => (Table_Id => {{ table_id }}, Destinations => null){{ "," if not loop.last }}
{% endif %}
{% endfor %}
   ];

end {{ name }};
