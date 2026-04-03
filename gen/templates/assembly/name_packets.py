################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################
{% if packets.items() %}

# Packet ID constants:
{% for id, packet in packets.items() %}
{{ packet.full_name|replace(".","_") }} = {{ packet.id }}
{% endfor %}

{% endif -%}

# Reverse lookup: ID to name string
packet_id_to_name = {
{% for id, packet in packets.items() %}
    {{ packet.id }}: "{{ packet.full_name }}"{{ "," if not loop.last }}
{% endfor %}
}

# Forward lookup: name string to ID
packet_name_to_id = {
{% for id, packet in packets.items() %}
    "{{ packet.full_name }}": {{ packet.id }}{{ "," if not loop.last }}
{% endfor %}
}
