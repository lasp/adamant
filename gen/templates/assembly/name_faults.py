################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################
{% if faults.items() %}


# Fault ID constants:
{% for id, fault in faults.items() %}
{{ fault.suite.component.instance_name }}_{{ fault.name }} = {{ fault.id }}
{% endfor %}
{% endif %}

# Reverse lookup: ID to name string
fault_id_to_name = {
{% for id, fault in faults.items() %}
    {{ fault.id }}: "{{ fault.suite.component.instance_name }}.{{ fault.name }}"{{ "," if not loop.last }}
{% endfor %}
}

# Forward lookup: name string to ID
fault_name_to_id = {
{% for id, fault in faults.items() %}
    "{{ fault.suite.component.instance_name }}.{{ fault.name }}": {{ fault.id }}{{ "," if not loop.last }}
{% endfor %}
}
