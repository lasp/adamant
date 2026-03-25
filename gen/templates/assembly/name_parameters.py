################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################
{% if parameters.items() %}


# Parameter ID constants:
{% for id, param in parameters.items() %}
{{ param.suite.component.instance_name }}_{{ param.name }} = {{ param.id }}
{% endfor %}
{% endif %}

# Reverse lookup: ID to name string
parameter_id_to_name = {
{% for id, param in parameters.items() %}
    {{ param.id }}: "{{ param.suite.component.instance_name }}.{{ param.name }}"{{ "," if not loop.last }}
{% endfor %}
}

# Forward lookup: name string to ID
parameter_name_to_id = {
{% for id, param in parameters.items() %}
    "{{ param.suite.component.instance_name }}.{{ param.name }}": {{ param.id }}{{ "," if not loop.last }}
{% endfor %}
}
