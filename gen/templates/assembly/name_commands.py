################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################
{% if commands.items() %}


# Command ID constants:
{% for id, command in commands.items() %}
{{ command.suite.component.instance_name }}_{{ command.name }} = {{ command.id }}
{% endfor %}
{% endif %}

# Reverse lookup: ID to name string
command_id_to_name = {
{% for id, command in commands.items() %}
    {{ command.id }}: "{{ command.suite.component.instance_name }}.{{ command.name }}"{{ "," if not loop.last }}
{% endfor %}
}

# Forward lookup: name string to ID
command_name_to_id = {
{% for id, command in commands.items() %}
    "{{ command.suite.component.instance_name }}.{{ command.name }}": {{ command.id }}{{ "," if not loop.last }}
{% endfor %}
}
