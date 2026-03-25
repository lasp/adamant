################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################
{% if data_products.items() %}


# Data product ID constants:
{% for id, dp in data_products.items() %}
{{ dp.suite.component.instance_name }}_{{ dp.name }} = {{ dp.id }}
{% endfor %}
{% endif %}

# Reverse lookup: ID to name string
data_product_id_to_name = {
{% for id, dp in data_products.items() %}
    {{ dp.id }}: "{{ dp.suite.component.instance_name }}.{{ dp.name }}"{{ "," if not loop.last }}
{% endfor %}
}

# Forward lookup: name string to ID
data_product_name_to_id = {
{% for id, dp in data_products.items() %}
    "{{ dp.suite.component.instance_name }}.{{ dp.name }}": {{ dp.id }}{{ "," if not loop.last }}
{% endfor %}
}
