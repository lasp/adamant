################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################

from base_classes.enum_base import EnumBase
{% if description %}

{{ printMultiLine(description, '# ') }}
{% endif %}
{% for enum in enums.values() %}


# {{ enum.name }} Definition:
{% if enum.description %}
{{ printMultiLine(enum.description, '# ') }}
{% endif %}
class {{ enum.name }}(EnumBase):
{% for literal in enum.literals %}
{% if literal.description %}
{{ printMultiLine(literal.description, '    # ') }}
{% endif %}
    {{ literal.name }}{{ "_" if literal.name in ["None", "True", "False"] }} = {{ literal.value }}{{ "\n" if not loop.last}}{% endfor %}
{{ "\n" if not loop.last}}{% endfor %}
