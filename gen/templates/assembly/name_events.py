################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
################################################################################
{% if events.items() %}
from util import event_class_generator as ecg

# Import event parameter types:
{% set imports = [] %}
{% for id, event in events.items() %}
{% if event.type_model and event.type_package not in imports %}
{% do imports.append(event.type_package) %}
from {{ event.type_package|lower }} import {{ event.type_package }}
{% endif %}
{% endfor %}
{% endif %}

event_id_cls_dict = {
{% for id, event in events.items() %}
    {{ id }}: ecg.create_event_cls(
        "{{ event.suite.component.instance_name }}",
        "{{ event.name }}",
        {% if event.type_model %}{{ event.type_package }}{% else %}None{% endif %},
        "{{ event.description|default('', true)|replace('"', '\\"') }}"{{ "\n    " }}){{ "," if not loop.last }}
{% endfor %}
}
