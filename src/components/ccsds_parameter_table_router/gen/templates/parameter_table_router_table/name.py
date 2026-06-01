################################################################################
# {{ formatType(model_name) }} {{ formatType(model_type) }}
#
# Generated from {{ filename }} on {{ time }}.
#
# Routing table for the Ccsds_Parameter_Table_Router instance
# '{{ parameter_table_router_instance_name }}'. Each table_id maps to one or
# more downstream component instances; uploading a table with a given id at
# the router fans the payload out to every destination listed under that id.
#
# This file is the Python counterpart to the generated .ads -- intended for
# use by ground-side test scripts and tooling that need to know which numeric
# table_id to use when commanding a given downstream component.
################################################################################

# Every table_id this router manages (in declaration order).
table_ids = [
{% for table_id, entry in table.items() %}
    {{ table_id }},
{% endfor %}
]

# Mapping from each downstream destination instance name to its table_id.
# Forwarders and their paired stores share a table_id by design (a single
# upload fans out to every destination listed under that id).
destination_to_table_id = {
{% for table_id, entry in table.items() %}
{% for dest in entry.destinations %}
    "{{ dest.component_name }}": {{ table_id }},
{% endfor %}
{% endfor %}
}

# Mapping from each table_id to the ordered list of destination instances
# the router will push to when that id arrives.
table_id_to_destinations = {
{% for table_id, entry in table.items() %}
    {{ table_id }}: [{% for dest in entry.destinations %}"{{ dest.component_name }}"{{ ", " if not loop.last }}{% endfor %}],
{% endfor %}
}
