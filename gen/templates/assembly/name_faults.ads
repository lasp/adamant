--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if faults %}

with Fault_Types; use Fault_Types;
{% endif %}

-- Fault related constants for the {{ name }} assembly.
package {{ name }}_Faults is

{% if faults %}
   -- Assembly-wide constants:
{% for id, evt in faults.items() %}
{% if loop.first %}
   Minimum_Fault_Id : constant Fault_Id := {{ id }};
{% endif %}
{% endfor %}
{% for id, evt in faults.items() %}
{% if loop.last %}
   Maximum_Fault_Id : constant Fault_Id := {{ id }};
{% endif %}
{% endfor %}
   Number_Of_Faults : constant Natural := {{ faults|length }};
   Number_Of_Components_With_Faults : constant Natural := {{ component_kind_dict['faults']|length }};

{% set based_components = component_kind_dict['faults']|rejectattr('faults.id_base', 'none')|list %}
{% if based_components %}
   -- The fault id base of each component. A component whose faults
   -- carry static ids has no base and is not listed here:
{% for component in based_components %}
   {{ component.instance_name }}_Fault_Id_Base : constant Fault_Id := {{ component.faults.id_base }}; -- 0x{{ '%04x' % component.faults.id_base }}
{% endfor %}

{% endif %}
   -- List of fault ids:
{% for id, evt in faults.items() %}
   {{ evt.suite.component.instance_name }}_{{ evt.name }} : constant Fault_Id := {{ evt.id }}; -- 0x{{ '%04x' % id }}
{% endfor %}
{% else %}
   -- No faults in assembly...
{% endif %}

end {{ name }}_Faults;
