--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if packets %}

with Packet_Types; use Packet_Types;
{% endif %}

-- Packet related constants for the {{ name }} assembly.
package {{ name }}_Packets is

{% if packets %}
   -- Assembly-wide constants:
{% for id, packet in packets.items() %}
{% if loop.first %}
   Minimum_Packet_Id : constant Packet_Id := {{ id }};
{% endif %}
{% endfor %}
{% for id, packet in packets.items() %}
{% if loop.last %}
   Maximum_Packet_Id : constant Packet_Id := {{ id }};
{% endif %}
{% endfor %}
   Number_Of_Packets : constant Natural := {{ packets|length }};
   Number_Of_Components_With_Packets : constant Natural := {{ component_kind_dict['packets']|length }};

   -- List of Packet ids:
{% for id, packet in packets.items() %}
   {{ packet.full_name|replace(".","_") }} : constant Packet_Id := {{ packet.id }}; -- 0x{{ '%04x' % packet.id }}
{% endfor %}
{% else %}
   -- No packets in assembly...
{% endif %}

end {{ name }}_Packets;
