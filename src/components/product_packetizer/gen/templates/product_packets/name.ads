-- Standard includes:
with Product_Packet_Types; use Product_Packet_Types;
with Packet_Types;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

{% for packet in packets.values() %}
   -- {{ packet.name }}:
{% if packet.description %}
{{ printMultiLine(packet.description, '   -- ') }}
{% endif %}

   -- {{ packet.name }} data product items:
   -- Total packet buffer size: {{ packet.size }} bits
   {{ packet["name"] }}_Items : aliased Packet_Items_Type := [
{% for dp in packet.data_products %}
      -- Item entry for {{ dp.name }}:
      {{ loop.index }} => (Data_Product_Id => {% if dp.data_product %}{{ dp.data_product.id }}{% else %}0{% endif %}, Use_Timestamp => {% if dp.use_timestamp %}True{% else %}False{% endif %}, Include_Timestamp => {% if dp.include_timestamp %}True{% else %}False{% endif %}, Event_On_Missing => {% if dp.event_on_missing %}True{% else %}False{% endif %}, Packet_Period_Item => {% if dp.packet_period_item %}True{% else %}False{% endif %}, Size => {{ (dp.size/8)|int }}){{ "," if not loop.last }}
{% endfor %}
   ];

   -- {{ packet.name }} packet description:
   {{ packet.name }}_Description : Packet_Description_Type := (
      Id => {{ packet.id }},
      Items => {{ packet.name }}_Items'Access,
      Period => {{ packet.period }},
      Offset => {{ packet.offset }},
      Enabled => {{ packet.enabled }},
      Use_Tick_Timestamp => {{ packet.use_tick_timestamp }},
      Count => Packet_Types.Sequence_Count_Mod_Type'First,
      Send_Now => False
   );

{% endfor %}
   -- List of packets for the packetizer to build:
   Packet_List : aliased Packet_Description_List_Type := [
{% for packet in packets.values() %}
      {{ loop.index }} => {{ packet.name }}_Description{{ "," if not loop.last }}
{% endfor %}
   ];

end {{ name }};
