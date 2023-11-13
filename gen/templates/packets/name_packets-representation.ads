--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Packet;
with Packet_Types;
with Sys_Time;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }}.Representation is

   -----------------------------------------------
   -- Packet to string functions:
   -----------------------------------------------
{% for p in packets %}
   function {{ p.name }}_Image (Timestamp : in Sys_Time.T; Id : in Packet_Types.Packet_Id; {% if p.type %}Item : in {{ p.type }}{% else %}Buf : in Basic_Types.Byte_Array{% endif %}; Instance_Name : in String := "") return String;
   function {{ p.name }}_Image (P : in Packet.T; Instance_Name : in String := "") return String;

{% endfor %}
end {{ name }}.Representation;
