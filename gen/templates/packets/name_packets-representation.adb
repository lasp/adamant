--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
{% if basic_types %}
with Serializer;
{% endif %}
with String_Util; use String_Util;
with Sys_Time.Pretty;

{% if complex_type_includes %}
-- Packet Type Includes
{% for include in complex_type_includes %}
with {{ include }}.Representation;
{% endfor %}

{% endif %}
package body {{ name }}.Representation is

{% if preamble %}
   -- Preamble code:
   {{ comp.preamble }}

{% endif %}
   -----------------------------------------------
   -- Packet to string functions:
   -----------------------------------------------
{% for p in packets %}
   function {{ p.name }}_Image (Timestamp : in Sys_Time.T; Id : in Packet_Types.Packet_Id; {% if p.type %}Item : in {{ p.type }}{% else %}Buf : in Basic_Types.Byte_Array{% endif %}; Instance_Name : in String := "") return String is
   begin
      return Sys_Time.Pretty.Image (Timestamp, 10) & " - " & Instance_Name & ".{{ p.name }} " & "(0x" & Natural_2_Hex_String (Natural (Id), 8) & ") : " & {% if p.type %}Trim_Both ({% if p.type_package %}{{ p.type_package }}.Representation.To_Tuple_String (Item){% else %}{{ p.type }}'Image (Item){% endif %}){% else %}String_Util.Bytes_To_String (Buf){% endif %};
   end {{ p.name }}_Image;

   function {{ p.name }}_Image (P : in Packet.T; Instance_Name : in String := "") return String is
{% if p.type %}
{% if p.type_model %}
      package Packet_Deserializer renames {{ p.type_package }}.Serialization;
{% else %}
      package Packet_Deserializer is new Serializer ({{ p.type }});
{% endif %}
{% endif %}
   begin
{% if p.type %}
{% if p.type_model and p.type_model.variable_length %}
      declare
         Item : {{ p.type }};
         Ignore : Serialization_Status := Packet_Deserializer.From_Byte_Array (Item, P.Buffer);
      begin
         return {{ p.name }}_Image (P.Header.Time, P.Header.Id, Item, Instance_Name);
      end;
{% else %}
      return {{ p.name }}_Image (P.Header.Time, P.Header.Id, Packet_Deserializer.From_Byte_Array (P.Buffer (P.Buffer'First .. P.Buffer'First + Packet_Deserializer.Serialized_Length - 1)), Instance_Name);
{% endif %}
{% else %}
      return {{ p.name }}_Image (P.Header.Time, P.Header.Id, P.Buffer (P.Buffer'First .. P.Buffer'First + P.Header.Buffer_Length - 1), Instance_Name);
{% endif %}
   end {{ p.name }}_Image;

{% endfor %}
end {{ name }}.Representation;
