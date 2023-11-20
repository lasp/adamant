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
-- Parameter Includes
{% for include in complex_type_includes %}
with {{ include }}.Representation;
{% endfor %}

{% endif %}
package body {{ name }}.Representation is

   -----------------------------------------------
   -- Event to string functions:
   -----------------------------------------------
{% for event in events %}
   function {{ event.name }}_Image (Timestamp : in Sys_Time.T; Id : in Event_Types.Event_Id{% if event.type %}; Param : in {{ event.type }}{% endif %}; Instance_Name : in String := "") return String is
   begin
      return Sys_Time.Pretty.Image (Timestamp, 10) & " - " & Instance_Name & ".{{ event.name }} " & "(0x" & Natural_2_Hex_String (Natural (Id), 8) & ")"{% if event.type %} & " : " & Trim_Both ({% if event.type_model %}{{ event.type_package }}.Representation.To_Tuple_String (Param){% else %}{{ event.type }}'Image (Param){% endif %}){% endif %};
   end {{ event.name }}_Image;

   function {{ event.name }}_Image (The_Event : in Event.T; Instance_Name : in String := "") return String is
{% if event.type %}
{% if event.type_model %}
      package Param_Deserializer renames {{ event.type_package }}.Serialization;
{% else %}
      package Param_Deserializer is new Serializer ({{ event.type }});
{% endif %}
{% endif %}
   begin
      return {{ event.name }}_Image (The_Event.Header.Time, The_Event.Header.Id{% if event.type %}, Param_Deserializer.From_Byte_Array (The_Event.Param_Buffer (The_Event.Param_Buffer'First .. The_Event.Param_Buffer'First + Param_Deserializer.Serialized_Length - 1)){% endif %}, Instance_Name);
   end {{ event.name }}_Image;

{% endfor %}
end {{ name }}.Representation;
