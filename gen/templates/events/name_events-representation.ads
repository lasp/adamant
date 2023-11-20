--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Event;
with Event_Types;
with Sys_Time;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }}.Representation is

   -----------------------------------------------
   -- Event to string functions:
   -----------------------------------------------
{% for event in events %}
   function {{ event.name }}_Image (Timestamp : in Sys_Time.T; Id : in Event_Types.Event_Id{% if event.type %}; Param : in {{ event.type }}{% endif %}; Instance_Name : String := "") return String;
   function {{ event.name }}_Image (The_Event : in Event.T; Instance_Name : in String := "") return String;

{% endfor %}
end {{ name }}.Representation;
