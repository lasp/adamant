--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Event;
with Event_Types;
with Sys_Time;
{% if includes %}

-- Parameter Includes
{% for include in includes %}
{% if include not in ["Sys_Time", "Event_Types", "Event"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if description %}

{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   -- Object instance type:
   type Instance is tagged limited private;

   -----------------------------------------------
   -- Local Event Identifiers:
   -----------------------------------------------
   Num_Events : constant Natural := {{ events|length }};
   type Local_Event_Id_Type is (
{% for event in events %}
      {{ event.name }}_Id{{ "," if not loop.last }}
{% endfor %}
   );
   for Local_Event_Id_Type use (
{% for event in events %}
      {{ event.name }}_Id => {{ loop.index0 }}{{ "," if not loop.last }}
{% endfor %}
   );

   -----------------------------------------------
   -- Setter procedure for event ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Event_Types.Event_Id
      with Inline => True;
   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Event_Types.Event_Id)
      with Inline => True;

   -----------------------------------------------
   -- Getter function for global Event IDs:
   -----------------------------------------------
{% for event in events %}
   not overriding function Get_{{ event.name }}_Id (Self : in Instance) return Event_Types.Event_Id
      with Inline => True;
{% endfor %}

   -----------------------------------------------
   -- Event creation functions:
   -----------------------------------------------
{% for event in events %}
{% if event.description %}
{{ printMultiLine(event.description, '   -- ') }}
{% endif %}
   not overriding function {{ event.name }} (Self : in Instance; Timestamp : Sys_Time.T{% if event.type %}; Param : in {{ event.type }}{% endif %}) return Event.T;

{% endfor %}
{% if types %}
   -- Compile time checks to make sure types do not serialize longer than the event buffer size:
   pragma Warnings (Off, "condition can only be True if invalid values present");
{% for event in events %}
{% if event.type %}
{% if event.type_model %}
   pragma Compile_Time_Error (
      {{ event.type_package }}.Size_In_Bytes > Event_Types.Parameter_Buffer_Type'Length,
      "Event '{{ event.name }}' has argument of type '{{ event.type }}' which has a maximum serialized length larger than the buffer size of Event.T."
   );
{% else %}
   pragma Compile_Time_Error (
      (({{ event.type   }}'Object_Size - 1) / 8 + 1) > Event_Types.Parameter_Buffer_Type'Length,
      "Event '{{ event.name }}' has argument of type '{{ event.type }}' which has a maximum serialized length larger than the buffer size of Event.T."
   );
{% endif %}
{% endif %}
{% endfor %}
   pragma Warnings (On, "condition can only be True if invalid values present");
{% endif %}
private
   type Instance is tagged limited record
      Id_Base : Event_Types.Event_Id := 0;
   end record;

end {{ name }};
