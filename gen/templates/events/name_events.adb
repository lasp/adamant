--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if basic_types %}

-- Standard Includes:
with Serializer;
{% endif %}

package body {{ name }} is

   -----------------------------------------------
   -- Getter/setter subprograms for event ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Event_Types.Event_Id is
   begin
      return Self.Id_Base;
   end Get_Id_Base;

   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Event_Types.Event_Id) is
   begin
{% if (events|length) > 1 %}
      -- ID base set too high for event ID set. This is checked in the assembly model.
      pragma Assert (Natural (Id_Base) + {{ (events|length) - 1 }} <= Natural (Event_Types.Event_Id'Last));
{% endif %}
      Self.Id_Base := Id_Base;
   end Set_Id_Base;

   -----------------------------------------------
   -- Getter function for global Event IDs:
   -----------------------------------------------
{% for event in events %}
   not overriding function Get_{{ event.name }}_Id (Self : in Instance) return Event_Types.Event_Id is
      use Event_Types;
   begin
      return Self.Id_Base + Local_Event_Id_Type'Enum_Rep ({{ event.name }}_Id);
   end Get_{{ event.name }}_Id;

{% endfor %}
   -----------------------------------------------
   -- Event creation functions:
   -----------------------------------------------
{% for event in events %}
   not overriding function {{ event.name }} (Self : in Instance; Timestamp : Sys_Time.T{% if event.type %}; Param : in {{ event.type }}{% endif %}) return Event.T is
{% if event.type %}
{% if event.type_model %}
      package Param_Serializer renames {{ event.type_package }}.Serialization;
{% else %}
      package Param_Serializer is new Serializer ({{ event.type }});
{% endif %}
{% endif %}
      Evnt : {% if not event.type %}constant {% endif %}Event.T := (Header => (Id => Self.Get_{{ event.name }}_Id, Time => Timestamp, Param_Buffer_Length => {% if event.type %}Param_Serializer.Serialized_Length{% else %}0{% endif %}), Param_Buffer => [others => 0]);
   begin
{% if event.type %}
      Evnt.Param_Buffer (Evnt.Param_Buffer'First .. Evnt.Param_Buffer'First + Param_Serializer.Serialized_Length - 1) := Param_Serializer.To_Byte_Array (Param);
{% endif %}
      return Evnt;
   end {{ event.name }};

{% endfor %}
end {{ name }};
