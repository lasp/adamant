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

{% if not ids %}
   -----------------------------------------------
   -- Getter/setter subprograms for fault ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Fault_Types.Fault_Id is
   begin
      return Self.Id_Base;
   end Get_Id_Base;

   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Fault_Types.Fault_Id) is
   begin
{% if (faults|length) > 1 %}
      -- ID base set too high for fault ID set. This is checked in the assembly model.
      pragma Assert (Natural (Id_Base) + {{ (faults|length) - 1 }} <= Natural (Fault_Types.Fault_Id'Last));
{% endif %}
      Self.Id_Base := Id_Base;
   end Set_Id_Base;

{% endif %}
   -----------------------------------------------
   -- Getter function for global fault IDs:
   -----------------------------------------------
{% for fault in faults %}
   not overriding function Get_{{ fault.name }}_Id (Self : in Instance) return Fault_Types.Fault_Id is
{% if ids %}
      Ignore : Instance renames Self;
{% else %}
      use Fault_Types;
{% endif %}
   begin
{% if ids %}
      -- Return hard coded fault identifier:
      return Local_Fault_Id_Type'Enum_Rep ({{ fault.name }}_Id);
{% else %}
      -- Return calculated fault identifier:
      return Self.Id_Base + Local_Fault_Id_Type'Enum_Rep ({{ fault.name }}_Id);
{% endif %}
   end Get_{{ fault.name }}_Id;

{% endfor %}
   -----------------------------------------------
   -- Fault creation functions:
   -----------------------------------------------
{% for fault in faults %}
   not overriding function {{ fault.name }} (Self : in Instance; Timestamp : Sys_Time.T{% if fault.type %}; Param : in {{ fault.type }}{% endif %}) return Fault.T is
{% if fault.type %}
{% if fault.type_model %}
      package Param_Serializer renames {{ fault.type_package }}.Serialization;
{% else %}
      package Param_Serializer is new Serializer ({{ fault.type }});
{% endif %}
{% endif %}
      Flt : {% if not fault.type %}constant {% endif %}Fault.T := (Header => (Id => Self.Get_{{ fault.name }}_Id, Time => Timestamp, Param_Buffer_Length => {% if fault.type %}Param_Serializer.Serialized_Length{% else %}0{% endif %}), Param_Buffer => (others => 0));
   begin
{% if fault.type %}
      Flt.Param_Buffer (Flt.Param_Buffer'First .. Flt.Param_Buffer'First + Param_Serializer.Serialized_Length - 1) := Param_Serializer.To_Byte_Array (Param);
{% endif %}
      return Flt;
   end {{ fault.name }};

{% endfor %}
end {{ name }};
