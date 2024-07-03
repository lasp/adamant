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
   -- Getter/setter subprograms for packet ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Parameter_Types.Parameter_Id is
   begin
      return Self.Id_Base;
   end Get_Id_Base;

   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Parameter_Types.Parameter_Id) is
   begin
{% if (parameters|length) > 1 %}
      -- ID base set too high for parameter ID set. This is checked in the assembly model.
      pragma Assert (Natural (Id_Base) + {{ (parameters|length) - 1 }} <= Natural (Parameter_Types.Parameter_Id'Last));
{% endif %}
      Self.Id_Base := Id_Base;
   end Set_Id_Base;

   -----------------------------------------------
   -- Getter function for global parameter IDs:
   -----------------------------------------------
{% for param in parameters %}
   not overriding function Get_{{ param.name }}_Id (Self : in Instance) return Parameter_Types.Parameter_Id is
      use Parameter_Types;
   begin
      return Self.Id_Base + Local_Parameter_Id_Type'Enum_Rep ({{ param.name }}_Id);
   end Get_{{ param.name }}_Id;

{% endfor %}
   -----------------------------------------------
   -- Parameter creation functions:
   -----------------------------------------------
{% for param in parameters %}
   not overriding function {{ param.name }} (Self : in Instance; Arg : {{ param.type }}) return Parameter.T is
{% if param.type_model %}
      package Arg_Serializer renames {{ param.type_package }}.Serialization;
{% else %}
      package Arg_Serializer is new Serializer ({{ param.type }});
{% endif %}
      Param : Parameter.T := (Header => (Id => Self.Get_{{ param.name }}_Id, Buffer_Length => Arg_Serializer.Serialized_Length), Buffer => [others => 0]);
   begin
      Param.Buffer (Param.Buffer'First .. (Param.Buffer'First + Arg_Serializer.Serialized_Length - 1)) := Arg_Serializer.To_Byte_Array (Arg);
      return Param;
   end {{ param.name }};

{% endfor %}
end {{ name }};
