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
   -- Getter/setter subprograms for command source ID:
   -----------------------------------------------
   not overriding function Get_Source_Id (Self : in out Instance) return Command_Types.Command_Source_Id is
   begin
      return Self.Source_Id;
   end Get_Source_Id;

   not overriding procedure Set_Source_Id (Self : in out Instance; Source_Id : in Command_Types.Command_Source_Id) is
   begin
      Self.Source_Id := Source_Id;
   end Set_Source_Id;

   -----------------------------------------------
   -- Getter/setter subprograms for command ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Command_Types.Command_Id is
   begin
      return Self.Id_Base;
   end Get_Id_Base;

   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Command_Types.Command_Id) is
   begin
{% if (commands|length) > 1 %}
      -- ID base set too high for command ID set. This is checked in the assembly model.
      pragma Assert (Natural (Id_Base) + {{ (commands|length) - 1 }} <= Natural (Command_Types.Command_Id'Last));
{% endif %}
      Self.Id_Base := Id_Base;
   end Set_Id_Base;

   -----------------------------------------------
   -- Getter function for global command IDs:
   -----------------------------------------------
{% for command in commands %}
   not overriding function Get_{{ command.name }}_Id (Self : in Instance) return Command_Types.Command_Id is
      use Command_Types;
   begin
      return Self.Id_Base + Local_Command_Id_Type'Enum_Rep ({{ command.name }}_Id);
   end Get_{{ command.name }}_Id;

{% endfor %}
   -----------------------------------------------
   -- Command creation functions:
   -----------------------------------------------
{% for command in commands %}
{% if command.type and command.type_model and command.type_model.variable_length %}
   not overriding function {{ command.name }} (Self : in Instance{% if command.type %}; Arg : in {{ command.type }}{% endif %}; Cmd : out Command.T) return Serialization_Status is
      Stat : Serialization_Status;
      Len : Natural;
      Ignore : Natural;
   begin
      -- Calculate the serialized length of the argument:
      Stat := {{ command.type_package }}.Serialized_Length (Arg, Len);
      if Stat /= Success then
         return Stat;
      end if;

      -- Set the command length and id and initialize buffer:
      if Len > Command_Types.Command_Arg_Buffer_Length_Type'Last then
         pragma Annotate (CodePeer, False_Positive, "test always false",
            "Some CodePeer can prove the Len can never be too large so this code does not execute. This is OK.");
         return Failure;
         pragma Annotate (CodePeer, False_Positive, "dead code",
            "Some CodePeer can prove the Len can never be too large so this code does not execute. This is OK.");
      end if;
      Cmd := (Header => (Source_Id => Self.Source_Id, Id => Self.Get_{{ command.name }}_Id, Arg_Buffer_Length => Len), Arg_Buffer => [others => 0]);

      -- Serialize the argument onto the buffer:
      Stat := {{ command.type_package }}.Serialization.To_Byte_Array (Cmd.Arg_Buffer, Arg, Ignore);
      if Stat /= Success then
         return Stat;
      end if;

      return Success;
   end {{ command.name }};
{% else %}
   not overriding function {{ command.name }} (Self : in Instance{% if command.type %}; Arg : {{ command.type }}{% endif %}) return Command.T is
{% if command.type %}
{% if command.type_model %}
      package Arg_Serializer renames {{ command.type_package }}.Serialization;
{% else %}
      package Arg_Serializer is new Serializer ({{ command.type }});
{% endif %}
      Cmd : {% if not command.type %}constant {% endif %}Command.T := (Header => (Source_Id => Self.Source_Id, Id => Self.Get_{{ command.name }}_Id, Arg_Buffer_Length => Arg_Serializer.Serialized_Length), Arg_Buffer => [others => 0]);
{% else %}
      Cmd : {% if not command.type %}constant {% endif %}Command.T := (Header => (Source_Id => Self.Source_Id, Id => Self.Get_{{ command.name }}_Id, Arg_Buffer_Length => 0), Arg_Buffer => [others => 0]);
{% endif %}
   begin
{% if command.type %}
      Cmd.Arg_Buffer (Cmd.Arg_Buffer'First .. (Cmd.Arg_Buffer'First + Arg_Serializer.Serialized_Length - 1)) := Arg_Serializer.To_Byte_Array (Arg);
{% endif %}
      return Cmd;
   end {{ command.name }};
{% endif %}

{% endfor %}
end {{ name }};
