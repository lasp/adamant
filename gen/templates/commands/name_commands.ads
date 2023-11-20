--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes
with Command;
with Command_Types;
{% if variable_length_types %}
with Serializer_Types; use Serializer_Types;
{% endif %}
{% if includes %}

-- Argument Includes
{% for include in includes %}
{% if include not in ["Command", "Command_Types"] %}
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
   -- Local Command Identifiers:
   -----------------------------------------------
   Num_Commands : constant Natural := {{ commands|length }};
   type Local_Command_Id_Type is (
{% for command in commands %}
      {{ command.name }}_Id{{ "," if not loop.last }}
{% endfor %}
   );
   for Local_Command_Id_Type use (
{% for command in commands %}
      {{ command.name }}_Id => {{ loop.index0 }}{{ "," if not loop.last }}
{% endfor %}
   );

   -----------------------------------------------
   -- Setter procedure for command source ID:
   -----------------------------------------------
   not overriding function Get_Source_Id (Self : in out Instance) return Command_Types.Command_Source_Id
      with Inline => True;
   not overriding procedure Set_Source_Id (Self : in out Instance; Source_Id : in Command_Types.Command_Source_Id)
      with Inline => True;

   -----------------------------------------------
   -- Setter procedure for command ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Command_Types.Command_Id
      with Inline => True;
   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Command_Types.Command_Id)
      with Inline => True;

   -----------------------------------------------
   -- Getter function for global command IDs:
   -----------------------------------------------
{% for command in commands %}
   not overriding function Get_{{ command.name }}_Id (Self : in Instance) return Command_Types.Command_Id
      with Inline => True;
{% endfor %}

   -----------------------------------------------
   -- Command creation functions:
   -----------------------------------------------
{% for command in commands %}
{% if command.description %}
{{ printMultiLine(command.description, '   -- ') }}
{% endif %}
{% if command.type and command.type_model and command.type_model.variable_length %}
   not overriding function {{ command.name }} (Self : in Instance{% if command.type %}; Arg : in {{ command.type }}{% endif %}; Cmd : out Command.T) return Serialization_Status;
{% else %}
   not overriding function {{ command.name }} (Self : in Instance{% if command.type %}; Arg : in {{ command.type }}{% endif %}) return Command.T;
{% endif %}
{% endfor %}
{% if types %}

   -- Compile time checks to make sure types do not serialize longer than the command buffer size:
   pragma Warnings (Off, "condition can only be True if invalid values present");
{% for command in commands %}
{% if command.type %}
{% if command.type_model %}
   pragma Compile_Time_Error (
      {{ command.type_package }}.Size_In_Bytes > Command_Types.Command_Arg_Buffer_Type'Length,
      "Command '{{ command.name }}' has argument of type '{{ command.type }}' which has a maximum serialized length larger than the buffer size of Command.T."
   );
{% else %}
   pragma Compile_Time_Error (
      (({{ command.type   }}'Object_Size - 1) / 8 + 1) > Command_Types.Command_Arg_Buffer_Type'Length,
      "Command '{{ command.name }}' has argument of type '{{ command.type }}' which has a maximum serialized length larger than the buffer size of Command.T."
   );
{% endif %}
{% endif %}
{% endfor %}
   pragma Warnings (On, "condition can only be True if invalid values present");
{% endif %}

private
   type Instance is tagged limited record
      Id_Base : Command_Types.Command_Id := 1;
      Source_Id : Command_Types.Command_Source_Id := 0;
   end record;
end {{ name }};
