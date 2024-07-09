-- Standard includes:
with Fault_Correction_Types;
{% if has_command_args %}
with Basic_Types; use Basic_Types;
with Command_Types;
{% endif %}
with Fault_Correction_Enums; use Fault_Correction_Enums.Startup_Status_Type; use Fault_Correction_Enums.Latching_Type;
{% for include in includes %}
with {{ include }};
{% endfor %}
{% if description %}

{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   --
   -- The fault responses.
   --

{% for response in responses %}
   -- Fault response configuration definition for {{ response.full_fault_name }}:
{% if response.description %}
{{ printMultiLine(response.description, '   -- ') }}
{% endif %}
   {{ response.full_fault_name_str }}_Response : constant Fault_Correction_Types.Fault_Response_Config := (
      -- Set fault ID for {{ response.full_fault_name }}:
      Id => {{ response.fault_id }},
      -- Set latching configuration:
      Latching => {{ response.latching }},
      -- Set startup state:
      Startup_State => {{ response.startup_state }},
      -- Set command response as {{ response.full_command_name }}:
      Command_Response => (
         Header => (
            Source_Id => 0,
            Id => {{ response.command_id }},
            Arg_Buffer_Length => {{ response.command_arg_length }}
         ),
         Arg_Buffer => (
{% if response.command.type_model %}
            {{ response.command_arg_type_model.name }}.Serialization.To_Byte_Array ({{ response.command_arg }}) &
            [0 .. Command_Types.Command_Arg_Buffer_Type'Length - {{ response.command_arg_type_model.name }}.Size_In_Bytes - 1 => 0]
{% else %}
            others => 0
{% endif %}
         )
      )
   );

{% endfor %}
   --
   -- The fault response configuration list:
   --

   Fault_Response_List : constant Fault_Correction_Types.Fault_Response_Config_List := [
{% for response in responses %}
      {{ loop.index0 }} => {{ response.full_fault_name_str }}_Response{{ "," if not loop.last }}
{% endfor %}
   ];

end {{ name }};
