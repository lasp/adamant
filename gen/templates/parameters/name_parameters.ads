--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes
with Parameter;
with Parameter_Types;
{% if includes %}

-- Argument Includes
{% for include in includes %}
{% if include not in ["Parameter_Types", "Parameter"] %}
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
   -- Local Parameter Identifiers:
   -----------------------------------------------
   Num_Parameters : constant Natural := {{ parameters|length }};
   type Local_Parameter_Id_Type is (
{% for param in parameters %}
      {{ param.name }}_Id{{ "," if not loop.last }}
{% endfor %}
   );
   for Local_Parameter_Id_Type use (
{% for param in parameters %}
      {{ param.name }}_Id => {{ loop.index0 }}{{ "," if not loop.last }}
{% endfor %}
   );

   -----------------------------------------------
   -- Setter procedure for parameter ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Parameter_Types.Parameter_Id
      with Inline => True;
   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Parameter_Types.Parameter_Id)
      with Inline => True;

   -----------------------------------------------
   -- Getter function for global parameter IDs:
   -----------------------------------------------
{% for param in parameters %}
   not overriding function Get_{{ param.name }}_Id (Self : in Instance) return Parameter_Types.Parameter_Id
      with Inline => True;
{% endfor %}
   -----------------------------------------------
   -- Parameter creation functions:
   -----------------------------------------------
{% for param in parameters %}
{% if param.description %}
{{ printMultiLine(param.description, '   -- ') }}
{% endif %}
   not overriding function {{ param.name }} (Self : in Instance; Arg : {{ param.type }}) return Parameter.T;
{% endfor %}

   -- Compile time checks to make sure types do not serialize longer than the parameter buffer size:
   pragma Warnings (Off, "condition can only be True if invalid values present");
{% for param in parameters %}
{% if param.type_model %}
   pragma Compile_Time_Error (
      {{ param.type_package }}.Size_In_Bytes > Parameter_Types.Parameter_Buffer_Type'Length,
      "Parameter '{{ param.name }}' has argument of type '{{ param.type }}' which has a maximum serialized length larger than the buffer size of Parameter.T."
   );
{% else %}
   pragma Compile_Time_Error (
      (({{ param.type   }}'Object_Size - 1) / 8 + 1) > Parameter_Types.Parameter_Buffer_Type'Length,
      "Parameter '{{ param.name }}' has argument of type '{{ param.type }}' which has a maximum serialized length larger than the buffer size of Parameter.T."
   );
{% endif %}
{% endfor %}
   pragma Warnings (On, "condition can only be True if invalid values present");

private
   type Instance is tagged limited record
      Id_Base : Parameter_Types.Parameter_Id := 1;
   end record;
end {{ name }};
