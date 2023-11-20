--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Fault;
with Fault_Types;
with Sys_Time;
{% if includes %}

-- Parameter Includes
{% for include in includes %}
{% if include not in ["Sys_Time", "Fault_Types", "Fault"] %}
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
   -- Local Fault Identifiers:
   -----------------------------------------------
   Num_Faults : constant Natural := {{ faults|length }};
   type Local_Fault_Id_Type is (
{% for fault in faults %}
      {{ fault.name }}_Id{{ "," if not loop.last }}
{% endfor %}
   );
   for Local_Fault_Id_Type use (
{% for fault in faults %}
{% if ids %}
      {{ fault.name }}_Id => {{ fault.id }}{{ "," if not loop.last }}
{% else %}
      {{ fault.name }}_Id => {{ loop.index0 }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
   );

{% if not ids %}
   -----------------------------------------------
   -- Setter procedure for fault ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Fault_Types.Fault_Id
      with Inline => True;
   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Fault_Types.Fault_Id)
      with Inline => True;

{% endif %}
   -----------------------------------------------
   -- Getter function for global Fault IDs:
   -----------------------------------------------
{% for fault in faults %}
   not overriding function Get_{{ fault.name }}_Id (Self : in Instance) return Fault_Types.Fault_Id
      with Inline => True;
{% endfor %}

   -----------------------------------------------
   -- Fault creation functions:
   -----------------------------------------------
{% for fault in faults %}
{% if fault.description %}
{{ printMultiLine(fault.description, '   -- ') }}
{% endif %}
   not overriding function {{ fault.name }} (Self : in Instance; Timestamp : Sys_Time.T{% if fault.type %}; Param : in {{ fault.type }}{% endif %}) return Fault.T;

{% endfor %}
{% if types %}
   -- Compile time checks to make sure types do not serialize longer than the fault buffer size:
   pragma Warnings (Off, "condition can only be True if invalid values present");
{% for fault in faults %}
{% if fault.type %}
{% if fault.type_model %}
   pragma Compile_Time_Error (
      {{ fault.type_package }}.Size_In_Bytes > Fault_Types.Parameter_Buffer_Type'Length,
      "Fault '{{ fault.name }}' has argument of type '{{ fault.type }}' which has a maximum serialized length larger than the buffer size of Fault.T."
   );
{% else %}
   pragma Compile_Time_Error (
      (({{ fault.type   }}'Object_Size - 1) / 8 + 1) > Fault_Types.Parameter_Buffer_Type'Length,
      "Fault '{{ fault.name }}' has argument of type '{{ fault.type }}' which has a maximum serialized length larger than the buffer size of Fault.T."
   );
{% endif %}
{% endif %}
{% endfor %}
   pragma Warnings (On, "condition can only be True if invalid values present");
{% endif %}
private
   type Instance is tagged limited record
{% if not ids %}
      Id_Base : Fault_Types.Fault_Id := 0;
{% else %}
      null;
{% endif %}
   end record;

end {{ name }};
