--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }}
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, "unit ""String_Util"" is not referenced");
with String_Util;
pragma Warnings (On, "unit ""String_Util"" is not referenced");
{% if includes %}
{% for include in includes %}
{% if include not in unpacked_type_includes %}
pragma Warnings (Off, "unit ""{{ include }}"" is not referenced");
with {{ include }};
pragma Warnings (On, "unit ""{{ include }}"" is not referenced");
{% endif %}
{% endfor %}
{% endif %}
{% if type_includes %}
{% for include in unpacked_type_includes %}
pragma Warnings (Off, "unit ""{{ include }}"" is not referenced");
with {{ include }};
pragma Warnings (On, "unit ""{{ include }}"" is not referenced");
{% endfor %}
{% endif %}

procedure {{ name }}_Type_Ranges is
{% if preamble %}
   -- Preamble code:
   pragma Warnings (Off);
{{ printMultiLine(preamble, '   ', 10000) }}
   pragma Warnings (On);
{% endif %}
begin
   Put_Line ("---");
   Put_Line ("types:");
{% for unpacked_type_field in unpacked_typed_fields %}
{% if not unpacked_type_field.format.length and not unpacked_type_field.skip_validation %}
{% if unpacked_type_field.format.type[0] == "E" %}
   Put_Line ("  - name: {{ unpacked_type_field.type }}");
   Put_Line ("    literals:");
   for Val in {{ unpacked_type_field.type }} loop
      Put_Line ("      - name: """ & String_Util.Trim_Both ({{ unpacked_type_field.type }}'Image (Val)) & """");
      Put_Line ("        value: " & String_Util.Trim_Both (Integer'Image ({{ unpacked_type_field.type }}'Enum_Rep (Val))));
   end loop;
{% elif unpacked_type_field.format.length and unpacked_type_field.format.length > 1 %}
   Put_Line ("  - name: {{ unpacked_type_field.type }}");
{% else %}
   Put_Line ("  - name: {{ unpacked_type_field.type }}");
   Put_Line ("    min: " & String_Util.Trim_Both ({{ unpacked_type_field.type }}'Image ({{ unpacked_type_field.type }}'First)));
   Put_Line ("    max: " & String_Util.Trim_Both ({{ unpacked_type_field.type }}'Image ({{ unpacked_type_field.type }}'Last)));
{% endif %}
{% endif %}
{% endfor %}
end {{ name }}_Type_Ranges;
