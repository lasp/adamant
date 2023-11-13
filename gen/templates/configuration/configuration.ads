--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- This file contains the project specific Adamant configuration values generated from:
--
-- {{ full_filename }}
--
-- See the YAML file for more information.
package Configuration is

   Name : constant String := "{{ model_name }}";
   File_Name : constant String := "{{ full_filename }}";
{% for key, value in data.items() %}
{% if value is string %}
   {{ formatType(key) }} : constant String := "{{ value }}";
{% elif value is number %}
{% if value >= 0 %}
   {{ formatType(key) }} : constant Natural := {{ value }};
{% else %}
   {{ formatType(key) }} : constant Integer := {{ value }};
{% endif %}
{% endif %}
{% endfor %}

end Configuration;
