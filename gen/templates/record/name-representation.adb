--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

package body {{ name }}.Representation is

   -- Return string representation of record components
   function To_String (R : in U; Prefix : in String := "") return String is
   begin
      return
{% for field in fields.values() %}
         Prefix & "{{ field.name }} : {{ field.type }} => " & Trim_Both ({{ field.name }}_Image_With_Prefix (R.{{ field.name }}{% if field.type_model %}, Prefix & "   "{% endif %})){{ " & ASCII.LF &" if not loop.last else ";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_String;

{% if endianness in ["either", "big"] %}
   -- Return string representation of record components
   function To_String (R : in T; Prefix : in String := "") return String is
   begin
      return
{% for field in fields.values() %}
         Prefix & "{{ field.name }} : {{ field.type }} => " & Trim_Both ({{ field.name }}_Image_With_Prefix (R.{{ field.name }}{% if field.type_model %}, Prefix & "   "{% endif %})){{ " & ASCII.LF &" if not loop.last else ";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_String;

{% endif %}
{% if endianness in ["either", "little"] %}
   function To_String (R : in T_Le; Prefix : in String := "") return String is
   begin
      return
{% for field in fields.values() %}
         Prefix & "{{ field.name }} : {{ field.type }} => " & Trim_Both ({{ field.name }}_Image_With_Prefix (R.{{ field.name }}{% if field.type_model %}, Prefix & "   "{% endif %})){{ " & ASCII.LF &" if not loop.last else ";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_String;

{% endif %}
   function To_Tuple_String (R : in U) return String is
   begin
      return "(" &
{% for field in fields.values() %}
         "{{ field.name }} => " & Trim_Both ({{ field.name }}_To_Tuple_String (R.{{ field.name }})){{ " & \", \" &" if not loop.last else " & \")\";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_Tuple_String;

{% if endianness in ["either", "big"] %}
   function To_Tuple_String (R : in T) return String is
   begin
      return "(" &
{% for field in fields.values() %}
         "{{ field.name }} => " & Trim_Both ({{ field.name }}_To_Tuple_String (R.{{ field.name }})){{ " & \", \" &" if not loop.last else " & \")\";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_Tuple_String;

{% endif %}
{% if endianness in ["either", "little"] %}
   function To_Tuple_String (R : in T_Le) return String is
   begin
      return "(" &
{% for field in fields.values() %}
         "{{ field.name }} => " & Trim_Both ({{ field.name }}_To_Tuple_String (R.{{ field.name }})){{ " & \", \" &" if not loop.last else " & \")\";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_Tuple_String;

{% endif %}
{% if endianness in ["either", "big"] %}
   -- Return string representation of record components and bytes
   function Image_With_Prefix (R : in T; Prefix : in String := "") return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Image_With_Prefix (R : in T_Le; Prefix : in String := "") return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

{% endif %}
   function Image_With_Prefix (R : in U; Prefix : in String := "") return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

{% if endianness in ["either", "big"] %}
   function Image (R : in T) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Image (R : in T_Le) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;

{% endif %}
   function Image (R : in U) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;

end {{ name }}.Representation;
