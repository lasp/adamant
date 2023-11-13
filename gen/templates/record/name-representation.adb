--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

package body {{ name }}.Representation is

{% if is_volatile_type %}
   -- Validation not supported for volatile record. Convert to a regular record for
   -- a validation checking function.
   procedure Dummy_Image is
   begin
      null;
   end Dummy_Image;
{% else %}
   -- Return string representation of record components
   function To_String (R : in T; Prefix : in String := "") return String is
   begin
      return
{% for field in fields.values() %}
         Prefix & "{{ field.name }} : {{ field.type }} = " & Trim_Both ({{ field.name }}_Image_With_Prefix (R.{{ field.name }}{% if field.type_model %}, Prefix & "   "{% endif %})){{ " & ASCII.LF &" if not loop.last else ";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_String;

   -- Return string representation of record components
   function To_String (R : in T_Le; Prefix : in String := "") return String is
   begin
      return To_String (T (R), Prefix);
   end To_String;

   function To_String (R : in U; Prefix : in String := "") return String is
   begin
      return To_String (T (R), Prefix);
   end To_String;

   function To_Tuple_String (R : in T) return String is
   begin
      return "(" &
{% for field in fields.values() %}
         "{{ field.name }} = " & Trim_Both ({{ field.name }}_To_Tuple_String (R.{{ field.name }})){{ " & \", \" &" if not loop.last else " & \")\";" }}
{% endfor %}
   exception
      when Constraint_Error =>
         return "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_Tuple_String;

   function To_Tuple_String (R : in T_Le) return String is
   begin
      return To_Tuple_String (T (R));
   end To_Tuple_String;

   function To_Tuple_String (R : in U) return String is
   begin
      return To_Tuple_String (T (R));
   end To_Tuple_String;

   -- Return string representation of record components and bytes
   function Image_With_Prefix (R : in T; Prefix : in String := "") return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

   function Image_With_Prefix (R : in T_Le; Prefix : in String := "") return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

   function Image_With_Prefix (R : in U; Prefix : in String := "") return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

   function Image (R : in T) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;

   function Image (R : in T_Le) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;

   function Image (R : in U) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;
{% endif %}

end {{ name }}.Representation;
