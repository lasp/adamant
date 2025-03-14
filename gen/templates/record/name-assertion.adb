--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

pragma Warnings (Off, "no entities of ""Basic_Types"" are referenced");
with Basic_Types; use Basic_Types;
pragma Warnings (On, "no entities of ""Basic_Types"" are referenced");

package body {{ name }}.Assertion is

{% if variable_length or has_float %}
   function Err_Msg (Field_Name : in String) return String is
      Msg_Pre : constant String := "Assertion failed for field ";
      Msg_Post : constant String := " while comparing {{ name }} below.";
   begin
      return Msg_Pre & Field_Name & Msg_Post;
   end Err_Msg;

{% if has_float %}
   pragma Warnings (Off, "redundant conversion, ""Epsilon"" is of type ""Long_Float""");
{% endif %}
   procedure Assert_Eq (T1 : in U; T2 : in U{% if has_float %}; Epsilon : in Long_Float := 0.0{% endif %}) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.variable_length %}
{% if field.has_float %}
      pragma Assert (Epsilon = 0.0, "Floating point comparisons with Epsilon > 0.0 for an array of floats is not yet supported.");
{% endif %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) =
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         Err_Msg ("{{ field.name }}")
      );
{% elif field.is_packed_type %}
      {{ field.name }}_Assertion.{{ field.type_package }}_U_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }},{% if field.has_float %} Epsilon,{% endif %} Err_Msg ("{{ field.name }}"));
{% else %}
      {{ field.name }}_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }},{% if field.has_float %} {{ field.type }} (Epsilon),{% endif %} Err_Msg ("{{ field.name }}"));
{% endif %}
{% endfor %}
   end Assert_Eq;

{% if endianness in ["either", "big"] %}
   procedure Assert_Eq (T1 : in T; T2 : in T{% if has_float %}; Epsilon : in Long_Float := 0.0{% endif %}) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.variable_length %}
{% if field.has_float %}
      pragma Assert (Epsilon = 0.0, "Floating point comparisons with Epsilon > 0.0 for a variable sized array of floats is not yet supported.");
{% endif %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) =
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         Err_Msg ("{{ field.name }}")
      );
{% elif field.is_packed_type %}
      {{ field.name }}_Assertion.{{ field.type_package }}_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }},{% if field.has_float %} Epsilon,{% endif %} Err_Msg ("{{ field.name }}"));
{% else %}
      {{ field.name }}_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }},{% if field.has_float %} {{ field.type }} (Epsilon),{% endif %} Err_Msg ("{{ field.name }}"));
{% endif %}
{% endfor %}
   end Assert_Eq;

{% endif %}
{% if endianness in ["either", "little"] %}
   procedure Assert_Eq (T1 : in T_Le; T2 : in T_Le{% if has_float %}; Epsilon : in Long_Float := 0.0{% endif %}) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.variable_length %}
{% if field.has_float %}
      pragma Assert (Epsilon = 0.0, "Floating point comparisons with Epsilon > 0.0 for a variable sized array of floats is not yet supported.");
{% endif %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) =
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         Err_Msg ("{{ field.name }}")
      );
{% elif field.is_packed_type %}
      {{ field.name }}_Assertion.{{ field.type_package }}_Le_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }},{% if field.has_float %} Epsilon,{% endif %} Err_Msg ("{{ field.name }}"));
{% else %}
      {{ field.name }}_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }},{% if field.has_float %} {{ field.type }} (Epsilon),{% endif %} Err_Msg ("{{ field.name }}"));
{% endif %}
{% endfor %}
   end Assert_Eq;

{% endif %}
{% if has_float %}
   pragma Warnings (On, "redundant conversion, ""Epsilon"" is of type ""Long_Float""");
{% endif %}

{% if endianness in ["either", "big"] %}
   package body {{ name }}_Assert is
      procedure Eq (T1 : in T; T2 : in T;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2{% if has_float %}, Epsilon{% endif %});
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T; T2 : in T;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2{% if has_float %}, Epsilon{% endif %});
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_Assert;

{% endif %}
{% if endianness in ["either", "little"] %}
   package body {{ name }}_Le_Assert is
      procedure Eq (T1 : in T_Le; T2 : in T_Le;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2{% if has_float %}, Epsilon{% endif %});
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Le_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T_Le; T2 : in T_Le;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2{% if has_float %}, Epsilon{% endif %});
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_Le_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_Le_Assert;

{% endif %}
   package body {{ name }}_U_Assert is
      procedure Eq (T1 : in U; T2 : in U;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2{% if has_float %}, Epsilon{% endif %});
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_U_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in U; T2 : in U;{% if has_float %} Epsilon : in Long_Float := 0.0;{% endif %} Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2{% if has_float %}, Epsilon{% endif %});
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_U_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_U_Assert;

{% else %}
   package body Dummy is
      procedure Dumb is
      begin
         null;
      end Dumb;
   end Dummy;

{% endif %}
end {{ name }}.Assertion;
