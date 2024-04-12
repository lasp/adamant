--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

pragma Warnings (Off, "no entities of ""Basic_Types"" are referenced");
with Basic_Types; use Basic_Types;
pragma Warnings (On, "no entities of ""Basic_Types"" are referenced");

package body {{ name }}.Assertion is

{% if variable_length %}
   procedure Assert_Eq (T1 : in U; T2 : in U) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.is_packed_type and field.type_model.variable_length %}
      {{ field.name }}_Assertion.{{ field.type_package }}_U_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% elif field.variable_length %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) =
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         "Comparing {{ field.name }} failed."
      );
{% else %}
      pragma Assert (T1.{{ field.name }} = T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% endif %}
{% endfor %}
   end Assert_Eq;

{% if endianness in ["either", "big"] %}
   procedure Assert_Eq (T1 : in T; T2 : in T) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.is_packed_type and field.type_model.variable_length %}
      {{ field.name }}_Assertion.{{ field.type_package }}_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% elif field.variable_length %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) =
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         "Comparing {{ field.name }} failed."
      );
{% else %}
      pragma Assert (T1.{{ field.name }} = T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% endif %}
{% endfor %}
   end Assert_Eq;

{% endif %}
{% if endianness in ["either", "little"] %}
   procedure Assert_Eq (T1 : in T_Le; T2 : in T_Le) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.is_packed_type and field.type_model.variable_length %}
      {{ field.name }}_Assertion.{{ field.type_package }}_Le_Assert.Eq (T1.{{ field.name }}, T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% elif field.variable_length %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) =
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         "Comparing {{ field.name }} failed."
      );
{% else %}
      pragma Assert (T1.{{ field.name }} = T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% endif %}
{% endfor %}
   end Assert_Eq;

{% endif %}
   procedure Assert_Neq (T1 : in U; T2 : in U) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.is_packed_type and field.type_model.variable_length %}
      {{ field.name }}_Assertion.{{ field.type_package }}_U_Assert.Neq (T1.{{ field.name }}, T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% elif field.variable_length %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) /=
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         "Comparing {{ field.name }} failed."
      );
{% else %}
      pragma Assert (T1.{{ field.name }} /= T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% endif %}
{% endfor %}
   end Assert_Neq;

{% if endianness in ["either", "big"] %}
   procedure Assert_Neq (T1 : in T; T2 : in T) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.is_packed_type and field.type_model.variable_length %}
      {{ field.name }}_Assertion.{{ field.type_package }}_Assert.Neq (T1.{{ field.name }}, T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% elif field.variable_length %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) /=
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         "Comparing {{ field.name }} failed."
      );
{% else %}
      pragma Assert (T1.{{ field.name }} /= T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% endif %}
{% endfor %}
   end Assert_Neq;

{% endif %}
{% if endianness in ["either", "little"] %}
   procedure Assert_Neq (T1 : in T_Le; T2 : in T_Le) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.is_packed_type and field.type_model.variable_length %}
      {{ field.name }}_Assertion.{{ field.type_package }}_Le_Assert.Neq (T1.{{ field.name }}, T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% elif field.variable_length %}
      pragma Assert (
         T1.{{ field.name }} (T1.{{ field.name }}'First .. T1.{{ field.name }}'First + Integer (T1.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1) /=
         T2.{{ field.name }} (T2.{{ field.name }}'First .. T2.{{ field.name }}'First + Integer (T2.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }}) - 1),
         "Comparing {{ field.name }} failed."
      );
{% else %}
      pragma Assert (T1.{{ field.name }} /= T2.{{ field.name }}, "Comparing {{ field.name }} failed.");
{% endif %}
{% endfor %}
   end Assert_Neq;

{% endif %}
{% if endianness in ["either", "big"] %}
   package body {{ name }}_Assert is
      procedure Eq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T; T2 : in T; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Neq (T1, T2);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Assert_All.Neq (T1, T2, Message, Filename, Line);
      end Neq;
   end {{ name }}_Assert;

{% endif %}
{% if endianness in ["either", "little"] %}
   package body {{ name }}_Le_Assert is
      procedure Eq (T1 : in T_Le; T2 : in T_Le; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Le_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T_Le; T2 : in T_Le; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Neq (T1, T2);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Le_Assert_All.Neq (T1, T2, Message, Filename, Line);
      end Neq;
   end {{ name }}_Le_Assert;

{% endif %}
   package body {{ name }}_U_Assert is
      procedure Eq (T1 : in U; T2 : in U; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_U_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in U; T2 : in U; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Neq (T1, T2);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_U_Assert_All.Neq (T1, T2, Message, Filename, Line);
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
