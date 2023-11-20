--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

pragma Warnings (Off, "no entities of ""Basic_Types"" are referenced");
with Basic_Types; use Basic_Types;
pragma Warnings (On, "no entities of ""Basic_Types"" are referenced");

package body {{ name }}.Assertion is

{% if is_volatile_type %}
   -- Assertion not supported for volatile record. Convert to a regular record for
   -- a validation checking function.
   procedure Dummy_Assertion is
   begin
      null;
   end Dummy_Assertion;
{% else %}
{% if variable_length %}
   procedure Assert_Eq (T1 : in T; T2 : in T) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.variable_length %}
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

   procedure Assert_Neq (T1 : in T; T2 : in T) is
{% for include in type_uses %}
      use {{ include }};
{% endfor %}
   begin
      -- Assert on all fields individually:
{% for field in fields.values() %}
{% if field.variable_length %}
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

   package body {{ name }}_Le_Assert is
      procedure Eq (T1 : in T_Le; T2 : in T_Le; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T (T1), T (T2));
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Le_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T_Le; T2 : in T_Le; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Neq (T (T1), T (T2));
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Le_Assert_All.Neq (T1, T2, Message, Filename, Line);
      end Neq;
   end {{ name }}_Le_Assert;

   package body {{ name }}_U_Assert is
      procedure Eq (T1 : in U; T2 : in U; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T (T1), T (T2));
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_U_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in U; T2 : in U; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Neq (T (T1), T (T2));
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
{% endif %}
end {{ name }}.Assertion;
