--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

pragma Warnings (Off, "no entities of ""Basic_Types"" are referenced");
with Basic_Types; use Basic_Types;
pragma Warnings (On, "no entities of ""Basic_Types"" are referenced");

package body {{ name }}.Assertion is

{% if has_float %}
   function Err_Msg (Index : in Natural) return String is
      Msg_Pre : constant String := "Assertion failed for index ";
      Msg_Post : constant String := " while comparing {{ name }} below.";
   begin
      return Msg_Pre & Natural'Image (Index) & Msg_Post;
   end Err_Msg;

   pragma Warnings (Off, "redundant conversion, ""Epsilon"" is of type ""Long_Float""");
{% if length %}
   procedure Assert_Eq (T1 : in U; T2 : in U; Epsilon : in Long_Float := 0.0) is
   begin
      -- Assert on all indices individually:
      for Idx in T1'Range loop
{% if element.is_packed_type %}
         Element_Assertion.{{ element.type_package }}_U_Assert.Eq (T1 (Idx), T2 (Idx), Epsilon, Err_Msg (Idx));
{% else %}
         Element_Assert.Eq (T1 (Idx), T2 (Idx), {{ element.type }} (Epsilon), Err_Msg (Idx));
{% endif %}
      end loop;
   end Assert_Eq;
{% else %}
   procedure Assert_Eq (T1 : in Unconstrained; T2 : in Unconstrained; Epsilon : in Long_Float := 0.0) is
   begin
      -- Assert on all indices individually:
      for Idx in T1'Range loop
{% if element.is_packed_type %}
         Element_Assertion.{{ element.type_package }}_U_Assert.Eq (T1 (Idx), T2 (Idx), Epsilon, Err_Msg (Idx));
{% else %}
         Element_Assert.Eq (T1 (Idx), T2 (Idx), {{ element.type }} (Epsilon), Err_Msg (Idx));
{% endif %}
      end loop;
   end Assert_Eq;
{% if endianness in ["either", "big"] %}

   procedure Assert_Eq (T1 : in T_Unconstrained; T2 : in T_Unconstrained; Epsilon : in Long_Float := 0.0) is
   begin
      -- Assert on all indices individually:
      for Idx in T1'Range loop
{% if element.is_packed_type %}
         Element_Assertion.{{ element.type_package }}_Assert.Eq (T1 (Idx), T2 (Idx), Epsilon, Err_Msg (Idx));
{% else %}
         Element_Assert.Eq (T1 (Idx), T2 (Idx), {{ element.type }} (Epsilon), Err_Msg (Idx));
{% endif %}
      end loop;
   end Assert_Eq;
{% endif %}
{% if endianness in ["either", "little"] %}

   procedure Assert_Eq (T1 : in T_Le_Unconstrained; T2 : in T_Le_Unconstrained; Epsilon : in Long_Float := 0.0) is
   begin
      -- Assert on all indices individually:
      for Idx in T1'Range loop
{% if element.is_packed_type %}
         Element_Assertion.{{ element.type_package }}_Le_Assert.Eq (T1 (Idx), T2 (Idx), Epsilon, Err_Msg (Idx));
{% else %}
         Element_Assert.Eq (T1 (Idx), T2 (Idx), {{ element.type }} (Epsilon), Err_Msg (Idx));
{% endif %}
      end loop;
   end Assert_Eq;
{% endif %}
{% endif %}
{% if length %}

{% if endianness in ["either", "big"] %}
   procedure Assert_Eq (T1 : in T; T2 : in T; Epsilon : in Long_Float := 0.0) is
   begin
      -- Assert on all indices individually:
      for Idx in T1'Range loop
{% if element.is_packed_type %}
         Element_Assertion.{{ element.type_package }}_Assert.Eq (T1 (Idx), T2 (Idx), Epsilon, Err_Msg (Idx));
{% else %}
         Element_Assert.Eq (T1 (Idx), T2 (Idx), {{ element.type }} (Epsilon), Err_Msg (Idx));
{% endif %}
      end loop;
   end Assert_Eq;

{% endif %}
{% if endianness in ["either", "little"] %}
   procedure Assert_Eq (T1 : in T_Le; T2 : in T_Le; Epsilon : in Long_Float := 0.0) is
   begin
      -- Assert on all indices individually:
      for Idx in T1'Range loop
{% if element.is_packed_type %}
         Element_Assertion.{{ element.type_package }}_Le_Assert.Eq (T1 (Idx), T2 (Idx), Epsilon, Err_Msg (Idx));
{% else %}
         Element_Assert.Eq (T1 (Idx), T2 (Idx), {{ element.type }} (Epsilon), Err_Msg (Idx));
{% endif %}
      end loop;
   end Assert_Eq;

{% endif %}
   pragma Warnings (On, "redundant conversion, ""Epsilon"" is of type ""Long_Float""");

{% if endianness in ["either", "big"] %}
   package body {{ name }}_Assert is
      procedure Eq (T1 : in T; T2 : in T; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2, Epsilon);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T; T2 : in T; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2, Epsilon);
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
      procedure Eq (T1 : in T_Le; T2 : in T_Le; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2, Epsilon);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Le_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T_Le; T2 : in T_Le; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2, Epsilon);
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_Le_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_Le_Assert;

{% endif %}
{% endif %}
{% if length %}
   package body {{ name }}_U_Assert is
      procedure Eq (T1 : in U; T2 : in U; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2, Epsilon);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_U_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in U; T2 : in U; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2, Epsilon);
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_U_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_U_Assert;
{% else %}
   package body {{ name }}_Unconstrained_Assert is
      procedure Eq (T1 : in Unconstrained; T2 : in Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2, Epsilon);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_Unconstrained_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in Unconstrained; T2 : in Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2, Epsilon);
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_Unconstrained_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_Unconstrained_Assert;

{% if endianness in ["either", "big"] %}
   package body {{ name }}_T_Unconstrained_Assert is
      procedure Eq (T1 : in T_Unconstrained; T2 : in T_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2, Epsilon);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_T_Unconstrained_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T_Unconstrained; T2 : in T_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2, Epsilon);
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_T_Unconstrained_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_T_Unconstrained_Assert;

{% endif %}
{% if endianness in ["either", "little"] %}
   package body {{ name }}_T_Le_Unconstrained_Assert is
      procedure Eq (T1 : in T_Le_Unconstrained; T2 : in T_Le_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         Assert_Eq (T1, T2, Epsilon);
      exception
         -- If an assertion was thrown above, then the comparison failed.
         -- Go ahead and call the assert all function to produce the error message.
         when others =>
            {{ name }}_T_Le_Unconstrained_Assert_All.Eq (T1, T2, Message, Filename, Line);
      end Eq;

      procedure Neq (T1 : in T_Le_Unconstrained; T2 : in T_Le_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line) is
      begin
         -- Call Eq. If this fails we know that they are not equal.
         Assert_Eq (T1, T2, Epsilon);
         -- If we got here without an assertion, then T1 and T2 are equal which is
         -- a failure. Call the assert all function to produce the error message.
         {{ name }}_T_Le_Unconstrained_Assert_All.Neq (T1, T2, Message, Filename, Line);
      exception
         -- If an assertion was not thrown above, then the comparison succeeded.
         when others => null;
      end Neq;
   end {{ name }}_T_Le_Unconstrained_Assert;

{% endif %}
{% endif %}
{% endif %}
end {{ name }}.Assertion;
