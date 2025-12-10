--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Assertion Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if not is_volatile_type %}
-- Standard includes:
with Smart_Assert;
with {{ name }}.Representation;
{% if has_float %}
with GNAT.Source_Info;
{% endif %}
{% if element.type_model %}
with {{ element.type_package }}.Assertion;
{% endif %}
{% endif %}

package {{ name }}.Assertion is

{% if has_float %}
   package Sinfo renames GNAT.Source_Info;

   -- Special assertion package for floating point arrays that allow the
   -- passing of an epsilon to compare floats without requiring exact
   -- equality.
{% if length %}
   package {{ name }}_U_Assert is
      procedure Eq (T1 : in U; T2 : in U; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in U; T2 : in U; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_U_Assert;

{% if endianness in ["either", "big"] %}
   package {{ name }}_Assert is
      procedure Eq (T1 : in T; T2 : in T; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T; T2 : in T; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_Assert;

{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_Le_Assert is
      procedure Eq (T1 : in T_Le; T2 : in T_Le; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T_Le; T2 : in T_Le; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_Le_Assert;
{% endif %}

   -- This package compares data without any Epsilon, using the
   -- Smart_Assert.Basic package.
   package {{ name }}_U_Assert_All is new Smart_Assert.Basic ({{ name }}.U, {{ name }}.Representation.Image);
{% if endianness in ["either", "big"] %}
   package {{ name }}_Assert_All is new Smart_Assert.Basic ({{ name }}.T, {{ name }}.Representation.Image);
{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_Le_Assert_All is new Smart_Assert.Basic ({{ name }}.T_Le, {{ name }}.Representation.Image);
{% endif %}
{% else %}
   package {{ name }}_Unconstrained_Assert is
      procedure Eq (T1 : in Unconstrained; T2 : in Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in Unconstrained; T2 : in Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_Unconstrained_Assert;

{% if endianness in ["either", "big"] %}
   package {{ name }}_T_Unconstrained_Assert is
      procedure Eq (T1 : in T_Unconstrained; T2 : in T_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T_Unconstrained; T2 : in T_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_T_Unconstrained_Assert;

{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_T_Le_Unconstrained_Assert is
      procedure Eq (T1 : in T_Le_Unconstrained; T2 : in T_Le_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
      procedure Neq (T1 : in T_Le_Unconstrained; T2 : in T_Le_Unconstrained; Epsilon : in Long_Float := 0.0; Message : in String := ""; Filename : in String := Sinfo.File; Line : in Natural := Sinfo.Line);
   end {{ name }}_T_Le_Unconstrained_Assert;

{% endif %}
   -- This package compares data without any Epsilon, using the
   -- Smart_Assert.Basic package.
   package {{ name }}_Unconstrained_Assert_All is new Smart_Assert.Basic ({{ name }}.Unconstrained, {{ name }}.Representation.Image);
{% if endianness in ["either", "big"] %}
   package {{ name }}_T_Unconstrained_Assert_All is new Smart_Assert.Basic ({{ name }}.T_Unconstrained, {{ name }}.Representation.Image);
{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_T_Le_Unconstrained_Assert_All is new Smart_Assert.Basic ({{ name }}.T_Le_Unconstrained, {{ name }}.Representation.Image);
{% endif %}
{% endif %}
{% else %}
   -- We need this to force an adb to get built and compile without error.
   pragma Elaborate_Body;

   -- Basic assertion packages for packed array:
{% if length %}
   package {{ name }}_U_Assert is new Smart_Assert.Basic ({{ name }}.U, {{ name }}.Representation.Image);
{% if endianness in ["either", "big"] %}
   package {{ name }}_Assert is new Smart_Assert.Basic ({{ name }}.T, {{ name }}.Representation.Image);
{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_Le_Assert is new Smart_Assert.Basic ({{ name }}.T_Le, {{ name }}.Representation.Image);
{% endif %}
{% else %}
   package {{ name }}_Unconstrained_Assert is new Smart_Assert.Basic ({{ name }}.Unconstrained, {{ name }}.Representation.Image);
{% if endianness in ["either", "big"] %}
   package {{ name }}_T_Unconstrained_Assert is new Smart_Assert.Basic ({{ name }}.T_Unconstrained, {{ name }}.Representation.Image);
{% endif %}
{% if endianness in ["either", "little"] %}
   package {{ name }}_T_Le_Unconstrained_Assert is new Smart_Assert.Basic ({{ name }}.T_Le_Unconstrained, {{ name }}.Representation.Image);
{% endif %}
{% endif %}
{% endif %}

   -- Specialized smart assert package for the element in this array type:
{% if element.is_packed_type %}
   package Element_Assertion renames {{ element.type_package }}.Assertion;
   package Element_Assert renames Element_Assertion.{{ element.type_package }}_Assert;
{% elif element.is_enum %}
   package Element_Assertion renames {{ element.type_package }}.Assertion;
   package Element_Assert renames Element_Assertion.{{ element.type_model.name }}_Assert;
{% elif element.has_float %}
   package Element_Assert is new Smart_Assert.Float ({{ element.type }}, {{ name }}.Representation.Element_Image);
{% else %}
   package Element_Assert is new Smart_Assert.Basic ({{ element.type }}, {{ name }}.Representation.Element_Image);
{% endif %}

end {{ name }}.Assertion;
