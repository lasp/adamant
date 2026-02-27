--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if unpacked_types %}
with Byte_Array_Util;

{% endif %}
{% if packed_type_includes %}
-- Record Field Includes:
{% for include in packed_type_includes %}
with {{ include }}.Validation;
{% endfor %}

{% endif %}
package body {{ name }}.Validation is

{% if endianness in ["either", "big"] %}
   pragma Warnings (Off, "formal parameter ""r"" is not referenced");
   function Valid (R : in T; Errant_Field : out Interfaces.Unsigned_32) return Boolean is
   pragma Warnings (On, "formal parameter ""r"" is not referenced");
{% for include in variable_length_type_includes %}
{% if include not in ["Interfaces"] %}
      use {{ include }};
      pragma Annotate (GNATSAS, Intentional, "unused entity", "It is OK if this is unused, including it makes the generator simpler");
{% endif %}
{% endfor %}
{% if packed_type_includes %}
      E_Field : Interfaces.Unsigned_32;
{% endif %}
{% if variable_length_fields %}
      -- The variable length of the serialized type.
      Variable_Length : Integer;
{% endif %}
   begin
      -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
      -- i.e. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
      -- is OK and we want the compiler to delete this code, so ignore the warning.
      pragma Warnings (Off, "this code can never be executed and has been deleted");
      pragma Warnings (Off, "redundant conversion, expression is of type ""Natural""");
      pragma Warnings (Off, "condition can only be True if invalid values present");

{% if variable_length %}
      --
      -- First let's check lengths and make sure they are valid:
      --

{% for field in variable_length_fields.values() %}
      -- Make sure sizing field for the variable length field is valid for its type.
      if not R.{{ field.variable_length }}'Valid then
         Errant_Field := {{ field.variable_length_field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
         return False;
      end if;

      -- Check length for variable length field {{ field.name }} and make sure it
      -- is not bigger than the array.
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > R.{{ field.name }}'Length then
         Errant_Field := {{ field.variable_length_field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "since this field is already 'Valid, it may be not too large by definition");
         return False;
      end if;

{% endfor %}
{% endif %}
      --
      -- Check individual fields:
      --

{% for field in fields.values() %}
      -- Check {{ field.name }}:
{% if field.skip_validation %}
      -- Validation turned off for {{ field.name }}.
{% else %}
{% if field.format %}
{% if field.format.length %}
{% if field.variable_length %}
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > 0 then
         for E of R.{{ field.name }} (R.{{ field.name }}'First .. R.{{ field.name }}'First + Variable_Length - 1) loop
            if not E'Valid then
               Errant_Field := {{ field.start_field_number }};
               pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
               return False;
            end if;
         end loop;
      end if;
{% else %}
      for E of R.{{ field.name }} loop
         if not E'Valid then
            Errant_Field := {{ field.start_field_number }};
            pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
            return False;
         end if;
      end loop;
{% endif %}
{% else %}
{% if field.name not in variable_length_sizing_fields.keys() %}
      if not R.{{ field.name }}'Valid then
         Errant_Field := {{ field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
         return False;
      end if;
{% else %}
      -- Checked above ^
{% endif %}
{% endif %}
{% else %}
{% if field.variable_length %}
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > 0 then
         if not {{ field.type_package }}.Validation.Valid (R.{{ field.name }}, E_Field, R.{{ field.name }}'First, R.{{ field.name }}'First + Variable_Length - 1) then
            Errant_Field := {{ field.start_field_number - 1 }} + E_Field;
            return False;
         end if;
      end if;
{% else %}
      if not {{ field.type_package }}.Validation.Valid (R.{{ field.name }}, E_Field) then
         Errant_Field := {{ field.start_field_number - 1 }} + E_Field;
         return False;
      end if;
{% endif %}
{% endif %}
{% endif %}

{% endfor %}
      -- Re-enable warning.
      pragma Warnings (On, "redundant conversion, expression is of type ""Natural""");
      pragma Warnings (On, "this code can never be executed and has been deleted");
      pragma Warnings (On, "condition can only be True if invalid values present");

      -- Everything checks out:
      Errant_Field := 0;
      return True;
   exception
      -- From: http://www.adaic.org/resources/add_content/standards/05aarm/html/AA-13-9-2.html
      -- The Valid attribute may be used to check the result of calling an
      -- instance of Unchecked_Conversion (or any other operation that can
      -- return invalid values). However, an exception handler should also
      -- be provided because implementations are permitted to raise
      -- Constraint_Error or Program_Error if they detect the use of an
      -- invalid representation (see 13.9.1).
      when Constraint_Error =>
         Errant_Field := 0;
         return False;
      when Program_Error =>
         Errant_Field := 0;
         return False;
   end Valid;

{% endif %}
{% if endianness in ["either", "little"] %}
   pragma Warnings (Off, "formal parameter ""r"" is not referenced");
   function Valid (R : in T_Le; Errant_Field : out Interfaces.Unsigned_32) return Boolean is
   pragma Warnings (On, "formal parameter ""r"" is not referenced");
{% for include in variable_length_type_includes %}
{% if include not in ["Interfaces"] %}
      use {{ include }};
      pragma Annotate (GNATSAS, Intentional, "unused entity", "It is OK if this is unused, including it makes the generator simpler");
{% endif %}
{% endfor %}
{% if packed_type_includes %}
      E_Field : Interfaces.Unsigned_32;
{% endif %}
{% if variable_length_fields %}
      -- The variable length of the serialized type.
      Variable_Length : Integer;
{% endif %}
   begin
      -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
      -- i.e. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
      -- is OK and we want the compiler to delete this code, so ignore the warning.
      pragma Warnings (Off, "this code can never be executed and has been deleted");
      pragma Warnings (Off, "redundant conversion, expression is of type ""Natural""");
      pragma Warnings (Off, "condition can only be True if invalid values present");

{% if variable_length %}
      --
      -- First let's check lengths and make sure they are valid:
      --

{% for field in variable_length_fields.values() %}
      -- Make sure sizing field for the variable length field is valid for its type.
      if not R.{{ field.variable_length }}'Valid then
         Errant_Field := {{ field.variable_length_field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
         return False;
      end if;

      -- Check length for variable length field {{ field.name }} and make sure it
      -- is not bigger than the array.
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > R.{{ field.name }}'Length then
         Errant_Field := {{ field.variable_length_field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "since this field is already 'Valid, it may be not too large by definition");
         return False;
      end if;

{% endfor %}
{% endif %}
      --
      -- Check individual fields:
      --

{% for field in fields.values() %}
      -- Check {{ field.name }}:
{% if field.skip_validation %}
      -- Validation turned off for {{ field.name }}.
{% else %}
{% if field.format %}
{% if field.format.length %}
{% if field.variable_length %}
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > 0 then
         for E of R.{{ field.name }} (R.{{ field.name }}'First .. R.{{ field.name }}'First + Variable_Length - 1) loop
            if not E'Valid then
               Errant_Field := {{ field.start_field_number }};
               pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
               return False;
            end if;
         end loop;
      end if;
{% else %}
      for E of R.{{ field.name }} loop
         if not E'Valid then
            Errant_Field := {{ field.start_field_number }};
            pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
            return False;
         end if;
      end loop;
{% endif %}
{% else %}
{% if field.name not in variable_length_sizing_fields.keys() %}
      if not R.{{ field.name }}'Valid then
         Errant_Field := {{ field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
         return False;
      end if;
{% else %}
      -- Checked above ^
{% endif %}
{% endif %}
{% else %}
{% if field.variable_length %}
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > 0 then
         if not {{ field.type_package }}.Validation.Valid (R.{{ field.name }}, E_Field, R.{{ field.name }}'First, R.{{ field.name }}'First + Variable_Length - 1) then
            Errant_Field := {{ field.start_field_number - 1 }} + E_Field;
            return False;
         end if;
      end if;
{% else %}
      if not {{ field.type_package }}.Validation.Valid (R.{{ field.name }}, E_Field) then
         Errant_Field := {{ field.start_field_number - 1 }} + E_Field;
         return False;
      end if;
{% endif %}
{% endif %}
{% endif %}

{% endfor %}
      -- Re-enable warning.
      pragma Warnings (On, "redundant conversion, expression is of type ""Natural""");
      pragma Warnings (On, "this code can never be executed and has been deleted");
      pragma Warnings (On, "condition can only be True if invalid values present");

      -- Everything checks out:
      Errant_Field := 0;
      return True;
   exception
      -- From: http://www.adaic.org/resources/add_content/standards/05aarm/html/AA-13-9-2.html
      -- The Valid attribute may be used to check the result of calling an
      -- instance of Unchecked_Conversion (or any other operation that can
      -- return invalid values). However, an exception handler should also
      -- be provided because implementations are permitted to raise
      -- Constraint_Error or Program_Error if they detect the use of an
      -- invalid representation (see 13.9.1).
      when Constraint_Error =>
         Errant_Field := 0;
         return False;
      when Program_Error =>
         Errant_Field := 0;
         return False;
   end Valid;

{% endif %}
   pragma Warnings (Off, "formal parameter ""r"" is not referenced");
   function Valid (R : in U; Errant_Field : out Interfaces.Unsigned_32) return Boolean is
   pragma Warnings (On, "formal parameter ""r"" is not referenced");
{% for include in variable_length_type_includes %}
{% if include not in ["Interfaces"] %}
      use {{ include }};
      pragma Annotate (GNATSAS, Intentional, "unused entity", "It is OK if this is unused, including it makes the generator simpler");
{% endif %}
{% endfor %}
{% if packed_type_includes %}
      E_Field : Interfaces.Unsigned_32;
{% endif %}
{% if variable_length_fields %}
      -- The variable length of the serialized type.
      Variable_Length : Integer;
{% endif %}
   begin
      -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
      -- i.e. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
      -- is OK and we want the compiler to delete this code, so ignore the warning.
      pragma Warnings (Off, "this code can never be executed and has been deleted");
      pragma Warnings (Off, "redundant conversion, expression is of type ""Natural""");
      pragma Warnings (Off, "condition can only be True if invalid values present");

{% if variable_length %}
      --
      -- First let's check lengths and make sure they are valid:
      --

{% for field in variable_length_fields.values() %}
      -- Make sure sizing field for the variable length field is valid for its type.
      if not R.{{ field.variable_length }}'Valid then
         Errant_Field := {{ field.variable_length_field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
         return False;
      end if;

      -- Check length for variable length field {{ field.name }} and make sure it
      -- is not bigger than the array.
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > R.{{ field.name }}'Length then
         Errant_Field := {{ field.variable_length_field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "since this field is already 'Valid, it may be not too large by definition");
         return False;
      end if;

{% endfor %}
{% endif %}
      --
      -- Check individual fields:
      --
{% for field in fields.values() %}
      -- Check {{ field.name }}:
{% if field.skip_validation %}
      -- Validation turned off for {{ field.name }}.
{% else %}
{% if field.format %}
{% if field.format.length %}
{% if field.variable_length %}
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > 0 then
         for E of R.{{ field.name }} (R.{{ field.name }}'First .. R.{{ field.name }}'First + Variable_Length - 1) loop
            if not E'Valid then
               Errant_Field := {{ field.start_field_number }};
               pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
               return False;
            end if;
         end loop;
      end if;
{% else %}
      for E of R.{{ field.name }} loop
         if not E'Valid then
            Errant_Field := {{ field.start_field_number }};
            pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
            return False;
         end if;
      end loop;
{% endif %}
{% else %}
{% if field.name not in variable_length_sizing_fields.keys() %}
      if not R.{{ field.name }}'Valid then
         Errant_Field := {{ field.start_field_number }};
         pragma Annotate (GNATSAS, Intentional, "dead code", "some fields may not be bit-constrained and thus will always be valid");
         return False;
      end if;
{% else %}
      -- Checked above ^
{% endif %}
{% endif %}
{% else %}
{% if field.variable_length %}
      Variable_Length := Integer (R.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }});
      if Variable_Length > 0 then
         if not {{ field.type_package }}.Validation.Valid (R.{{ field.name }} (R.{{ field.name }}'First .. R.{{ field.name }}'First + Variable_Length - 1), E_Field) then
            Errant_Field := {{ field.start_field_number - 1 }} + E_Field;
            return False;
         end if;
      end if;
{% else %}
      if not {{ field.type_package }}.Validation.Valid (R.{{ field.name }}, E_Field) then
         Errant_Field := {{ field.start_field_number - 1 }} + E_Field;
         return False;
      end if;
{% endif %}
{% endif %}
{% endif %}

{% endfor %}
      -- Re-enable warning.
      pragma Warnings (On, "redundant conversion, expression is of type ""Natural""");
      pragma Warnings (On, "this code can never be executed and has been deleted");
      pragma Warnings (On, "condition can only be True if invalid values present");

      -- Everything checks out:
      Errant_Field := 0;
      return True;
   exception
      -- From: http://www.adaic.org/resources/add_content/standards/05aarm/html/AA-13-9-2.html
      -- The Valid attribute may be used to check the result of calling an
      -- instance of Unchecked_Conversion (or any other operation that can
      -- return invalid values). However, an exception handler should also
      -- be provided because implementations are permitted to raise
      -- Constraint_Error or Program_Error if they detect the use of an
      -- invalid representation (see 13.9.1).
      when Constraint_Error =>
         Errant_Field := 0;
         return False;
      when Program_Error =>
         Errant_Field := 0;
         return False;
   end Valid;

{% if endianness in ["either", "big"] %}
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if unpacked_types %}
      use Byte_Array_Util;
{% endif %}
      To_Return : Basic_Types.Poly_Type := [others => 0];
   begin
      case Field is
{% for field in fields.values() %}
{% if field.is_packed_type %}
         when {{ field.start_field_number }} .. {{ field.end_field_number }} =>
            To_Return := {{ field.type_package }}.Validation.Get_Field (Src.{{ field.name }}, Field - {{ field.start_field_number + 1 }});
{% else %}
         when {{ field.start_field_number }} =>
            declare
               -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
               -- and we know this, so suppress any checks by the compiler for this copy.
               pragma Suppress (Range_Check);
               pragma Suppress (Overflow_Check);
               Var : constant {{ field.type }} := Src.{{ field.name }};
               pragma Unsuppress (Range_Check);
               pragma Unsuppress (Overflow_Check);
               -- Now overlay the var with a byte array before copying it into the polytype.
{% if field.type in ["Basic_Types.Byte", "Byte"] %}
               subtype Byte_Array is Basic_Types.Byte_Array (0 .. 0);
{% else %}
               subtype Byte_Array is Basic_Types.Byte_Array (0 .. {{ field.type }}'Object_Size / Basic_Types.Byte'Object_Size - 1);
{% endif %}
               pragma Warnings (Off, "overlay changes scalar storage order");
               Overlay : constant Byte_Array with Import, Convention => Ada, Address => Var'Address;
               pragma Warnings (On, "overlay changes scalar storage order");
            begin
               Safe_Right_Copy (To_Return, Overlay);
            end;
{% endif %}
{% endfor %}
         when others => null;
      end case;
      return To_Return;
   exception
      -- We are just trying to do our best here. So if a constraint error is thrown during this process,
      -- we don't want to die.
      when Constraint_Error =>
         return To_Return;
   end Get_Field;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Get_Field (Src : in T_Le; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if unpacked_types %}
      use Byte_Array_Util;
{% endif %}
      To_Return : Basic_Types.Poly_Type := [others => 0];
   begin
      case Field is
{% for field in fields.values() %}
{% if field.is_packed_type %}
         when {{ field.start_field_number }} .. {{ field.end_field_number }} =>
            To_Return := {{ field.type_package }}.Validation.Get_Field (Src.{{ field.name }}, Field - {{ field.start_field_number + 1 }});
{% else %}
         when {{ field.start_field_number }} =>
            declare
               -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
               -- and we know this, so suppress any checks by the compiler for this copy.
               pragma Suppress (Range_Check);
               pragma Suppress (Overflow_Check);
               Var : constant {{ field.type }} := Src.{{ field.name }};
               pragma Unsuppress (Range_Check);
               pragma Unsuppress (Overflow_Check);
               -- Now overlay the var with a byte array before copying it into the polytype.
{% if field.type in ["Basic_Types.Byte", "Byte"] %}
               subtype Byte_Array is Basic_Types.Byte_Array (0 .. 0);
{% else %}
               subtype Byte_Array is Basic_Types.Byte_Array (0 .. {{ field.type }}'Object_Size / Basic_Types.Byte'Object_Size - 1);
{% endif %}
               pragma Warnings (Off, "overlay changes scalar storage order");
               Overlay : constant Byte_Array with Import, Convention => Ada, Address => Var'Address;
               pragma Warnings (On, "overlay changes scalar storage order");
            begin
               Safe_Right_Copy (To_Return, Overlay);
            end;
{% endif %}
{% endfor %}
         when others => null;
      end case;
      return To_Return;
   exception
      -- We are just trying to do our best here. So if a constraint error is thrown during this process,
      -- we don't want to die.
      when Constraint_Error =>
         return To_Return;
   end Get_Field;

{% endif %}
end {{ name }}.Validation;
