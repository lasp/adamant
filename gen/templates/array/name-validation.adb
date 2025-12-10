--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Validation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if length %}
{% if unpacked_types %}

-- Standard includes:
with Byte_Array_Util;
{% endif %}

{% endif %}
{% if packed_type_includes %}
-- Record Field Includes:
{% for include in packed_type_includes %}
with {{ include }}.Validation;
{% endfor %}

{% endif %}
package body {{ name }}.Validation is
{% if not length %}

   use Interfaces;
{% endif %}

   function Valid (R : in Unconstrained; Errant_Field : out Unsigned_32) return Boolean is
{% if packed_type_includes %}
      E_Field : Interfaces.Unsigned_32;
{% endif %}
      Count : Interfaces.Unsigned_32 := 0;
   begin
      -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
      -- ie. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
      -- is OK and we want the compiler to delete this code, so ignore the warning.
      pragma Warnings (Off, "this code can never be executed and has been deleted");

      -- Check each element:
{% if element.skip_validation %}
      -- Validation turned off for this element type.
{% else %}
      for Idx in R'Range loop
{% if element.is_packed_type %}
         if not {{ element.type_package }}.Validation.Valid (R (Idx), E_Field) then
            Errant_Field := Count * {{ element.type_model.num_fields }} + E_Field;
            return False;
         end if;
{% else %}
{% if element.format.length %}
         for Jdx in R (Idx)'Range loop
            if not R (Idx)(Jdx)'Valid then
               Errant_Field := Count + 1;
               pragma Annotate (GNATSAS, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
               return False;
            end if;
         end loop;
{% else %}
         if not R (Idx)'Valid then
            Errant_Field := Count + 1;
            pragma Annotate (GNATSAS, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
            return False;
         end if;
{% endif %}
{% endif %}
         Count := @ + 1;
      end loop;
{% endif %}

      -- Re-enable warning.
      pragma Warnings (On, "this code can never be executed and has been deleted");

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

{% if length %}
{% if endianness in ["either", "big"] %}
   function Valid (
      R : in T;
      Errant_Field : out Unsigned_32;
      First_Index : in Unconstrained_Index_Type := T'First;
      Last_Index : in Unconstrained_Index_Type := T'Last
   ) return Boolean is
{% if packed_type_includes %}
      E_Field : Interfaces.Unsigned_32;
{% endif %}
      Count : Interfaces.Unsigned_32 := 0;
   begin
      -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
      -- ie. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
      -- is OK and we want the compiler to delete this code, so ignore the warning.
      pragma Warnings (Off, "this code can never be executed and has been deleted");

      -- Check each element:
{% if element.skip_validation %}
      -- Validation turned off for this element type.
{% else %}
      for Idx in First_Index .. Last_Index loop
{% if element.is_packed_type %}
         if not {{ element.type_package }}.Validation.Valid (R (Idx), E_Field) then
            Errant_Field := Count * {{ element.type_model.num_fields }} + E_Field;
            return False;
         end if;
{% else %}
{% if element.format.length %}
         for Jdx in R (Idx)'Range loop
            if not R (Idx)(Jdx)'Valid then
               Errant_Field := Count + 1;
               pragma Annotate (GNATSAS, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
               return False;
            end if;
         end loop;
{% else %}
         if not R (Idx)'Valid then
            Errant_Field := Count + 1;
            pragma Annotate (GNATSAS, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
            return False;
         end if;
{% endif %}
{% endif %}
         Count := @ + 1;
      end loop;
{% endif %}

      -- Re-enable warning.
      pragma Warnings (On, "this code can never be executed and has been deleted");

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
   function Valid (
      R : in T_Le;
      Errant_Field : out Unsigned_32;
      First_Index : in Unconstrained_Index_Type := T_Le'First;
      Last_Index : in Unconstrained_Index_Type := T_Le'Last
   ) return Boolean is
{% if packed_type_includes %}
      E_Field : Interfaces.Unsigned_32;
{% endif %}
      Count : Interfaces.Unsigned_32 := 0;
   begin
      -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
      -- ie. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
      -- is OK and we want the compiler to delete this code, so ignore the warning.
      pragma Warnings (Off, "this code can never be executed and has been deleted");

      -- Check each element:
{% if element.skip_validation %}
      -- Validation turned off for this element type.
{% else %}
      for Idx in First_Index .. Last_Index loop
{% if element.is_packed_type %}
         if not {{ element.type_package }}.Validation.Valid (R (Idx), E_Field) then
            Errant_Field := Count * {{ element.type_model.num_fields }} + E_Field;
            return False;
         end if;
{% else %}
{% if element.format.length %}
         for Jdx in R (Idx)'Range loop
            if not R (Idx)(Jdx)'Valid then
               Errant_Field := Count + 1;
               pragma Annotate (GNATSAS, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
               return False;
            end if;
         end loop;
{% else %}
         if not R (Idx)'Valid then
            Errant_Field := Count + 1;
            pragma Annotate (GNATSAS, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
            return False;
         end if;
{% endif %}
{% endif %}
         Count := @ + 1;
      end loop;
{% endif %}

      -- Re-enable warning.
      pragma Warnings (On, "this code can never be executed and has been deleted");

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
{% if endianness in ["either", "big"] %}
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if element.is_packed_type %}
      Idx : constant Constrained_Index_Type := Constrained_Index_Type'First + Unconstrained_Index_Type (Field / {{ element.type_model.num_fields }});
      Remainder : Unsigned_32 := 0;
      To_Return : Basic_Types.Poly_Type;
{% else %}
      use Byte_Array_Util;
      To_Return : Basic_Types.Poly_Type := [others => 0];
{% endif %}
   begin
{% if element.is_packed_type %}
      if Field > 0 then
         Remainder := ((Field - 1) mod {{ element.type_model.num_fields }}) + 1;
      end if;
      To_Return := {{ element.type_package }}.Validation.Get_Field (Src (Idx), Remainder);
{% else %}
      declare
         -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
         -- and we know this, so suppresss any checks by the compiler for this copy.
         pragma Suppress (Range_Check);
         pragma Suppress (Overflow_Check);
         Var : constant {{ element.type }} := Src (Src'First + Unconstrained_Index_Type (Field) - 1);
         pragma Unsuppress (Range_Check);
         pragma Unsuppress (Overflow_Check);
         -- Now overlay the var with a byte array before copying it into the polytype.
{% if element.type in ["Basic_Types.Byte", "Byte"] %}
         subtype Byte_Array is Basic_Types.Byte_Array (0 .. 0);
{% else %}
         subtype Byte_Array is Basic_Types.Byte_Array (0 .. {{ element.type }}'Object_Size / Basic_Types.Byte'Object_Size - 1);
{% endif %}
         pragma Warnings (Off, "overlay changes scalar storage order");
         Overlay : constant Byte_Array with Import, Convention => Ada, Address => Var'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         Safe_Right_Copy (To_Return, Overlay);
      end;
{% endif %}
      return To_Return;
   exception
      when Constraint_Error =>
         return To_Return;
   end Get_Field;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Get_Field (Src : in T_Le; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if element.is_packed_type %}
      Idx : constant Constrained_Index_Type := Constrained_Index_Type'First + Unconstrained_Index_Type (Field / {{ element.type_model.num_fields }});
      Remainder : Unsigned_32 := 0;
      To_Return : Basic_Types.Poly_Type;
{% else %}
      use Byte_Array_Util;
      To_Return : Basic_Types.Poly_Type := [others => 0];
{% endif %}
   begin
{% if element.is_packed_type %}
      if Field > 0 then
         Remainder := ((Field - 1) mod {{ element.type_model.num_fields }}) + 1;
      end if;
      To_Return := {{ element.type_package }}.Validation.Get_Field (Src (Idx), Remainder);
{% else %}
      declare
         -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
         -- and we know this, so suppresss any checks by the compiler for this copy.
         pragma Suppress (Range_Check);
         pragma Suppress (Overflow_Check);
         Var : constant {{ element.type }} := Src (Src'First + Unconstrained_Index_Type (Field) - 1);
         pragma Unsuppress (Range_Check);
         pragma Unsuppress (Overflow_Check);
         -- Now overlay the var with a byte array before copying it into the polytype.
{% if element.type in ["Basic_Types.Byte", "Byte"] %}
         subtype Byte_Array is Basic_Types.Byte_Array (0 .. 0);
{% else %}
         subtype Byte_Array is Basic_Types.Byte_Array (0 .. {{ element.type }}'Object_Size / Basic_Types.Byte'Object_Size - 1);
{% endif %}
         pragma Warnings (Off, "overlay changes scalar storage order");
         Overlay : constant Byte_Array with Import, Convention => Ada, Address => Var'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         Safe_Right_Copy (To_Return, Overlay);
      end;
{% endif %}
      return To_Return;
   exception
      when Constraint_Error =>
         return To_Return;
   end Get_Field;

{% endif %}
{% endif %}
end {{ name }}.Validation;
