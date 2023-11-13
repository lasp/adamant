--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Validation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if not is_volatile_type %}
{% if packed_type_includes %}
-- Record Field Includes:
{% for include in packed_type_includes %}
with {{ include }}.Validation;
{% endfor %}

{% endif %}
{% endif %}
package body {{ name }}.Validation is

{% if is_volatile_type %}
   -- Validation not supported for volatile record. Convert to a regular record for
   -- a validation checking function.
   procedure Dummy_Valid is
   begin
      null;
   end Dummy_Valid;
{% else %}
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
               pragma Annotate (CodePeer, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
               return False;
            end if;
         end loop;
{% else %}
         if not R (Idx)'Valid then
            Errant_Field := Count + 1;
            pragma Annotate (CodePeer, Intentional, "dead code", "some array elements may not be bit-constrained and thus will always be valid");
            return False;
         end if;
{% endif %}
{% endif %}
         Count := Count + 1;
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

   function Valid (R : in T; Errant_Field : out Unsigned_32) return Boolean is
   begin
      return Valid (Unconstrained (R), Errant_Field);
   end Valid;

   function Valid (R : in T_Le; Errant_Field : out Unsigned_32) return Boolean is
   begin
      return Valid (Unconstrained (R), Errant_Field);
   end Valid;
{% endif %}

end {{ name }}.Validation;
