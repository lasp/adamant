--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Validation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if "Interfaces" not in includes %}

with Interfaces;
{% endif %}

-- Record validation package for {{ name }}
package {{ name }}.Validation is

   -- We assume that for this autocoder to work, the "field" number for the
   -- packed array takes less than 32-bits to represent:
   use Interfaces;
   pragma Compile_Time_Error ({{ num_fields }} > Interfaces.Unsigned_32'Last,
      "The autocoded validation functions assume a field size that can fit in 32-bits. This record's field size is too large. Please write this file by hand.");

{% if is_volatile_type %}
   -- Validation not supported for volatile record. Convert to a regular record for
   -- a validation checking function.
   procedure Dummy_Valid;
{% else %}
   -- Return True if the packed record is valid. The function performs
   -- range checks on all the fields of the record. If a field is invalid,
   -- False is returned and the errant_Field parameter is filled in with a
   -- Natural specifying which field was out of range.
   function Valid (R : in T; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
   function Valid (R : in T_Le; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
   function Valid (R : in U; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
{% endif %}

end {{ name }}.Validation;
