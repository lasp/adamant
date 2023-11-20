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
   pragma Compile_Time_Error (Interfaces.Unsigned_32 ({{ num_fields }}) > Interfaces.Unsigned_32'Last,
      "The autocoded validation functions assume a field size that can fit in 32-bits. This array's field size is too large. Please write this file by hand.");

{% if is_volatile_type %}
   -- Validation not supported for volatile record. Convert to a regular record for
   -- a validation checking function.
   procedure Dummy_Valid;
{% else %}
   -- Return True if the packed array is valid. The function performs
   -- range checks on all the fields of the array. If an element is invalid,
   -- False is returned. The errant_Field parameter is always 0.
   function Valid (R : in Unconstrained; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
   -- ^ "Unpacked" can be passed into this function as well because it is a subtype of Unconstrained
   function Valid (R : in T; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
   function Valid (R : in T_Le; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
{% endif %}

end {{ name }}.Validation;
