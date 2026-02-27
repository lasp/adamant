--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Validation Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if "Interfaces" not in includes %}

with Interfaces;
{% endif %}
{% if "Basic_Types" not in includes %}

with Basic_Types;
{% endif %}

-- Record validation package for {{ name }}
package {{ name }}.Validation is

   -- We assume that for this autocoder to work, the "field" number for the
   -- packed array takes less than 32-bits to represent:
   use Interfaces;
   pragma Compile_Time_Error (Interfaces.Unsigned_32 ({{ num_fields }}) > Interfaces.Unsigned_32'Last,
      "The autocoded validation functions assume a field size that can fit in 32-bits. This array's field size is too large. Please write this file by hand.");

   -- Return True if the packed array is valid. The function performs
   -- range checks on all the fields of the array. If an element is invalid,
   -- False is returned. The Errant_Field parameter is always 0.
   function Valid (R : in Unconstrained; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
   -- ^ "Unpacked" can be passed into this function as well because it is a subtype of Unconstrained
{% if endianness in ["either", "big"] %}
   -- Valid function for the T type. Optionally, a first or last index can be passed in to only check
   -- a portion of the array, otherwise the entire array is checked for validity.
   function Valid (
      R : in T;
      Errant_Field : out Unsigned_32;
      First_Index : in Unconstrained_Index_Type := T'First;
      Last_Index : in Unconstrained_Index_Type := T'Last
   ) return Boolean;
{% endif %}
{% if endianness in ["either", "little"] %}
   -- Valid function for the T_Le type. Optionally, a first or last index can be passed in to only check
   -- a portion of the array, otherwise the entire array is checked for validity.
   function Valid (
      R : in T_Le;
      Errant_Field : out Unsigned_32;
      First_Index : in Unconstrained_Index_Type := T_Le'First;
      Last_Index : in Unconstrained_Index_Type := T_Le'Last
   ) return Boolean;
{% endif %}

   -- Return a field (provided by a field number) as a polymorphic type.
   -- This is useful for returning any field in an array in a very generic
   -- way. Fields bigger than the polymorphic type will only have their
   -- least significant bits returned. This function should be used in tandem
   -- with the validation functions above to create useful error messages for an invalid
   -- type:
{% if endianness in ["either", "big"] %}
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Get_Field (Src : in T_Le; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type;
{% endif %}

end {{ name }}.Validation;
