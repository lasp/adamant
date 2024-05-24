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

   -- Return True if the packed record is valid. The function performs
   -- range checks on all the fields of the record. If a field is invalid,
   -- False is returned and the errant_Field parameter is filled in with a
   -- Natural specifying which field was out of range.
   function Valid (R : in U; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
{% if endianness in ["either", "big"] %}
   function Valid (R : in T; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Valid (R : in T_Le; Errant_Field : out Interfaces.Unsigned_32) return Boolean;
{% endif %}

   -- Return a field (provided by a field number) as a polymorphic type.
   -- This is useful for returning any field in a record in a very generic
   -- way. Fields bigger than the polymorphic type will only have their
   -- least significant bits returned. This function should be used in tandem
   -- with the Valid functions above to create useful error messages for an invalid
   -- type:
{% if endianness in ["either", "big"] %}
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Get_Field (Src : in T_Le; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type;
{% endif %}

end {{ name }}.Validation;
