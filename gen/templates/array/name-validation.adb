--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Validation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if unpacked_types %}
-- Standard includes:
with Byte_Array_Util;

{% endif %}
-- For runtime alignment checks on Bytes / Aligned addresses:
with System.Storage_Elements;
{% if packed_type_includes %}
-- Record Field Includes:
{% for include in packed_type_includes %}
with {{ include }}.Validation;
{% endfor %}

{% endif %}
package body {{ name }}.Validation is

   -- Packed arrays (unlike packed records) are not allowed to have
   -- Alignment => 1 unless their component size is 1, so GNAT picks
   -- a non-trivial T'Alignment for most cases. Overlaying T on a
   -- Byte_Array (natural alignment 1) at a runtime address that
   -- doesn't satisfy T'Alignment is erroneous per Ada 2022 RM
   -- 13.3(13/3):
   --
   --   "If an Address is specified, it is the programmer's
   --   responsibility to ensure that the address is valid and
   --   appropriate for the entity and its use; otherwise, program
   --   execution is erroneous."
   --   (http://www.ada-auth.org/standards/22rm/html/RM-13-3.html)
   --
   -- In practice this manifests as silent garbage on x86 hosts (GNAT
   -- folds the misalignment) and as traps or garbage reads on
   -- alignment-strict bareboard targets.
   --
   -- The autocode below picks one of three patterns depending on the
   -- element shape (computed in gen/models/array.py as
   -- `required_alignment`):
   --
   --   * Packed-record element (T'Alignment = 1) -- direct overlay,
   --     no copy needed; any byte address satisfies the requirement.
   --
   --   * Byte-aligned primitive (Short_Float, Unsigned_32, ...) --
   --     fast path overlays directly when Bytes' runtime address
   --     already satisfies T'Alignment, slow path copies into an
   --     aligned local Byte_Array and overlays on the copy.
   --
   --   * Sub-byte primitive (e.g. T'Component_Size = 10) -- always
   --     copy into the aligned local.
   --
   -- We assume that the required alignment computed in the model
   -- ({{ required_alignment }}) is compatible with the alignment chosen by
   -- the compiler for T (and T_Le) -- i.e. that T'Alignment divides
   -- {{ required_alignment }}. We verify that here with a `pragma
   -- Compile_Time_Error`. We know that T'Alignment must be a power
   -- of 2 via the GNAT Reference Manual section 10.1 (Alignment
   -- Clauses): "GNAT requires that all alignment clauses specify 0
   -- or a power of 2, and all default alignments are always a power
   -- of 2." We also know that {{ required_alignment }} is a power of 2
   -- via an assertion in gen/models/array.py. Because both sides are
   -- powers of 2, "T'Alignment <= {{ required_alignment }}" is equivalent to
   -- "{{ required_alignment }} is a multiple of T'Alignment" -- i.e. our
   -- aligned local copies satisfy T's alignment requirement and the
   -- typed overlays in the helpers below are non-erroneous per RM
   -- 13.3(13/3). The runtime checks below compare against the literal
   -- rather than T'Alignment: it is a known multiple of T'Alignment, so
   -- the check is at least as strict, and it is a compile-time constant.
{% if endianness in ["either", "big"] %}
   pragma Compile_Time_Error (
      T'Alignment > {{ required_alignment }},
      "T'Alignment > static literal ({{ required_alignment }}); update gen/models/array.py"
   );
{% endif %}
{% if endianness in ["either", "little"] %}
   pragma Compile_Time_Error (
      T_Le'Alignment > {{ required_alignment }},
      "T_Le'Alignment > static literal ({{ required_alignment }}); update gen/models/array.py"
   );
{% endif %}

{% if endianness in ["either", "big"] %}
   function Valid (
      Bytes : in Serialization.Byte_Array;
      Errant_Field : out Unsigned_32;
      First_Index : in Unconstrained_Index_Type := T'First;
      Last_Index : in Unconstrained_Index_Type := T'Last
   ) return Boolean is
      use System.Storage_Elements;

      -- The helper takes a Byte_Array which has no representation
      -- constraints and overlays T on it locally with an exception
      -- handler per RM 13.9.2. We walk the elements through R and
      -- set Errant_Field on the first invalid one.
      function Valid_Through (Aligned : in Serialization.Byte_Array) return Boolean is
{% if required_alignment > 1 %}
         -- Alignment of Aligned is made compatible by the caller.
         pragma Assert (
            To_Integer (Aligned'Address) mod {{ required_alignment }} = 0,
            "Valid_Through: caller passed buffer not aligned to {{ required_alignment }}"
         );
{% endif %}

         -- Overlay T. We know alignment is good so ignore warnings.
         pragma Warnings (Off, "specified address*may be inconsistent with alignment");
         pragma Warnings (Off, "program execution may be erroneous");
         R : T with Import, Convention => Ada, Address => Aligned'Address;
         pragma Warnings (On, "program execution may be erroneous");
         pragma Warnings (On, "specified address*may be inconsistent with alignment");
{% if packed_type_includes %}
         E_Field : Interfaces.Unsigned_32;
{% endif %}
         Count : Interfaces.Unsigned_32 := 0;
      begin
         -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
         -- i.e. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
         -- is OK and we want the compiler to delete this code, so ignore the warning.
         pragma Warnings (Off, "this code can never be executed and has been deleted");

         -- Check each element:
{% if element.skip_validation %}
         -- Validation turned off for this element type.
{% elif element.is_packed_type %}
         -- Only iterate if the element type can actually be invalid.
         if not {{ element.type_package }}.Always_Valid then
            for Idx in First_Index .. Last_Index loop
               declare
                  Elem_Bytes : {{ element.type_package }}.Serialization.Byte_Array
                     with Import, Convention => Ada, Address => R (Idx)'Address;
               begin
                  if not {{ element.type_package }}.Validation.Valid (Elem_Bytes, E_Field) then
                     Errant_Field := Count * {{ element.type_model.num_fields }} + E_Field;
                     return False;
                  end if;
               end;
               Count := @ + 1;
            end loop;
         end if;
{% else %}
         for Idx in First_Index .. Last_Index loop
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
         -- invalid representation (see 13.9.1). The handler lives in the
         -- helper because that's where the per-element 'Valid reads (the
         -- only realistic raise sites) are.
         when Constraint_Error =>
            Errant_Field := 0;
            return False;
         when Program_Error =>
            Errant_Field := 0;
            return False;
      end Valid_Through;
   begin
{% if required_alignment == 1 %}
      -- Since T has an alignment of 1, this is always safe.
      return Valid_Through (Bytes);
{% else %}
      -- If alignment of bytes is already compatible, then we are safe to
      -- validate right now.
      if To_Integer (Bytes'Address) mod {{ required_alignment }} = 0 then
         return Valid_Through (Bytes);
      else
         -- The alignment of bytes is not compatible so we need to
         -- copy bytes to an aligned local copy before validating
         -- to avoid erroneous behavior.
         declare
            Aligned_Bytes : constant Serialization.Byte_Array := Bytes
               with Alignment => {{ required_alignment }};
         begin
            return Valid_Through (Aligned_Bytes);
         end;
      end if;
{% endif %}
   end Valid;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Valid_Le (
      Bytes : in Serialization_Le.Byte_Array;
      Errant_Field : out Unsigned_32;
      First_Index : in Unconstrained_Index_Type := T_Le'First;
      Last_Index : in Unconstrained_Index_Type := T_Le'Last
   ) return Boolean is
      use System.Storage_Elements;

      -- The helper takes a Byte_Array which has no representation
      -- constraints and overlays T on it locally with an exception
      -- handler per RM 13.9.2. We walk the elements through R and
      -- set Errant_Field on the first invalid one.
      function Valid_Le_Through (Aligned : in Serialization_Le.Byte_Array) return Boolean is
{% if required_alignment > 1 %}
         -- Alignment of Aligned is made compatible by the caller.
         pragma Assert (
            To_Integer (Aligned'Address) mod {{ required_alignment }} = 0,
            "Valid_Le_Through: caller passed buffer not aligned to {{ required_alignment }}"
         );
{% endif %}

         -- Overlay T. We know alignment is good so ignore warnings.
         pragma Warnings (Off, "specified address*may be inconsistent with alignment");
         pragma Warnings (Off, "program execution may be erroneous");
         R : T_Le with Import, Convention => Ada, Address => Aligned'Address;
         pragma Warnings (On, "program execution may be erroneous");
         pragma Warnings (On, "specified address*may be inconsistent with alignment");
{% if packed_type_includes %}
         E_Field : Interfaces.Unsigned_32;
{% endif %}
         Count : Interfaces.Unsigned_32 := 0;
      begin
         -- Sometimes the valid functions below will NEVER be false, since the type can never be out of range,
         -- i.e. with an Unsigned_16. If this is the case Ada warns that some code can never be executed. This
         -- is OK and we want the compiler to delete this code, so ignore the warning.
         pragma Warnings (Off, "this code can never be executed and has been deleted");

         -- Check each element:
{% if element.skip_validation %}
         -- Validation turned off for this element type.
{% elif element.is_packed_type %}
         -- Only iterate if the element type can actually be invalid.
         if not {{ element.type_package }}.Always_Valid then
            for Idx in First_Index .. Last_Index loop
               declare
                  Elem_Bytes : {{ element.type_package }}.Serialization_Le.Byte_Array
                     with Import, Convention => Ada, Address => R (Idx)'Address;
               begin
                  if not {{ element.type_package }}.Validation.Valid_Le (Elem_Bytes, E_Field) then
                     Errant_Field := Count * {{ element.type_model.num_fields }} + E_Field;
                     return False;
                  end if;
               end;
               Count := @ + 1;
            end loop;
         end if;
{% else %}
         for Idx in First_Index .. Last_Index loop
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
         -- invalid representation (see 13.9.1). The handler lives in the
         -- helper because that's where the per-element 'Valid reads (the
         -- only realistic raise sites) are.
         when Constraint_Error =>
            Errant_Field := 0;
            return False;
         when Program_Error =>
            Errant_Field := 0;
            return False;
      end Valid_Le_Through;
   begin
{% if required_alignment == 1 %}
      -- Since T_Le has an alignment of 1, this is always safe.
      return Valid_Le_Through (Bytes);
{% else %}
      -- If alignment of bytes is already compatible, then we are safe to
      -- validate right now.
      if To_Integer (Bytes'Address) mod {{ required_alignment }} = 0 then
         return Valid_Le_Through (Bytes);
      else
         -- The alignment of bytes is not compatible so we need to
         -- copy bytes to an aligned local copy before validating
         -- to avoid erroneous behavior.
         declare
            Aligned_Bytes : constant Serialization_Le.Byte_Array := Bytes
               with Alignment => {{ required_alignment }};
         begin
            return Valid_Le_Through (Aligned_Bytes);
         end;
      end if;
{% endif %}
   end Valid_Le;

{% endif %}
{% if endianness in ["either", "big"] %}
   function Get_Field (Bytes : in Serialization.Byte_Array; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if element.is_packed_type %}
      Idx : constant Constrained_Index_Type := Constrained_Index_Type'First + Unconstrained_Index_Type ((Field - 1) / {{ element.type_model.num_fields }});
      Remainder : Unsigned_32 := 0;
      To_Return : Basic_Types.Poly_Type;
      Element_Stride_Bytes : constant Natural := {{ element.type_package }}.Size_In_Bytes;
      Slice_Start : constant Natural := Bytes'First + Natural (Idx - Constrained_Index_Type'First) * Element_Stride_Bytes;
{% elif (element.size % 8) == 0 %}
      use Byte_Array_Util;
      To_Return : Basic_Types.Poly_Type := [others => 0];
      -- Copy only the bytes for the one element into an aligned local
      -- then read the value through an overlay. This is cheaper than
      -- copying the whole Bytes buffer just to read one element. The
      -- slice's alignment must satisfy T_Unconstrained's alignment;
      -- the Compile_Time_Error below verifies our static literal does.
      Slice_Start : constant Natural := Bytes'First + (Natural (Field) - 1) * Element_Size_In_Bytes;
      pragma Compile_Time_Error (T_Unconstrained'Alignment > {{ required_alignment }},
         "T_Unconstrained'Alignment > static literal ({{ required_alignment }}); update gen/models/array.py");
      Slice : Basic_Types.Byte_Array (0 .. Element_Size_In_Bytes - 1) :=
         Bytes (Slice_Start .. Slice_Start + Element_Size_In_Bytes - 1)
         with Alignment => {{ required_alignment }};
      Src : T_Unconstrained (Constrained_Index_Type'First .. Constrained_Index_Type'First)
         with Import, Convention => Ada, Address => Slice'Address;
{% else %}
      use Byte_Array_Util;
      To_Return : Basic_Types.Poly_Type := [others => 0];
      -- For sub-byte type, for safety, we need to copy the full
      -- Bytes into an aligned local matching T'Alignment.
      Aligned_Bytes : constant Serialization.Byte_Array := Bytes
         with Alignment => {{ required_alignment }};
      Src : T with Import, Convention => Ada, Address => Aligned_Bytes'Address;
{% endif %}
   begin
{% if element.is_packed_type %}
      if Field > 0 then
         Remainder := ((Field - 1) mod {{ element.type_model.num_fields }}) + 1;
      end if;
      declare
         Elem_Bytes : {{ element.type_package }}.Serialization.Byte_Array
            with Import, Convention => Ada, Address => Bytes (Slice_Start)'Address;
      begin
         To_Return := {{ element.type_package }}.Validation.Get_Field (Elem_Bytes, Remainder);
      end;
{% else %}
      declare
         -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
         -- and we know this, so suppresss any checks by the compiler for this copy.
         pragma Suppress (Range_Check);
         pragma Suppress (Overflow_Check);
{% if (element.size % 8) == 0 %}
         Var : constant {{ element.type }} := Src (Constrained_Index_Type'First);
{% else %}
         Var : constant {{ element.type }} := Src (Src'First + Unconstrained_Index_Type (Field) - 1);
{% endif %}
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
   function Get_Field_Le (Bytes : in Serialization_Le.Byte_Array; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if element.is_packed_type %}
      Idx : constant Constrained_Index_Type := Constrained_Index_Type'First + Unconstrained_Index_Type ((Field - 1) / {{ element.type_model.num_fields }});
      Remainder : Unsigned_32 := 0;
      To_Return : Basic_Types.Poly_Type;
      Element_Stride_Bytes : constant Natural := {{ element.type_package }}.Size_In_Bytes;
      Slice_Start : constant Natural := Bytes'First + Natural (Idx - Constrained_Index_Type'First) * Element_Stride_Bytes;
{% elif (element.size % 8) == 0 %}
      use Byte_Array_Util;
      To_Return : Basic_Types.Poly_Type := [others => 0];
      -- Copy only the bytes for the one element into an aligned local
      -- then read the value through an overlay. This is cheaper than
      -- copying the whole Bytes buffer just to read one element. The
      -- slice's alignment must satisfy T_Le_Unconstrained's alignment;
      -- the Compile_Time_Error below verifies our static literal does.
      Slice_Start : constant Natural := Bytes'First + (Natural (Field) - 1) * Element_Size_In_Bytes;
      pragma Compile_Time_Error (T_Le_Unconstrained'Alignment > {{ required_alignment }},
         "T_Le_Unconstrained'Alignment > static literal ({{ required_alignment }}); update gen/models/array.py");
      Slice : Basic_Types.Byte_Array (0 .. Element_Size_In_Bytes - 1) :=
         Bytes (Slice_Start .. Slice_Start + Element_Size_In_Bytes - 1)
         with Alignment => {{ required_alignment }};
      Src : T_Le_Unconstrained (Constrained_Index_Type'First .. Constrained_Index_Type'First)
         with Import, Convention => Ada, Address => Slice'Address;
{% else %}
      use Byte_Array_Util;
      To_Return : Basic_Types.Poly_Type := [others => 0];
      -- For sub-byte type, for safety, we need to copy the full
      -- Bytes into an aligned local matching T_Le'Alignment.
      Aligned_Bytes : constant Serialization_Le.Byte_Array := Bytes
         with Alignment => {{ required_alignment }};
      Src : T_Le with Import, Convention => Ada, Address => Aligned_Bytes'Address;
{% endif %}
   begin
{% if element.is_packed_type %}
      if Field > 0 then
         Remainder := ((Field - 1) mod {{ element.type_model.num_fields }}) + 1;
      end if;
      declare
         Elem_Bytes : {{ element.type_package }}.Serialization_Le.Byte_Array
            with Import, Convention => Ada, Address => Bytes (Slice_Start)'Address;
      begin
         To_Return := {{ element.type_package }}.Validation.Get_Field_Le (Elem_Bytes, Remainder);
      end;
{% else %}
      declare
         -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
         -- and we know this, so suppresss any checks by the compiler for this copy.
         pragma Suppress (Range_Check);
         pragma Suppress (Overflow_Check);
{% if (element.size % 8) == 0 %}
         Var : constant {{ element.type }} := Src (Constrained_Index_Type'First);
{% else %}
         Var : constant {{ element.type }} := Src (Src'First + Unconstrained_Index_Type (Field) - 1);
{% endif %}
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
   end Get_Field_Le;

{% endif %}
end {{ name }}.Validation;
