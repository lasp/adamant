--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

{% if unpacked_types %}
-- Standard includes:
with Byte_Array_Util;

{% endif %}
package body {{ name }} is

{% if is_volatile_type %}
   -- We create this so that an .adb can be generated legally. This will
   -- get optimized out. Volatile packed records do not need regular packed
   -- record .adb.
   procedure Dummy is
   begin
      null;
   end Dummy;
{% else %}
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if element.is_packed_type %}
      use Interfaces;
      Idx : constant Constrained_Index_Type := Constrained_Index_Type'First + Unconstrained_Index_Type (Field / {{ element.type_model.num_fields }});
      Remainder : Unsigned_32 := 0;
      To_Return : Basic_Types.Poly_Type;
{% else %}
      use Byte_Array_Util;
      To_Return : Basic_Types.Poly_Type := (others => 0);
{% endif %}
   begin
{% if element.is_packed_type %}
      if Field > 0 then
         Remainder := ((Field - 1) mod {{ element.type_model.num_fields }}) + 1;
      end if;
      To_Return := {{ element.type_package }}.Get_Field (Src (Idx), Remainder);
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

end {{ name }};
