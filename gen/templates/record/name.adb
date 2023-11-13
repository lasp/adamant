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
{% if variable_length %}
   function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status is
{% for include in variable_length_dynamically_sized_type_includes %}
      use {{ include }};
{% endfor %}
      -- The length in bytes of the serialized type.
      Length : Integer;
      -- Initialize the returned length to the size of the prefix:
      Size_In_Bytes_To_Return : Natural := ({{ prefix_size }} - 1) / Basic_Types.Byte'Object_Size + 1; -- in bytes
   begin
{% for field in (fields.values()|list)[-1:] %}
{% if field.variable_length %}
      -- Size of variable sized field "{{ field.name }}":
      Length := (Integer (Src.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }})) * {{ field.name }}_Unit_Length;
      if Length > 0 then
         Size_In_Bytes_To_Return := Size_In_Bytes_To_Return + Length;
      end if;
{% else %}
      -- Add size of packed type for field "{{ field.name }}":
      if {{ field.type_package }}.Serialized_Length (Src.{{ field.name }}, Length) = Failure then
         Num_Bytes_Serialized := Size_In_Bytes_To_Return;
         return Failure;
      end if;
      Size_In_Bytes_To_Return := Size_In_Bytes_To_Return + Length;
{% endif %}

{% endfor %}
      -- Set return length:
      Num_Bytes_Serialized := Size_In_Bytes_To_Return;

      -- Check the bytes serialized, and make sure it is not too large so
      -- as to be deemed invalid.
      if Num_Bytes_Serialized > Max_Serialized_Length then
         return Failure;
         pragma Annotate (CodePeer, Intentional, "dead code",
            "Sometimes based on the length type of the variable length type it is impossible for Failure to " &
            "be returned here. That is OK. The compiler will optimize this out.");
      end if;
      return Success;
   exception
      when Constraint_Error =>
         Num_Bytes_Serialized := Size_In_Bytes_To_Return;
         return Failure;
   end Serialized_Length;

   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status is
   begin
      -- Make sure byte array is big enough:
      if Min_Serialized_Length > Src'Length then
         Num_Bytes_Serialized := 0;
         return Failure;
      end if;

      declare
         -- Overlay destination with entire byte array:
         pragma Warnings (Off, "overlay changes scalar storage order");
         Overlay : constant T with Import, Convention => Ada, Address => Src'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
         -- Get the serialized length of the destination:
         Stat : constant Serialization_Status := Serialized_Length (Overlay, Num_Bytes_Serialized);
      begin
         -- Make sure calculated length is valid:
         if Stat /= Success then
            return Stat;
         end if;

         -- Make sure the source buffer is large enough to hold computed value:
         if Num_Bytes_Serialized > Src'Length then
            return Failure;
         end if;

         return Stat;
      end;
   end Serialized_Length;

   function Serialized_Length_Le (Src : in T_Le; Num_Bytes_Serialized : out Natural) return Serialization_Status is
{% for include in variable_length_dynamically_sized_type_includes %}
      use {{ include }};
{% endfor %}
      -- The length in bytes of the serialized type.
      Length : Integer;
      -- Initialize the returned length to the minimum:
      Size_In_Bytes_To_Return : Natural := ({{ prefix_size }} - 1) / Basic_Types.Byte'Object_Size + 1; -- in bytes
   begin
{% for field in (fields.values()|list)[-1:] %}
{% if field.variable_length %}
      -- Size of variable sized field "{{ field.name }}":
      Length := (Integer (Src.{{ field.variable_length }}) + Integer ({{ field.variable_length_offset }})) * {{ field.name }}_Unit_Length;
      if Length > 0 then
         Size_In_Bytes_To_Return := Size_In_Bytes_To_Return + Length;
      end if;
{% else %}
      -- Add size of packed type for field "{{ field.name }}":
      if {{ field.type_package }}.Serialized_Length (Src.{{ field.name }}, Length) = Failure then
         Num_Bytes_Serialized := Size_In_Bytes_To_Return;
         return Failure;
      end if;
      Size_In_Bytes_To_Return := Size_In_Bytes_To_Return + Length;
{% endif %}

{% endfor %}
      -- Set return length:
      Num_Bytes_Serialized := Size_In_Bytes_To_Return;

      -- Check the bytes serialized, and make sure it is not too large so
      -- as to be deemed invalid.
      if Num_Bytes_Serialized > Max_Serialized_Length then
         return Failure;
         pragma Annotate (CodePeer, Intentional, "dead code",
            "Sometimes based on the length type of the variable length type it is impossible for Failure to " &
            "be returned here. That is OK. The compiler will optimize this out.");
      end if;
      return Success;
   exception
      when Constraint_Error =>
         Num_Bytes_Serialized := Size_In_Bytes_To_Return;
         return Failure;
   end Serialized_Length_Le;

   function Serialized_Length_Le (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status is
   begin
      -- Make sure byte array is big enough:
      if Min_Serialized_Length > Src'Length then
         Num_Bytes_Serialized := 0;
         return Failure;
      end if;

      declare
         -- Overlay destination with entire byte array:
         pragma Warnings (Off, "overlay changes scalar storage order");
         Overlay : constant T_Le with Import, Convention => Ada, Address => Src'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
         -- Get the serialized length of the destination:
         Stat : constant Serialization_Status := Serialized_Length_Le (Overlay, Num_Bytes_Serialized);
      begin
         -- Make sure calculated length is valid:
         if Stat /= Success then
            return Stat;
         end if;

         -- Make sure the source buffer is large enough to hold computed value:
         if Num_Bytes_Serialized > Src'Length then
            return Failure;
         end if;

         return Stat;
      end;
   end Serialized_Length_Le;

{% else %}
   function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status is
      Ignore : T renames Src;
   begin
      Num_Bytes_Serialized := Max_Serialized_Length;
      return Success;
   end Serialized_Length;

   function Serialized_Length_Le (Src : in T_Le; Num_Bytes_Serialized : out Natural) return Serialization_Status is
      Ignore : T_Le renames Src;
   begin
      Num_Bytes_Serialized := Max_Serialized_Length;
      return Success;
   end Serialized_Length_Le;

   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status is
   begin
      if Max_Serialized_Length > Src'Length then
         Num_Bytes_Serialized := 0;
         return Failure;
      end if;

      Num_Bytes_Serialized := Max_Serialized_Length;
      return Success;
   end Serialized_Length;

   function Serialized_Length_Le (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status is
   begin
      if Max_Serialized_Length > Src'Length then
         Num_Bytes_Serialized := 0;
         return Failure;
      end if;

      Num_Bytes_Serialized := Max_Serialized_Length;
      return Success;
   end Serialized_Length_Le;

{% endif %}
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if packed_type_includes %}
      use Interfaces;
{% endif %}
{% if unpacked_types %}
      use Byte_Array_Util;
{% endif %}
      To_Return : Basic_Types.Poly_Type := (others => 0);
   begin
      case Field is
{% for field in fields.values() %}
{% if field.is_packed_type %}
         when {{ field.start_field_number }} .. {{ field.end_field_number }} =>
            To_Return := {{ field.type_package }}.Get_Field (Src.{{ field.name }}, Field - {{ field.start_field_number + 1 }});
{% else %}
         when {{ field.start_field_number }} =>
            declare
               -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
               -- and we know this, so suppresss any checks by the compiler for this copy.
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

   function Get_Field (Src : in T_Le; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type is
{% if packed_type_includes %}
      use Interfaces;
{% endif %}
{% if unpacked_types %}
      use Byte_Array_Util;
{% endif %}
      To_Return : Basic_Types.Poly_Type := (others => 0);
   begin
      case Field is
{% for field in fields.values() %}
{% if field.is_packed_type %}
         when {{ field.start_field_number }} .. {{ field.end_field_number }} =>
            To_Return := {{ field.type_package }}.Get_Field (Src.{{ field.name }}, Field - {{ field.start_field_number + 1 }});
{% else %}
         when {{ field.start_field_number }} =>
            declare
               -- Copy field over to an unpacked var so that it is byte aligned. The value here is out of range,
               -- and we know this, so suppresss any checks by the compiler for this copy.
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

end {{ name }};
