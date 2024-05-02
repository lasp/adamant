--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
pragma Ada_2022;

package body {{ name }} is

{% if endianness in ["either", "big"] %}
   function Pack (Src : in U) return T is
   begin
{% if complex_type_models %}
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.Pack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
{% else %}
      return T (Src);
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U) return T_Le is
   begin
{% if complex_type_models %}
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.Pack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
{% else %}
      return T_Le (Src);
{% endif %}
   end Pack;

{% endif %}
{% if endianness in ["either", "big"] %}
   function Unpack (Src : in T) return U is
   begin
{% if complex_type_models %}
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.Unpack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
{% else %}
      return U (Src);
{% endif %}
   end Unpack;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Unpack (Src : in T_Le) return U is
   begin
{% if complex_type_models %}
      return (
{% for field in fields.values() %}
{% if field.is_packed_type %}
         {{ field.name }} => {{ field.type_package }}.Unpack (Src.{{ field.name }}){{ "," if not loop.last }}
{% else %}
         {{ field.name }} => Src.{{ field.name }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
      );
{% else %}
      return U (Src);
{% endif %}
   end Unpack;
{% if endianness in ["either"] %}

   function Swap_Endianness (Src : in T) return T_Le is
      Unpacked : constant U := Unpack (Src);
   begin
      return Pack (Unpacked);
   end Swap_Endianness;

   function Swap_Endianness (Src : in T_Le) return T is
      Unpacked : constant U := Unpack (Src);
   begin
      return Pack (Unpacked);
   end Swap_Endianness;
{% endif %}

{% endif %}
{% if variable_length %}
{% if endianness in ["either", "big"] %}
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

{% endif %}
{% if endianness in ["either", "little"] %}
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

{% endif %}
{% else %}
{% if endianness in ["either", "big"] %}
   function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status is
      Ignore : T renames Src;
   begin
      Num_Bytes_Serialized := Max_Serialized_Length;
      return Success;
   end Serialized_Length;

{% endif %}
{% if endianness in ["either", "little"] %}
   function Serialized_Length_Le (Src : in T_Le; Num_Bytes_Serialized : out Natural) return Serialization_Status is
      Ignore : T_Le renames Src;
   begin
      Num_Bytes_Serialized := Max_Serialized_Length;
      return Success;
   end Serialized_Length_Le;

{% endif %}
{% if endianness in ["either", "big"] %}
   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status is
   begin
      if Max_Serialized_Length > Src'Length then
         Num_Bytes_Serialized := 0;
         return Failure;
      end if;

      Num_Bytes_Serialized := Max_Serialized_Length;
      return Success;
   end Serialized_Length;

{% endif %}
{% if endianness in ["either", "little"] %}
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
{% endif %}
end {{ name }};
