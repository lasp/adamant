--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Representation Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

package body {{ name }}.Representation is

   -- Helper function to get element string for unconstrained arrays
   function Get_Element_String (R : in {% if element.is_packed_type %}{{ element.type_package }}.U{% else %}{{ element.type }}{% endif %}) return String is
   begin
      return Element_To_Tuple_String (R);
   end Get_Element_String;
{% if element.is_packed_type %}
{% if endianness in ["either", "big"] %}

   function Get_Element_String (R : in {{ element.type_package }}.T) return String is
   begin
      return Element_To_Tuple_String ({{ element.type_package }}.Unpack (R));
   end Get_Element_String;
{% endif %}
{% if endianness in ["either", "little"] %}

   function Get_Element_String (R : in {{ element.type_package }}.T_Le) return String is
   begin
      return Element_To_Tuple_String ({{ element.type_package }}.Unpack (R));
   end Get_Element_String;
{% endif %}
{% endif %}
{% if length %}

   -- Private function which translates constrained array into string:
{% if endianness in ["either", "big"] %}
   function Array_To_String is new String_Util.To_Array_String ({% if element.is_packed_type %}{{ element.type_package }}.T{% else %}{{ element.type }}{% endif %}, Constrained_Index_Type, T, Element_To_Tuple_String);
{% endif %}
{% if endianness in ["either", "little"] %}
   function Array_To_String is new String_Util.To_Array_String ({% if element.is_packed_type %}{{ element.type_package }}.T_Le{% else %}{{ element.type }}{% endif %}, Constrained_Index_Type, T_Le, Element_To_Tuple_String);
{% endif %}
{% endif %}

   -- Return string representation of array elements:
   function To_String (R : in Unconstrained; Prefix : in String := "") return String is
      Result : constant String := To_Tuple_String (R);
   begin
      return Prefix & "{{ name }} : array {% if element.is_packed_type %}{{ element.type_package }}.U{% else %}{{ element.type }}{% endif %} (" & Unconstrained_Index_Type'Image (R'First) & " .. " & Unconstrained_Index_Type'Image (R'Last) & ") => " & Result;
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.Unconstrained invalid. Constraint_Error thrown.";
   end To_String;
{% if (element.size % 8) == 0 and endianness in ["either", "big"] %}

   function To_String (R : in T_Unconstrained; Prefix : in String := "") return String is
      Result : constant String := To_Tuple_String (R);
   begin
      return Prefix & "{{ name }} : array {% if element.is_packed_type %}{{ element.type_package }}.T{% else %}{{ element.type }}{% endif %} (" & Unconstrained_Index_Type'Image (R'First) & " .. " & Unconstrained_Index_Type'Image (R'Last) & ") => " & Result;
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T_Unconstrained invalid. Constraint_Error thrown.";
   end To_String;
{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "little"] %}

   function To_String (R : in T_Le_Unconstrained; Prefix : in String := "") return String is
      Result : constant String := To_Tuple_String (R);
   begin
      return Prefix & "{{ name }} : array {% if element.is_packed_type %}{{ element.type_package }}.T_Le{% else %}{{ element.type }}{% endif %} (" & Unconstrained_Index_Type'Image (R'First) & " .. " & Unconstrained_Index_Type'Image (R'Last) & ") => " & Result;
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T_Le_Unconstrained invalid. Constraint_Error thrown.";
   end To_String;
{% endif %}

{% if length and endianness in ["either", "big"] %}
   function To_String (R : in T; Prefix : in String := "") return String is
   begin
      return Prefix & "{{ name }} : array {{ element.type }} (1 .. {{ length }}) => [ " & Array_To_String (R, Show_Index => True) & "]";
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_String;

{% endif %}
{% if length and endianness in ["either", "little"] %}
   function To_String (R : in T_Le; Prefix : in String := "") return String is
   begin
      return Prefix & "{{ name }} : array {{ element.type }} (1 .. {{ length }}) => [ " & Array_To_String (R, Show_Index => True) & "]";
   exception
      when Constraint_Error =>
         return Prefix & "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_String;

{% endif %}
   -- Return compact representation of array as string:
   function To_Tuple_String (R : in Unconstrained) return String is
      Result : String (1 .. 10000) := [others => ' '];
      Last : Natural := 1;
   begin
      Result (Last) := '[';
      for I in R'Range loop
         if I > R'First then
            Last := Last + 1;
            Result (Last .. Last + 1) := ", ";
            Last := Last + 1;
         else
            Last := Last + 1;
         end if;
         declare
            Elem_Str : constant String := Get_Element_String (R (I));
         begin
            Result (Last .. Last + Elem_Str'Length - 1) := Elem_Str;
            Last := Last + Elem_Str'Length - 1;
         end;
      end loop;
      Last := Last + 1;
      Result (Last) := ']';
      return Result (1 .. Last);
   exception
      when Constraint_Error =>
         return "{{ name }}.Unconstrained invalid. Constraint_Error thrown.";
   end To_Tuple_String;
{% if (element.size % 8) == 0 and endianness in ["either", "big"] %}

   function To_Tuple_String (R : in T_Unconstrained) return String is
      Result : String (1 .. 10000) := [others => ' '];
      Last : Natural := 1;
   begin
      Result (Last) := '[';
      for I in R'Range loop
         if I > R'First then
            Last := Last + 1;
            Result (Last .. Last + 1) := ", ";
            Last := Last + 1;
         else
            Last := Last + 1;
         end if;
         declare
            Elem_Str : constant String := Get_Element_String (R (I));
         begin
            Result (Last .. Last + Elem_Str'Length - 1) := Elem_Str;
            Last := Last + Elem_Str'Length - 1;
         end;
      end loop;
      Last := Last + 1;
      Result (Last) := ']';
      return Result (1 .. Last);
   exception
      when Constraint_Error =>
         return "{{ name }}.T_Unconstrained invalid. Constraint_Error thrown.";
   end To_Tuple_String;
{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "little"] %}

   function To_Tuple_String (R : in T_Le_Unconstrained) return String is
      Result : String (1 .. 10000) := [others => ' '];
      Last : Natural := 1;
   begin
      Result (Last) := '[';
      for I in R'Range loop
         if I > R'First then
            Last := Last + 1;
            Result (Last .. Last + 1) := ", ";
            Last := Last + 1;
         else
            Last := Last + 1;
         end if;
         declare
            Elem_Str : constant String := Get_Element_String (R (I));
         begin
            Result (Last .. Last + Elem_Str'Length - 1) := Elem_Str;
            Last := Last + Elem_Str'Length - 1;
         end;
      end loop;
      Last := Last + 1;
      Result (Last) := ']';
      return Result (1 .. Last);
   exception
      when Constraint_Error =>
         return "{{ name }}.T_Le_Unconstrained invalid. Constraint_Error thrown.";
   end To_Tuple_String;
{% endif %}

{% if length and endianness in ["either", "big"] %}
   -- Return compact representation of array as string:
   function To_Tuple_String (R : in T) return String is
   begin
      return "[" & Array_To_String (R, Show_Index => False) & "]";
   exception
      when Constraint_Error =>
         return "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_Tuple_String;

{% endif %}
{% if length and endianness in ["either", "little"] %}
   -- Return compact representation of array as string:
   function To_Tuple_String (R : in T_Le) return String is
   begin
      return "[" & Array_To_String (R, Show_Index => False) & "]";
   exception
      when Constraint_Error =>
         return "{{ name }}.T invalid. Constraint_Error thrown.";
   end To_Tuple_String;

{% endif %}
   -- Return string representation of array elements and bytes
   function Image_With_Prefix (R : in Unconstrained; Prefix : in String) return String is
   begin
      return To_String (R, Prefix);
   end Image_With_Prefix;
{% if (element.size % 8) == 0 and endianness in ["either", "big"] %}

   function Image_With_Prefix (R : in T_Unconstrained; Prefix : in String) return String is
   begin
      return To_String (R, Prefix);
   end Image_With_Prefix;
{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "little"] %}

   function Image_With_Prefix (R : in T_Le_Unconstrained; Prefix : in String) return String is
   begin
      return To_String (R, Prefix);
   end Image_With_Prefix;
{% endif %}

{% if length and endianness in ["either", "big"] %}
   function Image_With_Prefix (R : in T; Prefix : in String) return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

{% endif %}
{% if length and endianness in ["either", "little"] %}
   function Image_With_Prefix (R : in T_Le; Prefix : in String) return String is
   begin
      return To_Byte_String (R) & ASCII.LF & To_String (R, Prefix);
   end Image_With_Prefix;

{% endif %}
   -- Return string representation of array elements and bytes (with no prefix):
   function Image (R : in Unconstrained) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;
{% if (element.size % 8) == 0 and endianness in ["either", "big"] %}

   function Image (R : in T_Unconstrained) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;
{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "little"] %}

   function Image (R : in T_Le_Unconstrained) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;
{% endif %}
{% if length and endianness in ["either", "big"] %}

   function Image (R : in T) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;
{% endif %}
{% if length and endianness in ["either", "little"] %}

   function Image (R : in T_Le) return String is
   begin
      return Image_With_Prefix (R, "");
   end Image;
{% endif %}
end {{ name }}.Representation;
