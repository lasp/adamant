--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
pragma Ada_2022;

-- Standard Includes:
with Basic_Types;
with System;
with Serializer_Types; use Serializer_Types;
{% if not variable_length %}
with Serializer;
{% endif %}
{% if variable_length %}
with Variable_Serializer;
{% endif %}
{% if includes %}

-- Custom Includes:
{% for include in includes %}
{% if include not in ["Basic_Types", "System"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if type_includes %}

-- Record Component Includes:
{% for include in type_includes %}
{% if include not in includes and include not in ["Basic_Types", "System"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is
{% if preamble %}

   -- Preamble code:
{{ printMultiLine(preamble, '   ', 10000) }}
{% endif %}

   -- Packed type size (in bits):
   Size : constant Positive := {{ size }};

   -- Packed type size rounded up to nearest byte.
   Size_In_Bytes : constant Positive := (Size - 1) / Basic_Types.Byte'Object_Size + 1;

   -- The total number of fields contained in the packed record, this includes
   -- any fields of packed records included directly as a member in this
   -- packed record.
   Num_Fields : constant Positive := {{ num_fields }};

   -- Unpacked type:
   type U is record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
{% if field.is_packed_type %}
      {{ field.name }} : {{ field.type_package }}.U{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% else %}
{% if field.format.length and field.format.length > 1 %}
      -- We can safely ignore scalar storage order warnings for arrayed fields with components <8 bits in
      -- size since endianness does not apply.
{% if endianness in ["either", "big"] %}
      pragma Warnings (Off, "scalar storage order specified for ""T"" does not apply to component");
      pragma Warnings (Off, "scalar storage order specified for ""Volatile_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
      pragma Warnings (Off, "scalar storage order specified for ""T_Le"" does not apply to component");
      pragma Warnings (Off, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% endif %}
{% if size == 32 or size == 16 or size == 8 %}
{% if endianness in ["either", "big"] %}
      pragma Warnings (Off, "scalar storage order specified for ""Atomic_T"" does not apply to component");
      pragma Warnings (Off, "scalar storage order specified for ""Register_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
      pragma Warnings (Off, "scalar storage order specified for ""Atomic_T_Le"" does not apply to component");
      pragma Warnings (Off, "scalar storage order specified for ""Register_T_Le"" does not apply to component");
{% endif %}
{% endif %}
{% endif %}
      {{ field.name }} : {{ field.type }}{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% if field.format.length and field.format.length > 1 %}
{% if endianness in ["either", "big"] %}
      pragma Warnings (On, "scalar storage order specified for ""T"" does not apply to component");
      pragma Warnings (On, "scalar storage order specified for ""Volatile_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
      pragma Warnings (On, "scalar storage order specified for ""T_Le"" does not apply to component");
      pragma Warnings (On, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% endif %}
{% if size == 32 or size == 16 or size == 8 %}
{% if endianness in ["either", "big"] %}
      pragma Warnings (On, "scalar storage order specified for ""Atomic_T"" does not apply to component");
      pragma Warnings (On, "scalar storage order specified for ""Register_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
      pragma Warnings (On, "scalar storage order specified for ""Atomic_T_Le"" does not apply to component");
      pragma Warnings (On, "scalar storage order specified for ""Register_T_Le"" does not apply to component");
{% endif %}
{% endif %}
{% endif %}
{% endif %}
{% endfor %}
   end record;

   -- Access type for U
   type U_Access is access all U;

{% if endianness in ["either", "big"] %}
   -- Packed type definition.
{% if complex_type_models %}
   type T is record
{% for field in fields.values() %}
{% if field.is_packed_type %}
      {{ field.name }} : {{ field.type_package }}.T{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% else %}
      {{ field.name }} : {{ field.type }}{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% endif %}
{% endfor %}
   end record
{% else %}
   type T is new U
{% endif %}
      with Bit_Order => System.High_Order_First,
           Scalar_Storage_Order => System.High_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Alignment => 1,
           Volatile => False;

   -- Packed type layout:
   for T use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Access type for T
   type T_Access is access all T;

{% endif %}
{% if endianness in ["either", "little"] %}
   -- Packed type definition with little endian definition.
{% if complex_type_models %}
   type T_Le is record
{% for field in fields.values() %}
{% if field.is_packed_type %}
      {{ field.name }} : {{ field.type_package }}.T_Le{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% else %}
      {{ field.name }} : {{ field.type }}{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% endif %}
{% endfor %}
   end record
{% else %}
   type T_Le is new U
{% endif %}
      with Bit_Order => System.Low_Order_First,
           Scalar_Storage_Order => System.Low_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Alignment => 1,
           Volatile => False;

   -- Packed type layout:
   for T_Le use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Access type for T_Le
   type T_Le_Access is access all T_Le;

{% endif %}
{% if volatile_descriptor %}
   --
   -- Note: This {{ volatile_descriptor|lower }} type is created because the model is defined with all {{ volatile_descriptor|lower }} fields.
   --

{% if endianness in ["either", "big"] %}
   -- {{ volatile_descriptor }} packed type:
   type {{ volatile_descriptor }}_T is record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} : {{ field.type }}{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% endfor %}
   end record
      with Bit_Order => System.High_Order_First,
           Scalar_Storage_Order => System.High_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Alignment => 4, -- Cannot be aligned at 1, must be aligned at word boundary
           Volatile => True;

   -- {{ volatile_descriptor }} packed type layout:
   for {{ volatile_descriptor }}_T use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Access type for {{ volatile_descriptor }}_T
   type {{ volatile_descriptor }}_T_Access is access all {{ volatile_descriptor }}_T;

{% endif %}
{% if endianness in ["either", "little"] %}
   -- {{ volatile_descriptor }} packed little endian type:
   type {{ volatile_descriptor }}_T_Le is record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} : {{ field.type }}{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% endfor %}
   end record
      with Bit_Order => System.Low_Order_First,
           Scalar_Storage_Order => System.Low_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Alignment => 4, -- Cannot be aligned at 1, must be aligned at word boundary
           Volatile => True;

   -- {{ volatile_descriptor }} packed type layout:
   for {{ volatile_descriptor }}_T_Le use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Access type for {{ volatile_descriptor }}_T_Le
   type {{ volatile_descriptor }}_T_Le_Access is access all {{ volatile_descriptor }}_T_Le;

{% endif %}
{% else %}
{% if endianness in ["either", "big"] %}
   -- Volatile packed type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T is new T
      with Bit_Order => System.High_Order_First,
           Scalar_Storage_Order => System.High_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Alignment => 1,
           Volatile => True;

   -- Access type for Volatile_T
   type Volatile_T_Access is access all Volatile_T;

{% endif %}
{% if endianness in ["either", "little"] %}
   -- Volatile little endian packed type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T_Le is new T_Le
      with Bit_Order => System.Low_Order_First,
           Scalar_Storage_Order => System.Low_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Alignment => 1,
           Volatile => True;

   -- Access type for Volatile_T_Le
   type Volatile_T_Le_Access is access all Volatile_T_Le;

{% endif %}
{% if size == 32 or size == 16 or size == 8 %}
   --
   -- Atomic and Register type definitions. These are only supported for types that are
   -- either 8, 16, or 32 bits in size.
   --
{% if endianness in ["either", "big"] %}
   -- Atomic packed type definition:
   -- Note: This type is atomic. Use this type to specify that the code
   -- generated must read and write the type or variable from memory atomically,
   -- i.e. as a single/non-interruptible operation. It implies pragma Volatile,
   -- the difference is that pragma Atomic is stronger: the compilation must
   -- fail if the variable cannot be updated atomically.
   -- Important: Atomic types create a synchronization point and can be used for
   -- very limited intertask communication. However, protected objects should almost
   -- always be preferred.
   type Atomic_T is new T
      with Bit_Order => System.High_Order_First,
           Scalar_Storage_Order => System.High_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Volatile => True,
           Atomic => True;

   -- Access type for Atomic_T
   type Atomic_T_Access is access all Atomic_T;

{% endif %}
{% if endianness in ["either", "little"] %}
   -- Atomic little endian packed type definition:
   -- Note: This type is atomic. Use this type to specify that the code
   -- generated must read and write the type or variable from memory atomically,
   -- i.e. as a single/non-interruptible operation. It implies pragma Volatile,
   -- the difference is that pragma Atomic is stronger: the compilation must
   -- fail if the variable cannot be updated atomically.
   -- Important: Atomic types create a synchronization point and can be used for
   -- very limited intertask communication. However, protected objects should almost
   -- always be preferred.
   type Atomic_T_Le is new T_Le
      with Bit_Order => System.Low_Order_First,
           Scalar_Storage_Order => System.Low_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Volatile => True,
           Atomic => True;

   -- Access type for Atomic_T_Le
   type Atomic_T_Le_Access is access all Atomic_T_Le;

{% endif %}
{% if endianness in ["either", "big"] %}
   -- Register packed type definition:
   -- Note: This type is Volatile_Full_Access. This is similar in effect to
   -- pragma Volatile, except that any reference to the object is guaranteed to
   -- be done only with instructions that read or write all the bits of the object.
   -- Furthermore, if the object is of a composite type, then any reference to a
   -- subcomponent of the object is guaranteed to read and/or write all the bits
   -- of the object.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T is new T
      with Bit_Order => System.High_Order_First,
           Scalar_Storage_Order => System.High_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Volatile => True,
           Volatile_Full_Access => True;

   -- Access type for Register_T
   type Register_T_Access is access all Register_T;

{% endif %}
{% if endianness in ["either", "little"] %}
   -- Register little endian packed type definition:
   -- Note: This type is Volatile_Full_Access. This is similar in effect to
   -- pragma Volatile, except that any reference to the object is guaranteed to
   -- be done only with instructions that read or write all the bits of the object.
   -- Furthermore, if the object is of a composite type, then any reference to a
   -- subcomponent of the object is guaranteed to read and/or write all the bits
   -- of the object.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T_Le is new T_Le
      with Bit_Order => System.Low_Order_First,
           Scalar_Storage_Order => System.Low_Order_First,
           Size => Size,
           Object_Size => Size,
           Value_Size => Size,
           Volatile => True,
           Volatile_Full_Access => True;

   -- Access type for Register_T_Le
   type Register_T_Le_Access is access all Register_T_Le;

{% endif %}
{% else %}
   -- Not supported. This type is {{ size }} bits in size.
{% endif %}
{% endif %}
   -- Type conversion functions between packed an unpacked representations:
{% if endianness in ["either", "big"] %}
   function Pack (Src : in U) return T{% if not complex_type_models %} with Inline => True{% endif %};
   function Unpack (Src : in T) return U{% if not complex_type_models %} with Inline => True{% endif %};
{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U) return T_Le{% if not complex_type_models %} with Inline => True{% endif %};
   function Unpack (Src : in T_Le) return U{% if not complex_type_models %} with Inline => True{% endif %};
{% endif %}
{% if endianness in ["either"] %}

   -- Endianness conversion functions
   function Swap_Endianness (Src : in T) return T_Le{% if not complex_type_models %} with Inline => True{% endif %};
   function Swap_Endianness (Src : in T_Le) return T{% if not complex_type_models %} with Inline => True{% endif %};
{% endif %}

{% if variable_length %}
   -- The minimum length in bytes of the serialized type.
   Min_Serialized_Length : constant Natural := ({{ min_size }} - 1) / Basic_Types.Byte'Object_Size + 1; -- in bytes

   -- The maximum length in bytes of the serialized type.
   Max_Serialized_Length : constant Natural := T'Object_Size / Basic_Types.Byte'Object_Size; -- in bytes

   -- Get the length of the record when serialized:
   -- These functions may produce an error if the length of a variable field
   -- is invalid, or if the result is larger than Max_Serialized_Length, above.
   -- The calculated length value is returned regardless of the error Serialization_Status, if you
   -- need to report it in an error event, etc.
   -- If the Serialization_Status is Failure and num_Bytes_Serialized is less than Max_Serialized_Length
   -- then the number symbolizes the byte at which serialization failed due to an out of bounds
   -- length. If the Serialization_Status is Failure and num_Bytes_Serialized is greater than
   -- Max_Serialized_Length, then the number symbolizes the actual calculated length, which is
   -- too large.
{% if endianness in ["either", "big"] %}
   function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Serialized_Length_Le (Src : in T_Le; Num_Bytes_Serialized : out Natural) return Serialization_Status;
{% endif %}

   -- Get the size of the already serialized type inside of the byte array. If the byte array is too small then
   -- a serialization error is returned.
{% if endianness in ["either", "big"] %}
   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Serialized_Length_Le (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status;
{% endif %}

   -- Serializing functions for entire record:
{% if endianness in ["either", "big"] %}
   package Serialization is new Variable_Serializer (T, Serialized_Length, Serialized_Length);
{% endif %}
{% if endianness in ["either", "little"] %}
   package Serialization_Le is new Variable_Serializer (T_Le, Serialized_Length_Le, Serialized_Length_Le);
{% endif %}
{% else %}
   -- Serializing functions for entire record:
{% if endianness in ["either", "big"] %}
   package Serialization is new Serializer (T);
{% endif %}
{% if endianness in ["either", "little"] %}
   package Serialization_Le is new Serializer (T_Le);
{% endif %}

{% if endianness in ["either", "big"] %}
   -- The length in bytes of the serialized type.
   Max_Serialized_Length : Natural renames Serialization.Serialized_Length; -- in bytes
   -- The length in bytes of the serialized type.
   Min_Serialized_Length : Natural renames Serialization.Serialized_Length; -- in bytes
{% elif endianness in ["little"] %}
   -- The length in bytes of the serialized type.
   Max_Serialized_Length : Natural renames Serialization_Le.Serialized_Length; -- in bytes
   -- The length in bytes of the serialized type.
   Min_Serialized_Length : Natural renames Serialization_Le.Serialized_Length; -- in bytes
{% endif %}

   -- Convenience function which always returns Success and the length defined above ^. This
   -- is useful when you want to instantiate a generic which requires a function of the
   -- definition below, but its for a statically sized type.
{% if endianness in ["either", "big"] %}
   function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Serialized_Length_Le (Src : in T_Le; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;
{% endif %}

   -- Get the size of the already serialized type inside of the byte array. If the byte array is too small then
   -- a serialization error is returned.
{% if endianness in ["either", "big"] %}
   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;
{% endif %}
{% if endianness in ["either", "little"] %}
   function Serialized_Length_Le (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;
{% endif %}
{% endif %}

   ----------------------------------------------
   -- Types related to record's internal fields:
   ----------------------------------------------
   -- Field type sizes (in bits):
{% for field in fields.values() %}
{% if field.is_packed_type %}
   {{ field.name }}_Size : constant Positive := {{ field.type_package }}.Size;
{% else %}
   {{ field.name }}_Size : constant Positive := {{ field.size }};
{% endif %}
{% endfor %}

   -- Packed field sizes in bytes rounded up to nearest byte boundary.
{% for field in fields.values() %}
{% if field.is_packed_type %}
   {{ field.name }}_Size_In_Bytes : constant Positive := {{ field.type_package }}.Size_In_Bytes;
{% else %}
   {{ field.name }}_Size_In_Bytes : constant Positive := ({{ field.name }}_Size - 1) / Basic_Types.Byte'Object_Size + 1;
{% endif %}
{% endfor %}
{% if variable_length %}

   -- Unit lengths (in bytes) for variable sized fields:
{% for field in variable_length_fields.values() %}
{% if field.is_packed_type %}
{% if not field.type_model.variable_length %}
   pragma Compile_Time_Error ({{ field.type }}'Component_Size /= {{ field.type_model.element.size }}, "Component size is not as expected!");
   {{ field.name }}_Unit_Length : constant Natural := ({{ field.type }}'Component_Size - 1) / Basic_Types.Byte'Object_Size + 1;
{% endif %}
{% elif field.size % 8 == 0 %}
   pragma Compile_Time_Error ({{ field.type }}'Component_Size /= {{ field.format.unit_size }}, "Component size is not as expected!");
   {{ field.name }}_Unit_Length : constant Natural := ({{ field.type }}'Component_Size - 1) / Basic_Types.Byte'Object_Size + 1;
{% endif %}
{% endfor %}
{% endif %}

end {{ name }};
