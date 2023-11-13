--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Basic_Types;
with System;
{% if not is_volatile_type %}
with Interfaces;
with Serializer_Types; use Serializer_Types;
{% if not variable_length %}
with Serializer;
{% endif %}
{% endif %}
{% if variable_length %}
with Variable_Serializer;
{% endif %}
{% if includes %}

-- Custom Includes:
{% for include in includes %}
{% if include not in ["Basic_Types", "System", "Interfaces"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if type_includes %}

-- Record Component Includes:
{% for include in type_includes %}
{% if include not in includes and include not in ["Basic_Types", "System", "Interfaces"] %}
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
{% if volatile_descriptor %}

   --
   -- Note: This is a {{ volatile_descriptor|lower }} type because it contains {{ volatile_descriptor|lower }} fields. This means
   -- that no "U", "T", or "T_Le" type will be created for it, only a "{{ volatile_descriptor }}_T" and a
   -- "{{ volatile_descriptor }}_T_Le" type. The big endian and little endian types are provided for
   -- symmetry, but it is important to realize that the endianness of the type is
   -- determined by the endianness of the lowest level fields.
   --
   -- Note also that serialization package is not supplied for a {{ volatile_descriptor|lower }} type. To
   -- gain access to a serialization package, define a version of this record
   -- without internal volatile types, and convert from this type to that type
   -- via overlay or unchecked conversion.
   --

   -- Fields that are 8-bit types or 8-bit array types trigger the following warning. Obviously endianness does
   -- not apply for types of 1 byte or less, so we can ignore this warning.
   pragma Warnings (Off, "scalar storage order specified for ""{{ volatile_descriptor }}_T"" does not apply to component");

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

   -- Re-enable warning.
   pragma Warnings (On, "scalar storage order specified for ""{{ volatile_descriptor }}_T"" does not apply to component");

   -- {{ volatile_descriptor }} packed type layout:
   for {{ volatile_descriptor }}_T use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Fields that are 8-bit types or 8-bit array types trigger the following warning. Obviously endianness does
   -- not apply for types of 1 byte or less, so we can ignore this warning.
   pragma Warnings (Off, "scalar storage order specified for ""{{ volatile_descriptor }}_T_Le"" does not apply to component");

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

   -- Re-enable warning.
   pragma Warnings (On, "scalar storage order specified for ""{{ volatile_descriptor }}_T_Le"" does not apply to component");

   -- {{ volatile_descriptor }} packed type layout:
   for {{ volatile_descriptor }}_T_Le use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Create access types to records:
   type {{ volatile_descriptor }}_T_Access is access all {{ volatile_descriptor }}_T;
   type {{ volatile_descriptor }}_T_Le_Access is access all {{ volatile_descriptor }}_T_Le;

   -- We create this so that an .adb can be generated legally. This will
   -- get optimized out. Volatile packed records do not need regular packed
   -- record .adb.
   procedure Dummy;
{% else %}
   -- Fields that are 8-bit types or 8-bit array types trigger the following warning. Obviously endianness does
   -- not apply for types of 1 byte or less, so we can ignore this warning.
   pragma Warnings (Off, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""T_Le"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% if size == 32 or size == 16 or size == 8 %}
   pragma Warnings (Off, "scalar storage order specified for ""Atomic_T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Atomic_T_Le"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Register_T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Register_T_Le"" does not apply to component");
{% endif %}

   -- Unpacked type:
   type U is record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} : {{ field.type }}{% if field.default_value %} := {{ field.default_value }}{% endif %};
{% endfor %}
   end record;

   -- Re-enable warning.
   pragma Warnings (On, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""T_Le"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Volatile_T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% if size == 32 or size == 16 or size == 8 %}
   pragma Warnings (On, "scalar storage order specified for ""Atomic_T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Atomic_T_Le"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Register_T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Register_T_Le"" does not apply to component");
{% endif %}

   -- Packed type definition.
   type T is new U
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

   -- Packed type definition with little endian definition.
   type T_Le is new U
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

   -- Volatile packed type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T is new U
      with Bit_Order => System.High_Order_First,
             Scalar_Storage_Order => System.High_Order_First,
             Size => Size,
             Object_Size => Size,
             Value_Size => Size,
             Alignment => 1,
             Volatile => True;

   -- Volatile packed type layout:
   for Volatile_T use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Volatile little endian packed type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T_Le is new U
      with Bit_Order => System.Low_Order_First,
             Scalar_Storage_Order => System.Low_Order_First,
             Size => Size,
             Object_Size => Size,
             Value_Size => Size,
             Alignment => 1,
             Volatile => True;

   -- Volatile packed type layout:
   for Volatile_T_Le use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   --
   -- Atomic and Register type definitions. These are only supported for types that are
   -- either 8, 16, or 32 bits in size.
   --

{% if size == 32 or size == 16 or size == 8 %}
   -- Atomic packed type definition:
   -- Note: This type is atomic. Use this type to specify that the code
   -- generated must read and write the type or variable from memory atomically,
   -- i.e. as a single/non-interruptible operation. It implies pragma Volatile,
   -- the difference is that pragma Atomic is stronger: the compilation must
   -- fail if the variable cannot be updated atomically.
   -- Important: Atomic types create a synchronization point and can be used for
   -- very limited intertask communication. However, protected objects should almost
   -- always be preferred.
   type Atomic_T is new U
      with Bit_Order => System.High_Order_First,
             Scalar_Storage_Order => System.High_Order_First,
             Size => Size,
             Object_Size => Size,
             Value_Size => Size,
             -- Alignment => 1, <- this is not supported for atomic types
             Volatile => True,
             Atomic => True;

   -- Atomic packed type layout:
   for Atomic_T use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Atomic little endian packed type definition:
   -- Note: This type is atomic. Use this type to specify that the code
   -- generated must read and write the type or variable from memory atomically,
   -- i.e. as a single/non-interruptible operation. It implies pragma Volatile,
   -- the difference is that pragma Atomic is stronger: the compilation must
   -- fail if the variable cannot be updated atomically.
   -- Important: Atomic types create a synchronization point and can be used for
   -- very limited intertask communication. However, protected objects should almost
   -- always be preferred.
   type Atomic_T_Le is new U
      with Bit_Order => System.Low_Order_First,
             Scalar_Storage_Order => System.Low_Order_First,
             Size => Size,
             Object_Size => Size,
             Value_Size => Size,
             -- Alignment => 1, <- this is not supported for atomic types
             Volatile => True,
             Atomic => True;

   -- Atomic packed type layout:
   for Atomic_T_Le use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Register packed type definition:
   -- Note: This type is Volatile_Full_Access. This is similar in effect to
   -- pragma Volatile, except that any reference to the object is guaranteed to
   -- be done only with instructions that read or write all the bits of the object.
   -- Furthermore, if the object is of a composite type, then any reference to a
   -- subcomponent of the object is guaranteed to read and/or write all the bits
   -- of the object.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T is new U
      with Bit_Order => System.High_Order_First,
             Scalar_Storage_Order => System.High_Order_First,
             Size => Size,
             Object_Size => Size,
             Value_Size => Size,
             -- Alignment => 1, <- this is not supported for Volatile_Full_Access types
             Volatile => True,
             Volatile_Full_Access => True;

   -- Register packed type layout:
   for Register_T use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;

   -- Register little endian packed type definition:
   -- Note: This type is Volatile_Full_Access. This is similar in effect to
   -- pragma Volatile, except that any reference to the object is guaranteed to
   -- be done only with instructions that read or write all the bits of the object.
   -- Furthermore, if the object is of a composite type, then any reference to a
   -- subcomponent of the object is guaranteed to read and/or write all the bits
   -- of the object.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T_Le is new U
      with Bit_Order => System.Low_Order_First,
             Scalar_Storage_Order => System.Low_Order_First,
             Size => Size,
             Object_Size => Size,
             Value_Size => Size,
             -- Alignment => 1, <- this is not supported for Volatile_Full_Access types
             Volatile => True,
             Volatile_Full_Access => True;

   -- Register packed type layout:
   for Register_T_Le use record
{% for field in fields.values() %}
{% if field.description %}
{{ printMultiLine(field.description, '      -- ') }}
{% endif %}
      {{ field.name }} at 0 range {{ field.start_bit }} .. {{ field.end_bit }};
{% endfor %}
   end record;
{% else %}
   -- Not supported. This type is {{ size }} bits in size.
{% endif %}

   -- Create access type to packed and unpacked records:
   type U_Access is access all U;
   type T_Access is access all T;
   type T_Le_Access is access all T_Le;
   type Volatile_T_Access is access all Volatile_T;
   type Volatile_T_Le_Access is access all Volatile_T_Le;
{% if size == 32 or size == 16 or size == 8 %}
   type Atomic_T_Access is access all Atomic_T;
   type Atomic_T_Le_Access is access all Atomic_T_Le;
   type Register_T_Access is access all Register_T;
   type Register_T_Le_Access is access all Register_T_Le;
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
   function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status;
   function Serialized_Length_Le (Src : in T_Le; Num_Bytes_Serialized : out Natural) return Serialization_Status;

   -- Get the size of the already serialized type inside of the byte array. If the byte array is too small then
   -- a serialization error is returned.
   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status;
   function Serialized_Length_Le (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status;

   -- Serializing functions for entire record:
   package Serialization is new Variable_Serializer (T, Serialized_Length, Serialized_Length);
   package Serialization_Le is new Variable_Serializer (T_Le, Serialized_Length_Le, Serialized_Length_Le);
{% else %}
   -- Serializing functions for entire record:
   package Serialization is new Serializer (T);
   package Serialization_Le is new Serializer (T_Le);

   -- The length in bytes of the serialized type.
   Max_Serialized_Length : Natural renames Serialization.Serialized_Length; -- in bytes
   -- The length in bytes of the serialized type.
   Min_Serialized_Length : Natural renames Serialization.Serialized_Length; -- in bytes

   -- Convenience function which always returns Success and the length defined above ^. This
   -- is useful when you want to instantiate a generic which requires a function of the
   -- definition below, but its for a statically sized type.
   function Serialized_Length (Src : in T; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;
   function Serialized_Length_Le (Src : in T_Le; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;

   -- Get the size of the already serialized type inside of the byte array. If the byte array is too small then
   -- a serialization error is returned.
   function Serialized_Length (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;
   function Serialized_Length_Le (Src : in Basic_Types.Byte_Array; Num_Bytes_Serialized : out Natural) return Serialization_Status
      with Inline => True;
{% endif %}

   -- Return a field (provided by a field number) as a polymorphic type.
   -- This is useful for returning any field in a record in a very generic
   -- way. Fields bigger than the polymorphic type will only have their
   -- least signficant bits returned. This function should be used in tandem
   -- with the Validation package to create useful error messages for an invalid
   -- type:
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type;
   function Get_Field (Src : in T_Le; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type;
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
