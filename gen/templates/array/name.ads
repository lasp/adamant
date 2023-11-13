--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with System;
{% if not is_volatile_type %}
with Interfaces;
with Basic_Types;
with Serializer;
{% endif %}
{% if includes %}

-- Other Includes:
{% for include in includes %}
{% if include not in ["Basic_Types", "Serializer", "System", "Interfaces"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if type_includes %}

-- Type Includes:
{% for include in type_includes %}
{% if include not in includes and include not in ["Basic_Types", "Serializer", "System", "Interfaces"] %}
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
   Length : constant Natural := {{ length }};
{% if element.is_packed_type %}
   Element_Size : constant Positive := {{ element.type_package }}.Size;
{% else %}
   Element_Size : constant Positive := {{ element.size }};
{% endif %}
{% if length %}
   Size : constant Positive := Element_Size * Length;
{% endif %}

   -- Packed type size rounded up to nearest byte.
{% if element.is_packed_type %}
   Element_Size_In_Bytes : constant Positive := {{ element.type_package }}.Size_In_Bytes;
{% else %}
   Element_Size_In_Bytes : constant Positive := (Element_Size - 1) / Basic_Types.Byte'Object_Size + 1;
{% endif %}
{% if length %}
   Size_In_Bytes : constant Positive := (Size - 1) / 8 + 1;
{% endif %}

   -- Array index type:
   subtype Unconstrained_Index_Type is Natural;
{% if length %}
   subtype Constrained_Index_Type is Unconstrained_Index_Type range 0 .. Length - 1;
{% endif %}

   -- Element types that are 8-bit types or 8-bit array types trigger the following warning. Obviously endianness does
   -- not apply for types of 1 byte or less, so we can ignore this warning.
   pragma Warnings (Off, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""T_Le"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% if element.size == 32 and not element.is_packed_type %}
   pragma Warnings (Off, "scalar storage order specified for ""Atomic_T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Atomic_T_Le"" does not apply to component");
{% endif %}

{% if volatile_descriptor %}
   --
   -- Note: This is a {{ volatile_descriptor|lower }} type because it contains {{ volatile_descriptor|lower }} fields. This means
   -- that no "U", "T" or "T_Le" type will be created for it, only a "{{ volatile_descriptor }}_T" and a
   -- "{{ volatile_descriptor }}_T_Le" type. The big endian and little endian types are provided for
   -- symmetry, but it is important to realize that the endianness of the type is
   -- determined by the endianness of the lowest level fields.
   --
   -- Note also that serialization package is not supplied for a {{ volatile_descriptor|lower }} type. To
   -- gain access to a serialization package, define a version of this record
   -- without internal {{ volatile_descriptor|lower }} types, and convert from this type to that type
   -- via overlay or unchecked conversion.
   --

   -- {{ volatile_descriptor }} packed type definition:
   -- Note: This type is {{ volatile_descriptor|lower }}. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the {{ volatile_descriptor|lower }} variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type {{ volatile_descriptor }}_T is array (Constrained_Index_Type) of {{ element.type }}
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.High_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True;

   -- {{ volatile_descriptor }} little endian packed type definition:
   -- Note: This type is {{ volatile_descriptor|lower }}. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the {{ volatile_descriptor|lower }} variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type {{ volatile_descriptor }}_T_Le is array (Constrained_Index_Type) of {{ element.type }}
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.Low_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True;

   -- Create access type to packed and unpacked records:
   type {{ volatile_descriptor }}_T_Access is access all {{ volatile_descriptor }}_T;
   type {{ volatile_descriptor }}_T_Le_Access is access all {{ volatile_descriptor }}_T_Le;

   -- We create this so that an .adb can be generated legally. This will
   -- get optimized out. Volatile packed records do not need regular packed
   -- record .adb.
   procedure Dummy;
{% else %}
   -- Unconstrained base type:
   type Unconstrained is array (Unconstrained_Index_Type range <>) of {{ element.type }};

   -- Unpacked array type:
   subtype U is Unconstrained{% if length %} (Constrained_Index_Type){% endif %};

   -- Packed type definition.
   type T is new U
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.High_Order_First,
{% endif %}
             Volatile => False,
             Volatile_Components => False;

   -- Packed type definition with little endian definition.
   type T_Le is new U
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.Low_Order_First,
{% endif %}
             Volatile => False,
             Volatile_Components => False;

   -- Volatile packed type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T is new U
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.High_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True;

   -- Volatile little endian packed type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T_Le is new U
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.Low_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True;

   --
   -- Atomic type definitions. These are only supported for array types that
   -- have components that are exactly 32 bits in size and are not themselves
   -- packed types.
   --

{% if element.size == 32 and not element.is_packed_type %}
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
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.High_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True,
             Atomic_Components => True;

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
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.Low_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True,
             Atomic_Components => True;

   -- Register packed type definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T is new U
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.High_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True,
             Atomic_Components => True;

   -- Register little endian packed type definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T_Le is new U
      with Component_Size => Element_Size,
{% if length %}
             Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
             Size => Size,
{% endif %}
             Object_Size => Size,
{% else %}
             Scalar_Storage_Order => System.Low_Order_First,
{% endif %}
             Volatile => True,
             Volatile_Components => True,
             Atomic_Components => True;
{% else %}
   -- Not supported. This type has array components that are {{ element.size }} bits in size.
{% endif %}

   -- Re-enable warning.
   pragma Warnings (On, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""T_Le"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Volatile_T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% if element.size == 32 and not element.is_packed_type %}
   pragma Warnings (On, "scalar storage order specified for ""Atomic_T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Atomic_T_Le"" does not apply to component");
{% endif %}

   -- Create access type to packed and unpacked records:
   type U_Access is access all U;
   type T_Access is access all T;
   type T_Le_Access is access all T_Le;
   type Volatile_T_Access is access all Volatile_T;
   type Volatile_T_Le_Access is access all Volatile_T_Le;
{% if element.size == 32 and not element.is_packed_type %}
   type Atomic_T_Access is access all Atomic_T;
   type Atomic_T_Le_Access is access all Atomic_T_Le;
   type Register_T_Access is access all Register_T;
   type Register_T_Le_Access is access all Register_T_Le;
{% endif %}

   -- Serializing functions for entire packed array:
   package Serialization is new Serializer (T);

   -- Return a field (provided by a field number) as a polymorphic type.
   -- This is useful for returning any field in a array in a very generic
   -- way. Fields bigger than the polymorphic type will only have their
   -- least signficant bits returned. This function should be used in tandem
   -- with the Validation package to create useful error messages for an invalid
   -- type:
   function Get_Field (Src : in T; Field : in Interfaces.Unsigned_32) return Basic_Types.Poly_Type;

{% if element.is_packed_type %}
   -- Serializing functions for an element of the array:
   package Element_Serialization renames {{ element.type_package }}.Serialization;
{% else %}
   -- Packed type definition for array element:
   subtype Element_Packed is {{ element.type }}
      with Object_Size => Element_Size_In_Bytes * 8,
             Value_Size => Element_Size_In_Bytes * 8;

   -- Serializing functions for an element of the array:
   package Element_Serialization is new Serializer (Element_Packed);
{% endif %}
{% endif %}

end {{ name }};
