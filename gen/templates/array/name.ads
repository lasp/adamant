--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with System;
with Serializer;
{% if not element.is_packed_type %}
with Basic_Types;
{% endif %}
{% if includes %}

-- Other Includes:
{% for include in includes %}
{% if include not in ["Serializer", "System"] %}
{% if element.is_packed_type or include not in ["Basic_Types"] %}
with {{ include }};
{% endif %}
{% endif %}
{% endfor %}
{% endif %}
{% if type_includes %}

-- Type Includes:
{% for include in type_includes %}
{% if include not in includes and include not in ["Serializer", "System"] %}
{% if element.is_packed_type or include not in ["Basic_Types"] %}
with {{ include }};
{% endif %}
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
{% if element.format.length and element.format.length > 1 %}

   -- We can safely ignore scalar storage order warnings for components <8 bits in
   -- size since endianness does not apply.
{% if endianness in ["either", "big"] %}
   pragma Warnings (Off, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
   pragma Warnings (Off, "scalar storage order specified for ""T_Le"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% endif %}
{% if element.size == 32 and not element.is_packed_type %}
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

   -- Unconstrained base type:
{% if element.is_packed_type %}
   type Unconstrained is array (Unconstrained_Index_Type range <>) of {{ element.type_package }}.U;
{% else %}
   type Unconstrained is array (Unconstrained_Index_Type range <>) of {{ element.type }};
{% endif %}

   -- Unpacked array type:
   subtype U is Unconstrained{% if length %} (Constrained_Index_Type){% endif %};

   -- Access type for U
   type U_Access is access all U;

{% if endianness in ["either", "big"] %}
   -- Packed type definition.
{% if element.is_packed_type %}
   type T is array (Constrained_Index_Type) of {{ element.type_package }}.T
{% else %}
   type T is new U
{% endif %}
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

   -- Access type for T
   type T_Access is access all T;

{% endif %}
{% if endianness in ["either", "little"] %}
   -- Packed type definition with little endian definition.
{% if element.is_packed_type %}
   type T_Le is array (Constrained_Index_Type) of {{ element.type_package }}.T_Le
{% else %}
   type T_Le is new U
{% endif %}
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

   -- Access type for T_Le
   type T_Le_Access is access all T_Le;

{% endif %}
{% if volatile_descriptor %}
   --
   -- Note: This {{ volatile_descriptor|lower }} type is created because the model is defined with all {{ volatile_descriptor|lower }} fields.
   --

{% if endianness in ["either", "big"] %}
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

   -- Create access type to packed and unpacked records:
   type {{ volatile_descriptor }}_T_Access is access all {{ volatile_descriptor }}_T;
{% endif %}
{% if endianness in ["either", "little"] %}
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

   -- Access type for Volatile_T_Le
   type Volatile_T_Le_Access is access all Volatile_T_Le;

{% endif %}
   --
   -- Atomic type definitions. These are only supported for array types that
   -- have components that are exactly 32 bits in size and are not themselves
   -- packed types.
   --

{% if element.size == 32 and not element.is_packed_type %}
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
   type Atomic_T_Le is new T
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

   -- Access type for Atomic_T_Le
   type Atomic_T_Le_Access is access all Atomic_T_Le;

{% endif %}
{% if endianness in ["either", "big"] %}
   -- Register packed type definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T is new T
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

   -- Access type for Register_T
   type Register_T_Access is access all Register_T;

{% endif %}
{% if endianness in ["either", "little"] %}
   -- Register little endian packed type definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T_Le is new T_Le
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

   -- Access type for Register_T_Le
   type Register_T_Le_Access is access all Register_T_Le;

{% endif %}
{% else %}
   -- Not supported. This type has array components that are {{ element.size }} bits in size.

{% endif %}
{% endif %}
{% if element.format.length and element.format.length > 1 %}
{% if endianness in ["either", "big"] %}
   pragma Warnings (On, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Volatile_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
   pragma Warnings (On, "scalar storage order specified for ""T_Le"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% endif %}
{% if element.size == 32 and not element.is_packed_type %}
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
   -- Type conversion functions between packed an unpacked representations:
{% if endianness in ["either", "big"] %}
   function Pack (Src : in U) return T{% if not element.is_packed_type %} with Inline => True{% endif %};
   function Unpack (Src : in T) return U{% if not element.is_packed_type %} with Inline => True{% endif %};
{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in U) return T_Le{% if not element.is_packed_type %} with Inline => True{% endif %};
   function Unpack (Src : in T_Le) return U{% if not element.is_packed_type %} with Inline => True{% endif %};
{% endif %}
{% if endianness in ["either"] %}

   -- Endianness conversion functions
   function Swap_Endianness (Src : in T) return T_Le{% if not element.is_packed_type %} with Inline => True{% endif %};
   function Swap_Endianness (Src : in T_Le) return T{% if not element.is_packed_type %} with Inline => True{% endif %};
{% endif %}

   -- Serializing functions for entire packed array:
{% if endianness in ["either", "big"] %}
   package Serialization is new Serializer (T);
{% endif %}
{% if endianness in ["either", "little"] %}
   package Serialization_Le is new Serializer (T_Le);
{% endif %}

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

end {{ name }};
