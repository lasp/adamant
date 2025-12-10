--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with System;
{% if length %}
with Serializer;
{% endif %}
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

   --
   --  Packed array type trees:
{% if endianness in ["either", "big"] %}
   --
   --  Unconstrained --> U
   --  |                 ^ constrained and unpacked
   --  v
   --  T_Unconstrained --> T -----------> Volatile_T, Atomic_T, Register_T
   --  | ^ BE packed       ^ constrained  ^ by-reference constrained
   --  v
   --  Volatile_T_Unconstrained, Atomic_T_Unconstrained, Register_T_Unconstrained
   --    ^ by-reference unconstrained
{% endif %}
{% if endianness in ["either", "little"] %}
   --
   --  Unconstrained --> U
   --  |                 ^ constrained and unpacked
   --  v
   --  T_Le_Unconstrained --> T_Le --------> Volatile_T_Le, Atomic_T_Le, Register_T_Le
   --  | ^ LE packed          ^ constrained  ^ by-reference constrained
   --  v
   --  Volatile_T_Le_Unconstrained, Atomic_T_Le_Unconstrained, Register_T_Le_Unconstrained
   --    ^ by-reference unconstrained
{% endif %}
   --
{% if not length %}
   pragma Elaborate_Body;
{% endif %}
{% if preamble %}

   -- Preamble code:
{{ printMultiLine(preamble, '   ', 10000) }}
{% endif %}

   -- Array element size (in bits and bytes):
{% if element.is_packed_type %}
   Element_Size : constant Positive := {{ element.type_package }}.Size;
   Element_Size_In_Bytes : constant Positive := {{ element.type_package }}.Size_In_Bytes;
{% else %}
   Element_Size : constant Positive := {{ element.size }};
   Element_Size_In_Bytes : constant Positive := (Element_Size - 1) / Basic_Types.Byte'Object_Size + 1;
{% endif %}
{% if length %}

   -- Packed type size (in bits):
   Length : constant Natural := {{ length }};
   Size : constant Positive := Element_Size * Length;
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
   pragma Warnings (Off, "scalar storage order specified for ""T_Unconstrained"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T_Unconstrained"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
   pragma Warnings (Off, "scalar storage order specified for ""T_Le_Unconstrained"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T_Le_Unconstrained"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""T_Le"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Volatile_T_Le"" does not apply to component");
{% endif %}
{% if element.size == 32 and not element.is_packed_type %}
{% if endianness in ["either", "big"] %}
   pragma Warnings (Off, "scalar storage order specified for ""Atomic_T_Unconstrained"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Register_T_Unconstrained"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Atomic_T"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Register_T"" does not apply to component");
{% endif %}
{% if endianness in ["either", "little"] %}
   pragma Warnings (Off, "scalar storage order specified for ""Atomic_T_Le_Unconstrained"" does not apply to component");
   pragma Warnings (Off, "scalar storage order specified for ""Register_T_Le_Unconstrained"" does not apply to component");
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
{% if length %}

   -- Unpacked constrained array type:
   subtype U is Unconstrained (Constrained_Index_Type);

   -- Access type for U
   type U_Access is access all U;
{% endif %}

{% if endianness in ["either", "big"] %}
{% if (element.size % 8) == 0 %}
   -- Packed unconstrained type definition.
{% if element.is_packed_type %}
   type T_Unconstrained is array (Unconstrained_Index_Type range <>) of {{ element.type_package }}.T
{% else %}
   type T_Unconstrained is new Unconstrained
{% endif %}
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.High_Order_First,
           Volatile => False,
           Volatile_Components => False;

   -- Access type for T_Unconstrained.
   type T_Unconstrained_Access is access all T_Unconstrained;

{% endif %}
{% if length %}
   -- Packed constrained type definition.
{% if element.is_packed_type %}
   type T is new T_Unconstrained (Constrained_Index_Type)
{% elif (element.size % 8) == 0 %}
   type T is new T_Unconstrained (Constrained_Index_Type)
{% else %}
   type T is new U
{% endif %}
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => False,
           Volatile_Components => False;

   -- Access type for T
   type T_Access is access all T;

{% endif %}
{% endif %}
{% if endianness in ["either", "little"] %}
{% if (element.size % 8) == 0 %}
   -- Packed unconstrained type with little endian definition.
{% if element.is_packed_type %}
   type T_Le_Unconstrained is array (Unconstrained_Index_Type range <>) of {{ element.type_package }}.T_Le
{% else %}
   type T_Le_Unconstrained is new Unconstrained
{% endif %}
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
           Volatile => False,
           Volatile_Components => False;

   -- Access type for T_Unconstrained.
   type T_Le_Unconstrained_Access is access all T_Le_Unconstrained;

{% endif %}
{% if length %}
   -- Packed type definition with little endian definition.
{% if element.is_packed_type %}
   type T_Le is new T_Le_Unconstrained (Constrained_Index_Type)
{% elif (element.size % 8) == 0 %}
   type T_Le is new T_Le_Unconstrained (Constrained_Index_Type)
{% else %}
   type T_Le is new U
{% endif %}
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => False,
           Volatile_Components => False;

   -- Access type for T_Le
   type T_Le_Access is access all T_Le;

{% endif %}
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

   -- {{ volatile_descriptor }} unconstrained type with big endian definition:
   type {{ volatile_descriptor }}_T_Unconstrained is array (Unconstrained_Index_Type range <>) of {{ element.type }}
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
           Volatile => True,
           Volatile_Components => True;

   -- Access type for {{ volatile_descriptor }}_T_Unconstrained
   type {{ volatile_descriptor }}_T_Unconstrained_Access is access all {{ volatile_descriptor }}_T_Unconstrained;

   -- {{ volatile_descriptor }} constrained type with big endian definition:
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

   -- Access type for {{ volatile_descriptor }}_T
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

   -- {{ volatile_descriptor }} unconstrained type with little endian definition:
   type {{ volatile_descriptor }}_T_Le_Unconstrained is array (Unconstrained_Index_Type range <>) of {{ element.type }}
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
           Volatile => True,
           Volatile_Components => True;

   -- Access type for {{ volatile_descriptor }}_T_Le_Unconstrained
   type {{ volatile_descriptor }}_T_Le_Unconstrained_Access is access all {{ volatile_descriptor }}_T_Le_Unconstrained;

   -- {{ volatile_descriptor }} constrained type with little endian definition:
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

   -- Access type for {{ volatile_descriptor }}_T_Le
   type {{ volatile_descriptor }}_T_Le_Access is access all {{ volatile_descriptor }}_T_Le;

{% endif %}
{% else %}
{% if (element.size % 8) == 0 and endianness in ["either", "big"] %}
   -- Volatile unconstrained type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T_Unconstrained is new T_Unconstrained
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.High_Order_First,
           Volatile => True,
           Volatile_Components => True;

   -- Access type for Volatile_T_Unconstrained
   type Volatile_T_Unconstrained_Access is access all Volatile_T_Unconstrained;

{% endif %}
{% if length and endianness in ["either", "big"] %}
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
           Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => True,
           Volatile_Components => True;

   -- Access type for Volatile_T
   type Volatile_T_Access is access all Volatile_T;

{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "little"] %}
   -- Volatile unconstrained type definition:
   -- Note: This type is volatile. You should use this type to specify that the
   -- variable in question may suddenly change in value. For example, this may
   -- occur due to a device writing to a shared buffer. When this pragma is used,
   -- the compiler must suppress any optimizations that would interfere with the
   -- correct reading of the volatile variables. For example, two successive
   -- readings of the same variable cannot be optimized to just one or reordered.
   -- Important: You should use the Register type for accessing hardware registers.
   type Volatile_T_Le_Unconstrained is new T_Le_Unconstrained
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.High_Order_First,
           Volatile => True,
           Volatile_Components => True;

   -- Access type for Volatile_T_Le_Unconstrained
   type Volatile_T_Le_Unconstrained_Access is access all Volatile_T_Le_Unconstrained;

{% endif %}
{% if length and endianness in ["either", "little"] %}
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
           Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => True,
           Volatile_Components => True;

   -- Access type for Volatile_T_Le
   type Volatile_T_Le_Access is access all Volatile_T_Le;

{% endif %}
{% if element.size == 32 and not element.is_packed_type %}
   --
   -- Atomic type definitions. These are only supported for array types that
   -- have components that are exactly 32 bits in size and are not themselves
   -- packed types.
   --
{% if (element.size % 8) == 0 and endianness in ["either", "big"] %}
   -- Atomic unconstrained type definition:
   -- Note: This type is atomic. Use this type to specify that the code
   -- generated must read and write the type or variable from memory atomically,
   -- i.e. as a single/non-interruptible operation. It implies pragma Volatile,
   -- the difference is that pragma Atomic is stronger: the compilation must
   -- fail if the variable cannot be updated atomically.
   -- Important: Atomic types create a synchronization point and can be used for
   -- very limited intertask communication. However, protected objects should almost
   -- always be preferred.
   type Atomic_T_Unconstrained is new T_Unconstrained
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.High_Order_First,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Atomic_T_Unconstrained
   type Atomic_T_Unconstrained_Access is access all Atomic_T_Unconstrained;

{% endif %}
{% if length and endianness in ["either", "big"] %}
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
           Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Atomic_T
   type Atomic_T_Access is access all Atomic_T;

{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "little"] %}
   -- Atomic unconstrained type with little endian definition:
   -- Note: This type is atomic. Use this type to specify that the code
   -- generated must read and write the type or variable from memory atomically,
   -- i.e. as a single/non-interruptible operation. It implies pragma Volatile,
   -- the difference is that pragma Atomic is stronger: the compilation must
   -- fail if the variable cannot be updated atomically.
   -- Important: Atomic types create a synchronization point and can be used for
   -- very limited intertask communication. However, protected objects should almost
   -- always be preferred.
   type Atomic_T_Le_Unconstrained is new T_Le_Unconstrained
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Atomic_T_Le_Unconstrained
   type Atomic_T_Le_Unconstrained_Access is access all Atomic_T_Le_Unconstrained;

{% endif %}
{% if length and endianness in ["either", "little"] %}
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
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Atomic_T_Le
   type Atomic_T_Le_Access is access all Atomic_T_Le;

{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "big"] %}
   -- Register unconstrained type definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T_Unconstrained is new T_Unconstrained
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.High_Order_First,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Register_T_Unconstrained
   type Register_T_Unconstrained_Access is access all Register_T_Unconstrained;

{% endif %}
{% if length and endianness in ["either", "big"] %}
   -- Register packed type definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T is new T
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.High_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Register_T
   type Register_T_Access is access all Register_T;

{% endif %}
{% if (element.size % 8) == 0 and endianness in ["either", "little"] %}
   -- Register unconstrained type with little endian definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T_Le_Unconstrained is new T_Le_Unconstrained
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Register_T_Le_Unconstrained
   type Register_T_Le_Unconstrained_Access is access all Register_T_Le_Unconstrained;

{% endif %}
{% if length and endianness in ["either", "little"] %}
   -- Register little endian packed type definition:
   -- Note: This type is the same as Atomic for arrays with component sizes of 32-bits.
   -- Important: You should use a Register type for accessing memory mapped IO or
   -- hardware registers.
   type Register_T_Le is new T_Le
      with Component_Size => Element_Size,
           Scalar_Storage_Order => System.Low_Order_First,
{% if ((element.size % 8) == 0) or (element.size in [0, 1, 2]) %}
           Size => Size,
{% endif %}
           Object_Size => Size,
           Volatile => True,
           Volatile_Components => True,
           Atomic_Components => True;

   -- Access type for Register_T_Le
   type Register_T_Le_Access is access all Register_T_Le;

{% endif %}
{% else %}
   -- Atomic and Register type not supported.
   -- This type has array components that are {{ element.size }} bits in size.

{% endif %}
{% endif %}
{% if element.format.length and element.format.length > 1 %}
{% if endianness in ["either", "big"] %}
   pragma Warnings (On, "scalar storage order specified for ""T"" does not apply to component");
   pragma Warnings (On, "scalar storage order specified for ""T_Unconstrained"" does not apply to component");
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
{% if length %}
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

{% else %}
   -- Type conversion functions between packed an unpacked representations for unconstrained arrays:
{% if endianness in ["either", "big"] %}
   function Pack (Src : in Unconstrained) return T_Unconstrained{% if not element.is_packed_type %} with Inline => True{% endif %};
   function Unpack (Src : in T_Unconstrained) return Unconstrained{% if not element.is_packed_type %} with Inline => True{% endif %};
{% endif %}
{% if endianness in ["either", "little"] %}
   function Pack (Src : in Unconstrained) return T_Le_Unconstrained{% if not element.is_packed_type %} with Inline => True{% endif %};
   function Unpack (Src : in T_Le_Unconstrained) return Unconstrained{% if not element.is_packed_type %} with Inline => True{% endif %};
{% endif %}
{% if endianness in ["either"] %}

   -- Endianness conversion functions for unconstrained arrays
   function Swap_Endianness (Src : in T_Unconstrained) return T_Le_Unconstrained{% if not element.is_packed_type %} with Inline => True{% endif %};
   function Swap_Endianness (Src : in T_Le_Unconstrained) return T_Unconstrained{% if not element.is_packed_type %} with Inline => True{% endif %};
{% endif %}

{% endif %}
{% if length %}
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
   ----------------------------------------------------
   -- Named subtypes for array element for convenience:
   ----------------------------------------------------

{% if element.is_packed_type %}
   subtype Element_Type_U is {{ element.type_package }}.U;
{% if endianness in ["either", "big"] %}
   subtype Element_Type_T is {{ element.type_package }}.T;
{% endif %}
{% if endianness in ["either", "little"] %}
   subtype Element_Type_T_Le is {{ element.type_package }}.T_Le;
{% endif %}
{% else %}
   {% if ("Element_Type") == element.type %}-- {% endif %}subtype Element_Type is {{ element.type }};
{% endif %}

private

   --
   -- The unconstrained array types above do not technically guarantee that there are
   -- "no gaps" between components of the array. However, in Ada RM Implementation
   -- Advice (13.3(71-73)) it says:
   --
   --   https://www.adaic.org/resources/add_content/standards/12rm/html/RM-13-3.html
   --
   --   An implementation should support specified Component_Sizes that are factors
   --   and multiples of the word size. For such Component_Sizes, the array should
   --   contain no gaps between components. For other Component_Sizes (if supported),
   --   the array should contain no gaps between components when Pack is also specified;
   --   the implementation should forbid this combination in cases where it cannot
   --   support a no-gaps representation.
   --
   -- The word "should" is used here, not "shall". For GNAT in the RM it is
   -- explicitly recognized that the advice is followed. Since that is the most
   -- common Ada implementation used with Adamant, we can assume that the desired
   -- packing is respected. See:
   --
   --   https://gcc.gnu.org/onlinedocs/gnat_rm/RM-13-3-71-73-Component-Size-Clauses.html
   --
   -- In order to verify this, we can use the following compile-time checks:
   --
{% if (element.size % 8) == 0 %}
{% if endianness in ["either", "big"] %}

   subtype T_Unconstrained_5 is T_Unconstrained (0 .. 4);

   pragma Compile_Time_Error (T_Unconstrained'Component_Size /= Element_Size,
      "T_Unconstrained component size must equal element size (no padding)");
   pragma Compile_Time_Error (T_Unconstrained_5'Object_Size /= 5 * Element_Size,
      "T_Unconstrained_5 object size must be 5 * element size (no gaps between components)");
{% endif %}
{% if endianness in ["either", "little"] %}

   subtype T_Le_Unconstrained_5 is T_Le_Unconstrained (0 .. 4);

   pragma Compile_Time_Error (T_Le_Unconstrained'Component_Size /= Element_Size,
      "T_Le_Unconstrained component size must equal element size (no padding)");
   pragma Compile_Time_Error (T_Le_Unconstrained_5'Object_Size /= 5 * Element_Size,
      "T_Le_Unconstrained_5 object size must be 5 * element size (no gaps between components)");
{% endif %}
{% endif %}

end {{ name }};
