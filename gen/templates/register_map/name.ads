--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
{% if includes %}

-- Custom Includes:
{% for include in includes %}
{% if include not in ["System", "Interfaces"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if type_includes %}

-- Item Includes:
{% for include in type_includes %}
{% if include not in includes and include not in ["System", "Interfaces"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if description %}

{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }}
   -- We apply SPARK to this package to help us discover bit-constrained types in
   -- the register map. These types should be avoided in register maps at all costs since
   -- simply reading a type from the register map that got corrupted to a value that
   -- is out of range could be a mission ending failure.
   --
   -- If a bit-constrained type is detected by SPARK the following "high" warning will
   -- be issued as part of the analysis:
   --
   --    object with constraints on bit representation is unsuitable for aliasing
   --    via address clause
   --
   -- Any line with this issue present should be fixed by modifying the underlying type
   -- to NOT be bit-constrained.
   with SPARK_Mode => On
is
{% if preamble %}

   -- Preamble code:
{{ printMultiLine(preamble, '   ', 10000) }}
{% endif %}

   --
   -- Register map constants:
   --

   -- Memory map num items ({{ length }}):
   Num_Items : constant Natural := {{ num_items }};

   --
   -- Register map items:
   --

{% for item_name, item in items.items() %}
   -- 0x{{'%08X' % item.address}} - {{'%08X' % (item.address + item.size - 1)}}: {{ item.name }}
{% if item.description %}
{{ printMultiLine(item.description, '   -- ') }}
{% endif %}
   {{ item.name }}_Offset : constant Unsigned_32 := 16#{{ '%08X' % item.address }}#; -- 0x{{ '%08X' % item.address }}
   {{ item.name }}_Size : constant Unsigned_32 := Unsigned_32 ({{ item.type_package }}.Size_In_Bytes); -- {{ item.size }} bytes
   {{ item.name }}_End : constant Unsigned_32 := {{ item.name }}_Offset + {{ item.name }}_Size - 1; -- 0x{{ '%08X' % (item.address + item.size - 1) }}
   {{ item.name }}_Address : constant System.Address := To_Address (Integer_Address ({{ item.name }}_Offset));
   pragma Warnings (Off, "writing to ""{{ item.name }}"" is assumed to have no effects on other non-volatile objects");
   pragma Warnings (Off, "assuming no concurrent accesses to non-atomic object ""{{ item.name }}""");
   {{ item.name }} : aliased {{ item.type }}
      with {% if item.volatile_aspect %}{{ item.volatile_aspect }} => True, {% endif %}Import, Convention => Ada, Address => {{ item.name }}_Address;
   pragma Warnings (On, "assuming no concurrent accesses to non-atomic object ""{{ item.name }}""");
   pragma Warnings (On, "writing to ""{{ item.name }}"" is assumed to have no effects on other non-volatile objects");

{% endfor %}
   --
   -- Register map access types:
   --
   package Accesses
      -- Note that general access types are not allowed in SPARK,
      -- but we may want to use these in Ada code, so we provide them here.
      with SPARK_Mode => Off
   is
{% for item_name, item in items.items() %}
      {{ item.name }}_Access : constant {{ item.type }}_Access := {{ item.name }}'Access;
{% endfor %}
   end Accesses;

   --
   -- Register map compile-time checks. These checks helpt to ensure that there is not a bug in the autocoder.
   --

{% for item_name, item in items.items() %}
   -- 0x{{'%08X' % item.address}} - {{'%08X' % (item.address + item.size - 1)}}: {{ item.name }} Checks
   pragma Compile_Time_Error ({{ item.name }}_Offset /= 16#{{ '%08X' % item.address }}#, "Unexpected autocoder error. Item start address not as expected.");
   pragma Compile_Time_Error ({{ item.name }}_Size /= {{ item.size }}, "Unexpected autocoder error. Item size not as expected.");
   pragma Compile_Time_Error ({{ item.name }}_End /= 16#{{ '%08X' % (item.address + item.size - 1) }}#, "Unexpected autocoder error. Item end address not as expected.");
   pragma Compile_Time_Error ({{ item.name }}_Offset >= {{ item.name }}_End, "Unexpected autocoder error. Item end address less than start address.");
{% if item.prev_item %}
   pragma Compile_Time_Error ({{ item.name }}_Offset <= {{ item.prev_item.name }}_End, "Unexpected autocoder error. Item overlap found.");
   pragma Compile_Time_Error ({{ item.name }}_Offset <= {{ item.prev_item.name }}_Offset, "Unexpected autocoder error. Item overlap found.");
{% endif %}

{% endfor %}
end {{ name }};
