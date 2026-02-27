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
   -- the memory map. These types should be avoided in memory maps at all costs since
   -- simply reading a type from the memory map that got corrupted to a value that
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
   -- Memory map constants:
   --

   -- Memory map start (0x{{ '%08X' % start_address }}):
   Start_Offset : constant Unsigned_32 := 16#{{ '%08X' % start_address }}#; -- 0x{{ '%08X' % start_address }}
   Start_Address : constant System.Address := To_Address (Integer_Address (Start_Offset));

   -- Memory map size ({{ length }}):
   Length : constant Unsigned_32 := {{ length }}; -- 0x{{ '%08X' % length }} bytes
   Num_Items : constant Natural := {{ num_items }};

   -- Memory map end address (0x{{ '%08X' % (start_address + length - 1) }}):
   Last_Offset : constant Unsigned_32 := Start_Offset + Length - 1;
   Last_Address : constant System.Address := To_Address (Integer_Address (Last_Offset)); -- 0x{{ '%08X' % (start_address + length - 1) }}

   -- Memory map last used address (0x{{ '%08X' % last_used_address }}):
   Last_Used_Offset : constant Unsigned_32 := 16#{{ '%08X' % last_used_address }}#; -- 0x{{ '%08X' % last_used_address }}
   Last_Used_Address : constant System.Address := To_Address (Integer_Address (Last_Used_Offset));

   -- Calculate the space available:
   Used_Bytes : constant Unsigned_32 := Last_Used_Offset - Start_Offset + 1; -- {{ last_used_address - start_address + 1 }} bytes
   Unused_Bytes : constant Unsigned_32 := Last_Offset - Last_Used_Offset; -- {{ start_address + length - 1 - last_used_address }} bytes

   --
   -- Memory map items:
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

   pragma Warnings (GNATProve, Off, "[imprecise-address-specification]", Reason => "Intentional MMIO mapping for Time");
   pragma Warnings (Off, "indirect writes to ""{{ item.name }}"" through a potential alias are ignored");
   pragma Warnings (Off, "writing to ""{{ item.name }}"" is assumed to have no effects on other non-volatile objects");
   pragma Warnings (Off, "assuming no concurrent accesses to non-atomic object ""{{ item.name }}""");
   {{ item.name }} : aliased {{ item.type }}
      with Import, Convention => Ada, Address => {{ item.name }}_Address;
   pragma Warnings (On, "assuming no concurrent accesses to non-atomic object ""{{ item.name }}""");
   pragma Warnings (On, "writing to ""{{ item.name }}"" is assumed to have no effects on other non-volatile objects");
   pragma Warnings (On, "indirect writes to ""{{ item.name }}"" through a potential alias are ignored");
   pragma Warnings (GNATProve, On, "[imprecise-address-specification]", Reason => "Intentional MMIO mapping for Time");

{% endfor %}
   --
   -- Memory map access types:
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
   -- Memory map compile-time checks. These checks help to ensure that there is not a bug in the autocoder.
   --

   -- Make sure map boundary addresses makes sense:
   pragma Compile_Time_Error (Start_Offset /= 16#{{ '%08X' % start_address }}#, "Unexpected autocoder error. Start address not as expected.");
   pragma Compile_Time_Error (Last_Offset /= 16#{{ '%08X' % (start_address + length - 1) }}#, "Unexpected autocoder error. End address not as expected.");
   pragma Compile_Time_Error (Last_Used_Offset /= 16#{{ '%08X' % last_used_address }}#, "Unexpected autocoder error. Last used address not as expected.");
   pragma Compile_Time_Error (Last_Offset < Start_Offset, "Unexpected autocoder error. End address smaller than start address.");
   pragma Compile_Time_Error (Last_Used_Offset > Last_Offset, "Unexpected autocoder error. Last used address larger than last address.");
   pragma Compile_Time_Error (Unused_Bytes /= {{ start_address + length - 1 - last_used_address }}, "Unexpected autocoder error. Unused bytes not as expected.");
   pragma Compile_Time_Error (Used_Bytes /= {{ last_used_address - start_address + 1}}, "Unexpected autocoder error. Used bytes not as expected.");

{% for item_name, item in items.items() %}
   -- 0x{{'%08X' % item.address}} - {{'%08X' % (item.address + item.size - 1)}}: {{ item.name }} Checks
   pragma Compile_Time_Error ({{ item.name }}_Offset /= 16#{{ '%08X' % item.address }}#, "Unexpected autocoder error. Item start address not as expected.");
   pragma Compile_Time_Error ({{ item.name }}_Size /= {{ item.size }}, "Unexpected autocoder error. Item size not as expected.");
   pragma Compile_Time_Error ({{ item.name }}_End /= 16#{{ '%08X' % (item.address + item.size - 1) }}#, "Unexpected autocoder error. Item end address not as expected.");
   pragma Compile_Time_Error ({{ item.name }}_Offset >= {{ item.name }}_End, "Unexpected autocoder error. Item end address less than start address.");
   pragma Compile_Time_Error ({{ item.name }}_Offset < Start_Offset, "Unexpected autocoder error. Item starts before first address.");
   pragma Compile_Time_Error ({{ item.name }}_End > Last_Offset, "Unexpected autocoder error. Item ends after last address.");
{% if item.prev_item %}
   pragma Compile_Time_Error ({{ item.name }}_Offset <= {{ item.prev_item.name }}_End, "Unexpected autocoder error. Item overlap found.");
   pragma Compile_Time_Error ({{ item.name }}_Offset <= {{ item.prev_item.name }}_Offset, "Unexpected autocoder error. Item overlap found.");
{% else %}
   pragma Compile_Time_Error ({{ item.name }}_Offset < Start_Offset, "Unexpected autocoder error. Item overlap found.");
{% endif %}

{% endfor %}
end {{ name }};
