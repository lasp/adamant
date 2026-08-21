-- Standard includes:
with Product_Store_Types; use Product_Store_Types;
{% for pkg in type_packages %}
with {{ pkg }};
{% endfor %}

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

{% for pkg in type_packages %}
   -- Every data product type in the store must be "always valid", meaning no bit
   -- representation of the type can fail validation. This guarantees a restore
   -- can never inject a data product whose use downstream raises a constraint
   -- error. The model checks this at generation time; this check catches a type
   -- that somehow sneaks by (i.e. the type definition changed after generation):
   pragma Compile_Time_Error (not {{ pkg }}.Always_Valid, "Data product type {{ pkg }} must be always valid to be included in a product store.");
{% endfor %}

   -- Size of one copy of the store in bytes, including the CRC, save counter,
   -- and save time header and the per-entry stored length bytes. The component
   -- manages two copies of the store (double buffering), each of this size:
   Store_Size_In_Bytes : constant Natural := {{ store_size }};

   -- Store data product entries:
   Store_Entries : aliased Store_Entry_List_Type := [
{% for entry in entries %}
      -- Entry for {{ entry.name }}:
      {{ loop.index }} => (Data_Product_Id => {{ entry.data_product.id }}, Store_Timestamp => {% if entry.store_timestamp %}True{% else %}False{% endif %}, Restore_Time => Product_Store_Types.{{ entry.restore_time }}, Event_On_Missing => {% if entry.event_on_missing %}True{% else %}False{% endif %}, Size => {{ (entry.size / 8)|int }}){{ "," if not loop.last }}
{% endfor %}
   ];

   -- Store description for the product store component to manage:
   Store_Description : aliased Store_Description_Type := (
      Save_Time => Product_Store_Types.{{ save_time }},
      Entries => Store_Entries'Access,
      Store_Size => Store_Size_In_Bytes
   );

end {{ name }};
