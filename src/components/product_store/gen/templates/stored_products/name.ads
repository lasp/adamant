-- Standard includes:
with Product_Store_Types; use Product_Store_Types;

{% if description %}
{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   -- Total store size in bytes, including the CRC and save time header:
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
