--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Data_Product;
with Data_Product_Types;
with Sys_Time;
with Ada.Real_Time;
{% if includes %}

-- Data Product Type Includes
{% for include in includes %}
{% if include not in ["Sys_Time", "Data_Product", "Data_Product_Types"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if description %}

{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   -- Object instance type:
   type Instance is tagged limited private;

   -- Status type for extraction functions:
   type Status is (Success, Id_Error, Length_Error, Stale);

   -- Constants:
   Num_Data_Dependencies : constant Natural := {{ data_dependencies|length }};

   -----------------------------------------------
   -- Setter procedure for data product IDs:
   -----------------------------------------------
   not overriding procedure Set_Ids_And_Limits (
      Self : in out Instance;
{% for dd in data_dependencies %}
      {{ dd.name }}_Id : in Data_Product_Types.Data_Product_Id;
      {{ dd.name }}_Stale_Limit : in Ada.Real_Time.Time_Span{{ ";" if not loop.last }}
{% endfor %}
   );

   -----------------------------------------------
   -- Getter function for data product IDs:
   -----------------------------------------------
{% for dd in data_dependencies %}
   not overriding function Get_{{ dd.name }}_Id (Self : in Instance) return Data_Product_Types.Data_Product_Id
      with Inline => True;
{% endfor %}

   -----------------------------------------------
   -- Data dependency extraction functions:
   -----------------------------------------------
{% for dd in data_dependencies %}
{% if dd.description %}
{{ printMultiLine(dd.description, '   -- ') }}
{% endif %}
   not overriding function Extract_{{ dd.name }} (Self : in Instance; Product : in Data_Product.T; Stale_Reference : in Sys_Time.T; Timestamp : out Sys_Time.T; Item : out {{ dd.type }}) return Status;

{% endfor %}
   -- Compile time checks to make sure types do not serialize longer than the data product buffer size:
   pragma Warnings (Off, "condition can only be True if invalid values present");
{% for dd in data_dependencies %}
{% if dd.type_model %}
   pragma Compile_Time_Error (
      {{ dd.type_package }}.Size_In_Bytes > Data_Product_Types.Data_Product_Buffer_Type'Length,
      "Data Product '{{ dd.name }}' has argument of type '{{ dd.type }}' which has a maximum serialized length larger than the buffer size of Data_Product.T."
   );
{% else %}
   pragma Compile_Time_Error (
      (({{ dd.type   }}'Object_Size - 1) / 8 + 1) > Data_Product_Types.Data_Product_Buffer_Type'Length,
      "Data_Product '{{ dd.name }}' has argument of type '{{ dd.type }}' which has a maximum serialized length larger than the buffer size of Data_Product.T."
   );
{% endif %}
{% endfor %}
   pragma Warnings (On, "condition can only be True if invalid values present");

private
   type Instance is tagged limited record
      -- Default data dependency IDs just increment from zero for unit testing.
      -- These will get overridden by the Set_Ids_And_Limits procedure in a running assembly.
      -- The stale limits will be set to 1 second, which is just a general assumption
      -- we can make for unit testing. This will also get overridden by the
      -- Set_Ids_And_Limits procedure in a running assembly.
{% for dd in data_dependencies %}
      {{ dd.name }}_Id : Data_Product_Types.Data_Product_Id := {{ loop.index0 }};
      {{ dd.name }}_Stale_Limit : Ada.Real_Time.Time_Span := Ada.Real_Time.Seconds (1);
{% endfor %}
   end record;

end {{ name }};
