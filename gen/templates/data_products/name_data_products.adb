--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if basic_types %}

-- Standard Includes:
with Serializer;
{% endif %}

package body {{ name }} is

   -----------------------------------------------
   -- Getter/setter subprograms for data product ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Data_Product_Types.Data_Product_Id is
   begin
      return Self.Id_Base;
   end Get_Id_Base;

   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Data_Product_Types.Data_Product_Id) is
   begin
{% if (data_products|length) > 1 %}
      -- ID base set too high for data product ID set. This is checked in the assembly model.
      pragma Assert (Natural (Id_Base) + {{ (data_products|length) - 1 }} <= Natural (Data_Product_Types.Data_Product_Id'Last));
{% endif %}
      Self.Id_Base := Id_Base;
   end Set_Id_Base;

   -----------------------------------------------
   -- Getter function for global data product IDs:
   -----------------------------------------------
{% for dp in data_products %}
   not overriding function Get_{{ dp.name }}_Id (Self : in Instance) return Data_Product_Types.Data_Product_Id is
      use Data_Product_Types;
   begin
      return Self.Id_Base + Local_Data_Product_Id_Type'Enum_Rep ({{ dp.name }}_Id);
   end Get_{{ dp.name }}_Id;

{% endfor %}
   -----------------------------------------------
   -- Data product creation functions:
   -----------------------------------------------
{% for dp in data_products %}
   not overriding function {{ dp.name }} (Self : in Instance; Timestamp : Sys_Time.T; Item : in {{ dp.type }}) return Data_Product.T is
{% if dp.type_model %}
      package Data_Product_Serializer renames {{ dp.type_package }}.Serialization;
{% else %}
      package Data_Product_Serializer is new Serializer ({{ dp.type }});
{% endif %}
      Dp : Data_Product.T := (Header => (Id => Self.Get_{{ dp.name }}_Id, Time => Timestamp, Buffer_Length => Data_Product_Serializer.Serialized_Length), Buffer => (others => 0));
   begin
      Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Data_Product_Serializer.Serialized_Length - 1) := Data_Product_Serializer.To_Byte_Array (Item);
      return Dp;
   end {{ dp.name }};

{% endfor %}
end {{ name }};
