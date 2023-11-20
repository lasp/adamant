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
   -- Setter procedure for data product IDs:
   -----------------------------------------------
   not overriding procedure Set_Ids_And_Limits (
      Self : in out Instance;
{% for dd in data_dependencies %}
      {{ dd.name }}_Id : in Data_Product_Types.Data_Product_Id;
      {{ dd.name }}_Stale_Limit : in Ada.Real_Time.Time_Span{{ ";" if not loop.last }}
{% endfor %}
   ) is
   begin
      -- Copy over IDs into the object:
{% for dd in data_dependencies %}
      Self.{{ dd.name }}_Id := {{ dd.name }}_Id;
{% endfor %}
   end Set_Ids_And_Limits;

   -----------------------------------------------
   -- Getter function for data product IDs:
   -----------------------------------------------
{% for dd in data_dependencies %}
   not overriding function Get_{{ dd.name }}_Id (Self : in Instance) return Data_Product_Types.Data_Product_Id is
   begin
      return Self.{{ dd.name }}_Id;
   end Get_{{ dd.name }}_Id;

{% endfor %}
   -----------------------------------------------
   -- Data dependency extraction functions:
   -----------------------------------------------
{% for dd in data_dependencies %}
   not overriding function Extract_{{ dd.name }} (Self : in Instance; Product : in Data_Product.T; Stale_Reference : in Sys_Time.T; Timestamp : out Sys_Time.T; Item : out {{ dd.type }}) return Status is
      use Data_Product_Types;
      use Ada.Real_Time;
{% if dd.type_model %}
      package Data_Product_Serializer renames {{ dd.type_package }}.Serialization;
{% else %}
      package Data_Product_Serializer is new Serializer ({{ dd.type }});
{% endif %}
   begin
      -- Make sure ID matches what is expected:
      if Product.Header.Id /= Self.{{ dd.name }}_Id then
         return Id_Error;
      end if;

      -- Make sure length matches what is expected:
      if Product.Header.Buffer_Length /= Data_Product_Serializer.Serialized_Length then
         return Length_Error;
      end if;

      -- Extract time:
      Timestamp := Product.Header.Time;

      -- Extract data:
      Item := Data_Product_Serializer.From_Byte_Array (Product.Buffer (Product.Buffer'First .. Product.Buffer'First + Data_Product_Serializer.Serialized_Length - 1));

      -- Check if data product is stale:
      if Self.{{ dd.name }}_Stale_Limit /= Ada.Real_Time.Time_Span_Zero then
         declare
            Ret : constant Stale_Status := Check_Data_Product_Stale (
               Timestamp => Timestamp,
               Stale_Reference => Stale_Reference,
               Stale_Limit => Self.{{ dd.name }}_Stale_Limit
            );
         begin
            case Ret is
               when Success => null; -- Not stale.
               when Stale => return Stale; -- Is stale.
            end case;
         end;
      end if;

      return Success;
   end Extract_{{ dd.name }};

{% endfor %}
end {{ name }};
