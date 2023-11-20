--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Body
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------
{% if basic_types %}

-- Standard Includes:
with Serializer;
{% endif %}
{% if typeless_packet or variable_length_types %}
with Byte_Array_Util;
{% endif %}
{% if not typeless_packet and variable_length_types %}
with Basic_Types;
{% endif %}

package body {{ name }} is

   use Packet_Types;

{% if not ids %}
   -----------------------------------------------
   -- Getter/setter subprograms for packet ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Packet_Types.Packet_Id is
   begin
      return Self.Id_Base;
   end Get_Id_Base;

   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Packet_Types.Packet_Id) is
   begin
{% if (packets|length) > 1 %}
      -- ID base set too high for packet ID set. This is checked in the assembly model.
      pragma Assert (Natural (Id_Base) + {{ (packets|length) - 1 }} <= Natural (Packet_Types.Packet_Id'Last));
{% endif %}
      Self.Id_Base := Id_Base;
   end Set_Id_Base;

{% endif %}
   -----------------------------------------------
   -- Getter function for global packet IDs:
   -----------------------------------------------
{% for p in packets %}
   not overriding function Get_{{ p.name }}_Id (Self : in Instance) return Packet_Types.Packet_Id is
{% if ids %}
      Ignore : Instance renames Self;
{% endif %}
   begin
{% if ids %}
      -- Return hard coded packet identifier:
      return Local_Packet_Id_Type'Enum_Rep ({{ p.name }}_Id);
{% else %}
      -- Return calculated packet identifier:
      return Self.Id_Base + Local_Packet_Id_Type'Enum_Rep ({{ p.name }}_Id);
{% endif %}
   end Get_{{ p.name }}_Id;

{% endfor %}
   -----------------------------------------------
   -- Packet creation functions:
   -----------------------------------------------
{% for p in packets %}
{% if p.type %}
{% if p.type_model and p.type_model.variable_length %}
   not overriding function {{ p.name }} (Self : in out Instance; Timestamp : in Sys_Time.T; Item : in {{ p.type }}; Pkt : out Packet.T) return Serialization_Status is
      package Packet_Serializer renames {{ p.type_package }}.Serialization;
      Stat : Serialization_Status;
      Num_Bytes_Serialized : Natural;
   begin
      -- Initialize the packet:
      Pkt := (
         Header => (
            Id => Self.Get_{{ p.name }}_Id,
            Time => Timestamp,
            Sequence_Count => Self.{{ p.name }}_Sequence_Count,
            Buffer_Length => 0
         ),
         Buffer => (others => 0)
      );
      -- Copy over the data:
      Stat := Packet_Serializer.To_Byte_Array (Pkt.Buffer, Item, Num_Bytes_Serialized);
      if Stat /= Success then
         return Stat;
      end if;

      -- Update the packet length and sequence count:
      Pkt.Header.Buffer_Length := Num_Bytes_Serialized;
      Self.{{ p.name }}_Sequence_Count := Self.{{ p.name }}_Sequence_Count + 1;

      return Stat;
   end {{ p.name }};

   not overriding function {{ p.name }}_Truncate (Self : in out Instance; Timestamp : in Sys_Time.T; Item : in {{ p.type }}) return Packet.T is
      P : Packet.T := (
         Header => (
            Id => Self.Get_{{ p.name }}_Id,
            Time => Timestamp,
            Sequence_Count => Self.{{ p.name }}_Sequence_Count,
            Buffer_Length => 0
         ),
         Buffer => (others => 0)
      );
      Stat : Serialization_Status;
      Num_Bytes_Serialized : Natural;
   begin
      -- Get the length of the item:
      -- If getting the length failed then just set the number of bytes to serialize to the maximum
      -- to copy which is the minimum between the size of the packet buffer and the size of the serialized item.
      Stat := {{ p.type_package }}.Serialized_Length (Item, Num_Bytes_Serialized);
      if Stat /= Success then
         Num_Bytes_Serialized := Integer'Min (P.Buffer'Length, {{ p.type_package }}.Size_In_Bytes);
      end if;

      declare
         use Byte_Array_Util;
         -- Overlay type with properly sized byte array:
         subtype Sized_Byte_Array_Index is Natural range 0 .. Num_Bytes_Serialized - 1;
         subtype Sized_Byte_Array is Basic_Types.Byte_Array (Sized_Byte_Array_Index);
         pragma Warnings (Off, "overlay changes scalar storage order");
         Overlay : constant Sized_Byte_Array with Import, Convention => Ada, Address => Item'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
         -- Safely copy item into packet and update buffer length:
         Num_Bytes_Copied : constant Natural := Safe_Left_Copy (P.Buffer, Overlay);
      begin
         P.Header.Buffer_Length := Num_Bytes_Copied;
         Self.{{ p.name }}_Sequence_Count := Self.{{ p.name }}_Sequence_Count + 1;
      end;

      return P;
   end {{ p.name }}_Truncate;
{% else %}
   not overriding function {{ p.name }} (Self : in out Instance; Timestamp : in Sys_Time.T; Item : in {{ p.type }}) return Packet.T is
{% if p.type_model %}
      package Packet_Serializer renames {{ p.type_package }}.Serialization;
{% else %}
      package Packet_Serializer is new Serializer ({{ p.type }});
{% endif %}
      P : Packet.T := (
         Header => (
            Id => Self.Get_{{ p.name }}_Id,
            Time => Timestamp,
            Sequence_Count => Self.{{ p.name }}_Sequence_Count,
            Buffer_Length => Packet_Serializer.Serialized_Length
         ),
         Buffer => (others => 0)
      );
   begin
      P.Buffer (P.Buffer'First .. P.Buffer'First + Packet_Serializer.Serialized_Length - 1) := Packet_Serializer.To_Byte_Array (Item);
      Self.{{ p.name }}_Sequence_Count := Self.{{ p.name }}_Sequence_Count + 1;
      return P;
   end {{ p.name }};
{% endif %}
{% else %}
   not overriding function {{ p.name }} (Self : in out Instance; Timestamp : in Sys_Time.T; Buf : in Basic_Types.Byte_Array; Pkt : out Packet.T) return Serialization_Status is
   begin
      -- Make sure the passed in buffer will fit into the packet:
      if Buf'Length > Pkt.Buffer'Length then
         return Failure;
      else
         -- Initialize the packet:
         Pkt := Self.{{ p.name }}_Empty (Timestamp);
         -- Set the length of data to copy:
         Pkt.Header.Buffer_Length := Buf'Length;
         -- Copy over the data:
         Pkt.Buffer (Packet_Buffer_Type'First .. Packet_Buffer_Type'First + Buf'Length - 1) := Buf;
      end if;

      return Success;
   end {{ p.name }};

   not overriding function {{ p.name }}_Truncate (Self : in out Instance; Timestamp : in Sys_Time.T; Buf : in Basic_Types.Byte_Array) return Packet.T is
      use Byte_Array_Util;
      -- Initialize the packet:
      P : Packet.T := Self.{{ p.name }}_Empty (Timestamp);
      -- Copy over the number of bytes that fit in the packet:
      Num_Bytes_Copied : constant Natural := Safe_Left_Copy (P.Buffer, Buf);
   begin
      -- Set the length:
      P.Header.Buffer_Length := Num_Bytes_Copied;
      return P;
   end {{ p.name }}_Truncate;

   not overriding function {{ p.name }}_Empty (Self : in out Instance; Timestamp : in Sys_Time.T) return Packet.T is
      -- Initialize the packet:
      P : constant Packet.T := (
         Header => (
            Id => Self.Get_{{ p.name }}_Id,
            Time => Timestamp,
            Sequence_Count => Self.{{ p.name }}_Sequence_Count,
            Buffer_Length => 0
         ),
         Buffer => (others => 0)
      );
   begin
      -- Increment the sequence count:
      Self.{{ p.name }}_Sequence_Count := Self.{{ p.name }}_Sequence_Count + 1;
      return P;
   end {{ p.name }}_Empty;
{% endif %}

{% endfor %}
end {{ name }};
