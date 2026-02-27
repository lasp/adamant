--------------------------------------------------------------------------------
-- {{ formatType(model_name) }} {{ formatType(model_type) }} Spec
--
-- Generated from {{ filename }} on {{ time }}.
--------------------------------------------------------------------------------

-- Standard Includes:
with Packet_Types;
with Packet;
with Sys_Time;
{% if includes %}

-- Packet Type Includes
{% for include in includes %}
{% if include not in ["Sys_Time", "Packet_Types", "Packet"] %}
with {{ include }};
{% endif %}
{% endfor %}
{% endif %}
{% if typeless_packet %}
with Basic_Types;
{% endif %}
{% if typeless_packet or variable_length_types %}
with Serializer_Types; use Serializer_Types;
{% endif %}
{% if description %}

{{ printMultiLine(description, '-- ') }}
{% endif %}
package {{ name }} is

   -- Object instance type:
   type Instance is tagged limited private;

   -----------------------------------------------
   -- Local Packet Identifiers:
   -----------------------------------------------
   Num_Packets : constant Natural := {{ packets|length }};
   type Local_Packet_Id_Type is (
{% for p in packets %}
      {{ p.name }}_Id{{ "," if not loop.last }}
{% endfor %}
   );
   for Local_Packet_Id_Type use (
{% for p in packets %}
{% if ids %}
      {{ p.name }}_Id => {{ p.id }}{{ "," if not loop.last }}
{% else %}
      {{ p.name }}_Id => {{ loop.index0 }}{{ "," if not loop.last }}
{% endif %}
{% endfor %}
   );

{% if not ids %}
   -----------------------------------------------
   -- Setter procedure for packet ID base:
   -----------------------------------------------
   not overriding function Get_Id_Base (Self : in Instance) return Packet_Types.Packet_Id
      with Inline => True;
   not overriding procedure Set_Id_Base (Self : in out Instance; Id_Base : in Packet_Types.Packet_Id)
      with Inline => True;

{% endif %}
   -----------------------------------------------
   -- Getter function for global packet IDs:
   -----------------------------------------------
{% for p in packets %}
   not overriding function Get_{{ p.name }}_Id (Self : in Instance) return Packet_Types.Packet_Id
      with Inline => True;
{% endfor %}

   -----------------------------------------------
   -- Packet creation functions:
   -----------------------------------------------
{% for p in packets %}
{% if p.description %}
{{ printMultiLine(p.description, '   -- ') }}
{% endif %}
{% if p.type %}
{% if p.type_model and p.type_model.variable_length %}
   not overriding function {{ p.name }} (Self : in out Instance; Timestamp : in Sys_Time.T; Item : in {{ p.type }}; Pkt : out Packet.T) return Serialization_Status;
   -- Special function, which will fill the Packet.T type as much as possible, and then drop any remaining bytes found in the
   -- input item. Sometimes this is desirable over returning a Serialization_Error, like what would occur with the above function.
   not overriding function {{ p.name }}_Truncate (Self : in out Instance; Timestamp : in Sys_Time.T; Item : in {{ p.type }}) return Packet.T;
{% else %}
   not overriding function {{ p.name }} (Self : in out Instance; Timestamp : in Sys_Time.T; Item : in {{ p.type }}) return Packet.T;
{% endif %}
{% else %}
   not overriding function {{ p.name }} (Self : in out Instance; Timestamp : in Sys_Time.T; Buf : in Basic_Types.Byte_Array; Pkt : out Packet.T) return Serialization_Status;
   -- Special function, which will fill the Packet.T type as much as possible, and then drop any remaining bytes found in the
   -- input item. Sometimes this is desirable over returning a Serialization_Error, like what would occur with the above function.
   not overriding function {{ p.name }}_Truncate (Self : in out Instance; Timestamp : in Sys_Time.T; Buf : in Basic_Types.Byte_Array) return Packet.T;
   -- This function returns a packet with the timestamp, id, and sequence number filled in, but with a data length of zero and no
   -- zeroed buffer data. Sometimes this is more convenient to use than the functions above.
   not overriding function {{ p.name }}_Empty (Self : in out Instance; Timestamp : in Sys_Time.T) return Packet.T;
{% endif %}

{% endfor %}
{% if types %}
   -- Compile time checks to make sure types do not serialize longer than the packet buffer size:
{% for p in packets %}
   pragma Warnings (Off, "condition can only be True if invalid values present");
{% if p.type %}
{% if p.type_model %}
{% if not p.type_model.variable_length %}
   pragma Compile_Time_Error (
      {{ p.type_package }}.Size_In_Bytes > Packet_Types.Packet_Buffer_Type'Length,
      "Packet '{{ p.name }}' has argument of type '{{ p.type }}' which has a maximum serialized length larger than the buffer size of Packet.T."
   );
{% endif %}
{% else %}
   pragma Compile_Time_Error (
      (({{ p.type   }}'Object_Size - 1) / 8 + 1) > Packet_Types.Packet_Buffer_Type'Length,
      "Packet '{{ p.name }}' has argument of type '{{ p.type }}' which has a maximum serialized length larger than the buffer size of Packet.T."
   );
{% endif %}
{% endif %}
{% endfor %}
   pragma Warnings (On, "condition can only be True if invalid values present");
{% endif %}
private

   type Instance is tagged limited record
{% if not ids %}
      Id_Base : Packet_Types.Packet_Id := 0;
{% endif %}
{% for p in packets %}
      {{ p.name }}_Sequence_Count : Packet_Types.Sequence_Count_Mod_Type := 0;
{% endfor %}
   end record;

end {{ name }};
