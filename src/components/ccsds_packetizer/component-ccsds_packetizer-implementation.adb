--------------------------------------------------------------------------------
-- Ccsds_Packetizer Component Implementation Body
--------------------------------------------------------------------------------

with Ccsds_Primary_Header;
with Crc_16;
with Sys_Time;
with Basic_Types;
with Ccsds_Space_Packet;
with Interfaces;
with Packet_Types;
with Ccsds_Enums;

package body Component.Ccsds_Packetizer.Implementation is

   -- Adding some compilation error checks. This component makes the following
   -- assumptions about the sizes of the datatypes it is converting to/from:
   --
   -- The CCSDS space packet size must be bigger than the adamant packet size
   -- plus the size of a timestamp and checksum. Room for a configured number
   -- of trailing bytes is checked at initialization instead.
   pragma Compile_Time_Error
      ((Ccsds_Space_Packet.Ccsds_Data_Type'Length) < (Packet_Types.Packet_Buffer_Type'Length + Sys_Time.Size_In_Bytes + Crc_16.Crc_16_Type'Length), "Size mismatch detected. The size of a CCSDS packet is not large enough to hold an entire Adamant packet.");

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Initialization parameters for the CCSDS Packetizer. The defaults produce the
   -- standard packet, the Adamant packet data followed by a 16-bit CRC.
   --
   -- Init Parameters:
   -- Crc_Enabled : Boolean - If True, the default, a 16-bit CRC-CCITT computed over
   -- the CCSDS primary header and data is placed as the last two bytes of each
   -- packet's data section, inside the packet length. If False no CRC is computed
   -- and the packet ends with the Adamant packet data, or with the trailing bytes
   -- when Num_Trailing_Bytes_To_Add is non-zero.
   -- Num_Trailing_Bytes_To_Add : Natural - The number of zero bytes appended after
   -- the Adamant packet data, inside the packet length, so that a transmitter which
   -- computes the CRC in hardware and overwrites the last bytes of each packet it
   -- sends has a slot to fill. Must be zero when Crc_Enabled is True, since the CRC
   -- occupies the end of the packet.
   --
   overriding procedure Init (Self : in out Instance; Crc_Enabled : in Boolean := True; Num_Trailing_Bytes_To_Add : in Natural := 0) is
   begin
      -- The CRC occupies the end of the packet, so trailing bytes can only be
      -- added in its place.
      pragma Assert (not Crc_Enabled or else Num_Trailing_Bytes_To_Add = 0);
      -- The largest Adamant packet, its timestamp and the trailing bytes must
      -- fit within the CCSDS packet data section.
      pragma Assert (Packet_Types.Packet_Buffer_Type'Length + Sys_Time.Size_In_Bytes + Num_Trailing_Bytes_To_Add <= Ccsds_Space_Packet.Ccsds_Data_Type'Length);
      Self.Crc_Enabled := Crc_Enabled;
      Self.Num_Trailing_Bytes_To_Add := Num_Trailing_Bytes_To_Add;
   end Init;

   ---------------------------------------
   -- Private functions:
   ---------------------------------------

   function To_Ccsds (Self : in Instance; P : in Packet.T) return Ccsds_Space_Packet.T is
      -- Use clauses:
      use Interfaces;
      use Crc_16;
      use Ccsds_Primary_Header;
      use Ccsds_Enums;

      -- The bytes that follow the Adamant packet data inside the packet length:
      -- the CRC, or the zero placeholder for a CRC computed downstream.
      Num_Trailer_Bytes : constant Natural := (if Self.Crc_Enabled then Crc_16_Type'Length else Self.Num_Trailing_Bytes_To_Add);

      -- CCSDS Packet. The data section is zero beyond the copied bytes, which
      -- leaves any trailing bytes zero:
      To_Return : Ccsds_Space_Packet.T := (
       Header => (
          Version => 0,
          Packet_Type => Ccsds_Packet_Type.Telemetry,
          Secondary_Header => Ccsds_Secondary_Header_Indicator.Secondary_Header_Present,
          Apid => Ccsds_Apid_Type (P.Header.Id),
          Sequence_Flag => Ccsds_Sequence_Flag.Unsegmented,
          Sequence_Count => Ccsds_Sequence_Count_Type (P.Header.Sequence_Count),
          Packet_Length => Unsigned_16 (Sys_Time.Serialization.Serialized_Length + P.Header.Buffer_Length + Num_Trailer_Bytes - 1)
       ),
       Data => [others => 0]
    );
   begin
      -- Copy the timestamp:
      To_Return.Data (To_Return.Data'First .. To_Return.Data'First + Sys_Time.Serialization.Serialized_Length - 1) := Sys_Time.Serialization.To_Byte_Array (P.Header.Time);

      -- Copy the data:
      To_Return.Data (To_Return.Data'First + Sys_Time.Serialization.Serialized_Length .. To_Return.Data'First + Sys_Time.Serialization.Serialized_Length + P.Header.Buffer_Length - 1) :=
         P.Buffer (P.Buffer'First .. P.Buffer'First + P.Header.Buffer_Length - 1);

      -- Compute the CRC over the primary header and the data before it, and
      -- place it as the last two bytes within the packet length:
      if Self.Crc_Enabled then
         declare
            -- Overlay a byte array with the Ccsds_Packet:
            pragma Warnings (Off, "overlay changes scalar storage order");
            Overlay : Basic_Types.Byte_Array (0 .. Natural (To_Return.Header.Packet_Length) + Ccsds_Primary_Header.Serialization.Serialized_Length - Crc_16_Type'Length) with
               Import,
               Convention => Ada,
               Address => To_Return'Address;
            pragma Warnings (On, "overlay changes scalar storage order");
            Crc : constant Crc_16_Type := Compute_Crc_16 (Overlay);
         begin
            To_Return.Data (
               To_Return.Data'First + Natural (To_Return.Header.Packet_Length) - Crc_16_Type'Length + 1 ..
               To_Return.Data'First + Natural (To_Return.Header.Packet_Length))
            := Crc;
         end;
      end if;

      return To_Return;
   end To_Ccsds;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The packet receive connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
      -- Do conversion of packet to ccsds packet:
      To_Send : constant Ccsds_Space_Packet.T := To_Ccsds (Self, Arg);
   begin
      -- Send the packet:
      Self.Ccsds_Space_Packet_T_Send (To_Send);
   end Packet_T_Recv_Sync;

end Component.Ccsds_Packetizer.Implementation;
