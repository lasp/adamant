--------------------------------------------------------------------------------
-- Ccsds_Packetizer Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Packet;

-- This component converts Adamant packets into CCSDS synchronously and sends them out.
package Component.Ccsds_Packetizer.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Packetizer.Base_Instance with private;

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
   overriding procedure Init (Self : in out Instance; Crc_Enabled : in Boolean := True; Num_Trailing_Bytes_To_Add : in Natural := 0);

private

   -- The component class instance record:
   type Instance is new Ccsds_Packetizer.Base_Instance with record
      -- Configuration, defaulted to the standard packet with a CRC:
      Crc_Enabled : Boolean := True;
      Num_Trailing_Bytes_To_Add : Natural := 0;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The packet receive connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;

end Component.Ccsds_Packetizer.Implementation;
