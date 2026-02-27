--------------------------------------------------------------------------------
-- Ccsds_Subpacket_Extractor Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Ccsds_Space_Packet;

-- This component extracts CCSDS formatted subpackets from a larger CCSDS formatted packet. This component can receive packets either synchronously, or asynchronously and can be made either active or passive depending on the desired use case.
package Component.Ccsds_Subpacket_Extractor.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Subpacket_Extractor.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component extracts CCSDS subpackets from the data section of a larger CCSDS packet. The init function allows the component to ignore the first, n, or last, m, number of bytes during extraction. This might be useful to ignore a leading secondary header or a trailing checksum.
   --
   -- Init Parameters:
   -- Start_Offset : Natural - The number of bytes past the primary CCSDS header to start extracting subpackets from.
   -- Stop_Offset : Natural - The number of bytes at the end of CCSDS packet (calculated using the primary header length field) to not extract subpackets from. This value should be used to ignore X number of bytes at the end of a packet.
   -- Max_Subpackets_To_Extract : Integer - The maximum number of subpackets to attempt to extract from an incoming packet. A negative number indicates that there is no upper limit to the amount of subpackets that can be extracted. A value of zero disables any subpacketization, which might be useful to disable this component during testing.
   --
   overriding procedure Init (Self : in out Instance; Start_Offset : in Natural := 0; Stop_Offset : in Natural := 0; Max_Subpackets_To_Extract : in Integer := -1);

private

   -- The component class instance record:
   type Instance is new Ccsds_Subpacket_Extractor.Base_Instance with record
      Start_Offset : Natural := 0;
      Stop_Offset : Natural := 0;
      Max_Subpackets_To_Extract : Integer := -1;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The synchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- The asynchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- This procedure is called when a Ccsds_Space_Packet_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

end Component.Ccsds_Subpacket_Extractor.Implementation;
