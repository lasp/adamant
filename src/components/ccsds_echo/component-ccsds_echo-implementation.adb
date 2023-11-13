--------------------------------------------------------------------------------
-- Ccsds_Echo Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Ccsds_Echo.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The CCSDS receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      Self.Packet_T_Send_If_Connected (Self.Packets.Echo_Packet_Truncate (Self.Sys_Time_T_Get, Arg));
   end Ccsds_Space_Packet_T_Recv_Sync;

end Component.Ccsds_Echo.Implementation;
