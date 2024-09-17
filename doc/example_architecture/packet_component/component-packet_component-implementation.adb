--------------------------------------------------------------------------------
-- Packet_Component Component Implementation Body
--------------------------------------------------------------------------------

with Packed_U16;

package body Component.Packet_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Get the timestamp:
      Timestamp : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Send the counter packet:
      Self.Packet_T_Send (Self.Packets.Counter_Truncate (
         Timestamp,
         Packed_U16.Serialization.To_Byte_Array ((Value => Self.Count))
      ));

      -- Send the last tick data product:
      Self.Packet_T_Send (Self.Packets.Last_Tick_Received (Timestamp, Arg));

      -- Increment the count:
      Self.Count := @ + 1;
   end Tick_T_Recv_Sync;

end Component.Packet_Component.Implementation;
