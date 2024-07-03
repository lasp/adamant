--------------------------------------------------------------------------------
-- Example_Component Component Implementation Body
--------------------------------------------------------------------------------

with Interfaces;

package body Component.Example_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Interfaces;

      -- Ignore tick, since we don't actually use it for anything:
      Ignore : Tick.T renames Arg;

      -- Create a packet:
      Pkt : Packet.T := (
         Header => (
            Time => Self.Sys_Time_T_Get,
            Id => 0,
            Sequence_Count => 0,
            Buffer_Length => 1      -- We are using 1 byte of data.
         ),
         Buffer => [others => 0]
      );
   begin
      -- Set the packet data:
      Pkt.Buffer (Pkt.Buffer'First) := Self.Counter;

      -- Send the packet to every connected connector:
      for Idx in Self.Connector_Packet_T_Send.all'Range loop
         Self.Packet_T_Send_If_Connected (Idx, Pkt);
      end loop;

      -- Increment counter:
      Self.Counter := Self.Counter + 1;
   end Tick_T_Recv_Sync;

end Component.Example_Component.Implementation;
