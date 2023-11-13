--------------------------------------------------------------------------------
-- Ccsds_Echo Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Ccsds_Space_Packet;

-- This component creates Adamant packets whose data is the CCSDS packets sent to it. The main use of this component is to echo a CCSDS stream from uplink back down as downlink.
package Component.Ccsds_Echo.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Echo.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Ccsds_Echo.Base_Instance with record
      null; -- No internal state
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The CCSDS receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

end Component.Ccsds_Echo.Implementation;
