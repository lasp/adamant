--------------------------------------------------------------------------------
-- Ccsds_Packetizer Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Packet;

-- This component converts Adamant packets into CCSDS synchronously and sends them out.
package Component.Ccsds_Packetizer.Implementation is

   -- The component class instance record:
   type Instance is new Ccsds_Packetizer.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Ccsds_Packetizer.Base_Instance with record
      null; -- No internal state
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
