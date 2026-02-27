--------------------------------------------------------------------------------
-- Ccsds_Socket_Interface Component Implementation Spec
--------------------------------------------------------------------------------

-- Standard Includes:
-- Invokee Connector Includes:
with Socket;
with Socket_Address;

-- The Socket Component is an interface component which connects the rest of the assembly to an outside entity (usually the ground system) via a TCP/IP socket. It spawns an internal task to listen to the socket for incoming data. It also provides an asynchronous receive connector which it services on its task, sending any data it receives out of the socket. The data send and receive connectors are of a generic buffer type, Com_Packet, so that data of an arbitrary format can be sent via this component.
--
package Component.Ccsds_Socket_Interface.Implementation is

   type Instance is new Ccsds_Socket_Interface.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization subprogram connects the component to a TCP socket on the given address and port.
   overriding procedure Init (Self : in out Instance; Addr : in String := "127.0.0.1"; Port : in Natural := 2_001);
   -- Public function which closes the TCP socket.
   not overriding procedure Final (Self : in out Instance);

private
   -- The component class instance record:
   type Instance is new Ccsds_Socket_Interface.Base_Instance with record
      Sock : Socket.Instance;
      Addr : String (1 .. 512);
      Port : Natural;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- On this connector the Socket Interface Component receives data and sends it out of the socket.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- This procedure is called when a Ccsds_Space_Packet_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   ---------------------------------------
   -- Private helper functions:
   ---------------------------------------
   -- Helper function to convert GNAT socket address to Adamant Socket_Address
   function Convert_Socket_Address (Self : in Instance) return Socket_Address.T;

   -------------------------------------------------------
   -- Definition of subtasks functions for task execution:
   -------------------------------------------------------
   -- This internal task is used to listen on the socket for incoming packets.
   -- IMPORTANT: This component needs an inner task to run the
   -- following function as if it were the "cycle" method of
   -- a normal component. The "cycle" method of this component
   -- acts in the normal way, blocking on the component
   -- queue, waiting for messages to send through the socket.
   -- The special cycle method below should be run on another
   -- task to block on the socket, waiting for incoming messages
   -- which it will then send out to the rest of the assembly.
   overriding procedure Listener (Self : in out Instance);

end Component.Ccsds_Socket_Interface.Implementation;
