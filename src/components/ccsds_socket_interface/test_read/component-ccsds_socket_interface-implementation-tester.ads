--------------------------------------------------------------------------------
-- Ccsds_Socket_Interface Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Socket_Interface_Reciprocal;
with Sys_Time;
with History;
with Ccsds_Space_Packet;
with Event;
with Socket_Address;
with Ccsds_Primary_Header;

-- The Socket Interface Component is an interface component which connects the rest of the assembly to an outside entity (usually the ground system) via a TCP/IP socket. It spawns an internal task to listen to the socket for incoming data. It also provides an asynchronous receive connector which it services on its task, sending any data it receives out of the socket. The data send and receive connectors are CCSDS.
--
package Component.Ccsds_Socket_Interface.Implementation.Tester is

   -- Invoker connector history packages:
   package Ccsds_Space_Packet_T_Recv_Sync_History_Package is new History (Ccsds_Space_Packet.T);
   package Event_T_Recv_Sync_History_Package is new History (Event.T);
   package Sys_Time_T_Return_History_Package is new History (Sys_Time.T);

   -- Event history packages:
   package Socket_Connected_History_Package is new History (Socket_Address.T);
   package Socket_Not_Connected_History_Package is new History (Socket_Address.T);
   package Packet_Send_Failed_History_Package is new History (Ccsds_Primary_Header.T);
   package Packet_Recv_Failed_History_Package is new History (Ccsds_Primary_Header.T);

   -- Component class instance:
   type Instance is new Component.Ccsds_Socket_Interface_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Socket_Interface.Implementation.Instance;
      -- Connector histories:
      Ccsds_Space_Packet_T_Recv_Sync_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Socket_Connected_History : Socket_Connected_History_Package.Instance;
      Socket_Not_Connected_History : Socket_Not_Connected_History_Package.Instance;
      Packet_Send_Failed_History : Packet_Send_Failed_History_Package.Instance;
      Packet_Recv_Failed_History : Packet_Recv_Failed_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Ccsds_Space_Packet_T_Send_Dropped : Boolean := False;
      Ccsds_Space_Packet_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- On this connector the Socket Interface Component sends any data it received from the socket.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The socket was successfully connected on the host and port provided.
   overriding procedure Socket_Connected (Self : in out Instance; Arg : Socket_Address.T);
   -- The socket connection failed on the host and port provided
   overriding procedure Socket_Not_Connected (Self : in out Instance; Arg : Socket_Address.T);
   -- Failed to send a packet over the socket because it has an invalid CCSDS header.
   overriding procedure Packet_Send_Failed (Self : in out Instance; Arg : Ccsds_Primary_Header.T);
   -- Failed to receive a packet over the socket because it has an invalid CCSDS header.
   overriding procedure Packet_Recv_Failed (Self : in out Instance; Arg : Ccsds_Primary_Header.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : Positive := 1) return Natural;

end Component.Ccsds_Socket_Interface.Implementation.Tester;
