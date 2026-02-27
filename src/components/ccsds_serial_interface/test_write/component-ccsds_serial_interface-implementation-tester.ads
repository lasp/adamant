--------------------------------------------------------------------------------
-- Ccsds_Serial_Interface Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Serial_Interface_Reciprocal;
with Sys_Time;
with History;
with Ccsds_Space_Packet;
with Event;
with Ccsds_Primary_Header;
with Packed_U32;

-- This component is meant to be a backdoor serial component which uses Ada.Text_IO to send and receive data over a serial port. On Linux, this will send/recv data to/from the terminal, but Ada.Text_IO is attached to a diagnostic uart on most embedded systems. This means that this component can be used as a quick and dirty serial interface without implementing hardware specific uart drivers.
--
package Component.Ccsds_Serial_Interface.Implementation.Tester is

   -- Invoker connector history packages:
   package Ccsds_Space_Packet_T_Recv_Sync_History_Package is new History (Ccsds_Space_Packet.T);
   package Event_T_Recv_Sync_History_Package is new History (Event.T);
   package Sys_Time_T_Return_History_Package is new History (Sys_Time.T);

   -- Event history packages:
   package Packet_Send_Failed_History_Package is new History (Ccsds_Primary_Header.T);
   package Packet_Recv_Failed_History_Package is new History (Ccsds_Primary_Header.T);
   package Have_Not_Seen_Sync_Pattern_History_Package is new History (Packed_U32.T);

   -- Component class instance:
   type Instance is new Component.Ccsds_Serial_Interface_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Serial_Interface.Implementation.Instance;
      -- Connector histories:
      Ccsds_Space_Packet_T_Recv_Sync_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Packet_Send_Failed_History : Packet_Send_Failed_History_Package.Instance;
      Packet_Recv_Failed_History : Packet_Recv_Failed_History_Package.Instance;
      Have_Not_Seen_Sync_Pattern_History : Have_Not_Seen_Sync_Pattern_History_Package.Instance;
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
   -- On this connector the Serial Interface Component sends any data it received from the serial port.
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
   -- Failed to send a packet over the serial port because it has an invalid CCSDS header.
   overriding procedure Packet_Send_Failed (Self : in out Instance; Arg : Ccsds_Primary_Header.T);
   -- Failed to receive a packet over the serial port because it has an invalid CCSDS header.
   overriding procedure Packet_Recv_Failed (Self : in out Instance; Arg : Ccsds_Primary_Header.T);
   -- The component has received N number of bytes without seeing a sync pattern yet.
   overriding procedure Have_Not_Seen_Sync_Pattern (Self : in out Instance; Arg : Packed_U32.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : Positive := 1) return Natural;

end Component.Ccsds_Serial_Interface.Implementation.Tester;
