--------------------------------------------------------------------------------
-- Ccsds_Subpacket_Extractor Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Subpacket_Extractor_Reciprocal;
with Sys_Time;
with History;
with Ccsds_Space_Packet;
with Event;
with Packet;
with Invalid_Packet_Length;
with Packed_Byte;
with Ccsds_Primary_Header;

-- This component extracts CCSDS formatted subpackets from a larger CCSDS formatted packet. This component can receive packets either synchronously, or asynchronously and can be made either active or passive depending on the desired use case.
package Component.Ccsds_Subpacket_Extractor.Implementation.Tester is

   -- Invoker connector history packages:
   package Ccsds_Space_Packet_T_Recv_Sync_History_Package is new History (Ccsds_Space_Packet.T);
   package Event_T_Recv_Sync_History_Package is new History (Event.T);
   package Packet_T_Recv_Sync_History_Package is new History (Packet.T);
   package Sys_Time_T_Return_History_Package is new History (Sys_Time.T);

   -- Event history packages:
   package Invalid_Received_Packet_Length_History_Package is new History (Invalid_Packet_Length.T);
   package Invalid_Extracted_Packet_Length_History_Package is new History (Invalid_Packet_Length.T);
   package Dropped_Trailing_Bytes_History_Package is new History (Packed_Byte.T);
   package Dropped_Packet_History_Package is new History (Ccsds_Primary_Header.T);

   -- Packet history packages:
   package Error_Packet_History_Package is new History (Ccsds_Space_Packet.T);

   -- Component class instance:
   type Instance is new Component.Ccsds_Subpacket_Extractor_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Subpacket_Extractor.Implementation.Instance;
      -- Connector histories:
      Ccsds_Space_Packet_T_Recv_Sync_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Invalid_Received_Packet_Length_History : Invalid_Received_Packet_Length_History_Package.Instance;
      Invalid_Extracted_Packet_Length_History : Invalid_Extracted_Packet_Length_History_Package.Instance;
      Dropped_Trailing_Bytes_History : Dropped_Trailing_Bytes_History_Package.Instance;
      Dropped_Packet_History : Dropped_Packet_History_Package.Instance;
      -- Packet histories:
      Error_Packet_History : Error_Packet_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Ccsds_Space_Packet_T_Send_2_Dropped : Boolean := False;
      Ccsds_Space_Packet_T_Send_2_Dropped_Count : Natural := 0;
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
   -- The ccsds packet send connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Error packets are sent out of this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Ccsds_Space_Packet_T_Send_2 message is dropped due to a full queue.
   overriding procedure Ccsds_Space_Packet_T_Send_2_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A packet was received with a length that is too large or too small.
   overriding procedure Invalid_Received_Packet_Length (Self : in out Instance; Arg : Invalid_Packet_Length.T);
   -- A packet was extracted with a length that is too large.
   overriding procedure Invalid_Extracted_Packet_Length (Self : in out Instance; Arg : Invalid_Packet_Length.T);
   -- Some remaining bytes were found at the end of a packet that are too small to be a CCSDS packet.
   overriding procedure Dropped_Trailing_Bytes (Self : in out Instance; Arg : Packed_Byte.T);
   -- The component's queue overflowed and a packet with the following header was dropped.
   overriding procedure Dropped_Packet (Self : in out Instance; Arg : Ccsds_Primary_Header.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the CCSDS Subpacket Extractor component.
   -- This packet contains a CCSDS packet that was dropped due to error.
   overriding procedure Error_Packet (Self : in out Instance; Arg : Ccsds_Space_Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : Positive := 1) return Natural;

end Component.Ccsds_Subpacket_Extractor.Implementation.Tester;
