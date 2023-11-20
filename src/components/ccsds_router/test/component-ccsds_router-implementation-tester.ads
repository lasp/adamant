--------------------------------------------------------------------------------
-- Ccsds_Router Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Ccsds_Router_Reciprocal;
with Sys_Time;
with Printable_History;
with Ccsds_Space_Packet.Representation;
with Event.Representation;
with Packet.Representation;
with Sys_Time.Representation;
with Event;
with Ccsds_Primary_Header.Representation;
with Unexpected_Sequence_Count.Representation;
with Packet;
with History;

-- This component routes CCSDS packets to output connectors based on a static table matching APID to the output connector index. Table lookup is done by binary searching against APID. The look up returns a list of indexes which to route that packet. This component can receive packets either synchronously, or asynchronously and can be made either active or passive depending on the desired use case. If a packet is received with an APID not found in the routing table then it is forwarded out a seperate CCSDS connector, if it is connected. In addition to routing, this component can be configured to check the sequence counts on incoming packets, report discontiguous sequence counts, and drop duplicates.
--
-- Note, a race condition exists if packets of the same APID come in simultaneously on both the sync and async CCSDS connectors, and sequence counts are being checked, where the sequence count checking might get corrupted. This use case is not foreseen as actually happening, so complicating the component with protected objects seems unnecessary.
--
-- Note that an autocoder exists to ease the writing of the CCSDS Router input router table. See documentation for this autocoder in the local gen/doc subdirectory.
package Component.Ccsds_Router.Implementation.Tester is

   -- Invoker connector history packages:
   package Ccsds_Space_Packet_T_Recv_Sync_History_Package is new Printable_History (Ccsds_Space_Packet.T, Ccsds_Space_Packet.Representation.Image);
   package Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History_Package is new Printable_History (Ccsds_Space_Packet.T, Ccsds_Space_Packet.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Unrecognized_Apid_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Dropped_Packet_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);
   package Unexpected_Sequence_Count_Received_History_Package is new Printable_History (Unexpected_Sequence_Count.T, Unexpected_Sequence_Count.Representation.Image);
   package Dropped_Duplicate_Packet_History_Package is new Printable_History (Ccsds_Primary_Header.T, Ccsds_Primary_Header.Representation.Image);

   -- Packet history packages:
   package Error_Packet_History_Package is new History (Ccsds_Space_Packet.T);

   -- Component class instance:
   type Instance is new Component.Ccsds_Router_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Router.Implementation.Instance;
      -- Connector histories:
      Ccsds_Space_Packet_T_Recv_Sync_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History : Unrecognized_Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Adding extra CCSDS histories to store ccsds packets from each output
      -- connector separately:
      Ccsds_Space_Packet_T_Recv_Sync_1_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Ccsds_Space_Packet_T_Recv_Sync_2_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Ccsds_Space_Packet_T_Recv_Sync_3_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Ccsds_Space_Packet_T_Recv_Sync_4_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Ccsds_Space_Packet_T_Recv_Sync_5_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      Ccsds_Space_Packet_T_Recv_Sync_6_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
      -- Event histories:
      Unrecognized_Apid_History : Unrecognized_Apid_History_Package.Instance;
      Dropped_Packet_History : Dropped_Packet_History_Package.Instance;
      Unexpected_Sequence_Count_Received_History : Unexpected_Sequence_Count_Received_History_Package.Instance;
      Dropped_Duplicate_Packet_History : Dropped_Duplicate_Packet_History_Package.Instance;
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
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural; Ccsds_Space_Packet_T_Send_Count : in Connector_Count_Type);
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
   -- Ccsds packets not found in the routing table are forwarded out this connector if it is connected.
   overriding procedure Unrecognized_Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);
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
   -- A packet was received with an APID that was not found in the routing table. The packet was dropped.
   overriding procedure Unrecognized_Apid (Self : in out Instance; Arg : Ccsds_Primary_Header.T);
   -- The component's queue overflowed and a packet with the following header was dropped.
   overriding procedure Dropped_Packet (Self : in out Instance; Arg : Ccsds_Primary_Header.T);
   -- A packet with an unexpected sequence count was received.
   overriding procedure Unexpected_Sequence_Count_Received (Self : in out Instance; Arg : Unexpected_Sequence_Count.T);
   -- The component's received two or more packets in a row with identical sequence counts. The duplicate packet was dropped.
   overriding procedure Dropped_Duplicate_Packet (Self : in out Instance; Arg : Ccsds_Primary_Header.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the CCSDS Router component.
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

   -----------------------------------------------
   -- Adding extra CCSDS recv connectors so that
   -- we can differentiate where the CCSDS packets
   -- are getting sent.
   -----------------------------------------------
   function Ccsds_Space_Packet_T_Recv_Sync_1 (Class_Self : in out Component.Core_Instance'Class; Arg : in Ccsds_Space_Packet.T; Ignore_Index : in Connector_Index_Type; Ignore : in Connector_Types.Full_Queue_Action := Connector_Types.Drop) return Connector_Types.Connector_Status;
   function Ccsds_Space_Packet_T_Recv_Sync_2 (Class_Self : in out Component.Core_Instance'Class; Arg : in Ccsds_Space_Packet.T; Ignore_Index : in Connector_Index_Type; Ignore : in Connector_Types.Full_Queue_Action := Connector_Types.Drop) return Connector_Types.Connector_Status;
   function Ccsds_Space_Packet_T_Recv_Sync_3 (Class_Self : in out Component.Core_Instance'Class; Arg : in Ccsds_Space_Packet.T; Ignore_Index : in Connector_Index_Type; Ignore : in Connector_Types.Full_Queue_Action := Connector_Types.Drop) return Connector_Types.Connector_Status;
   function Ccsds_Space_Packet_T_Recv_Sync_4 (Class_Self : in out Component.Core_Instance'Class; Arg : in Ccsds_Space_Packet.T; Ignore_Index : in Connector_Index_Type; Ignore : in Connector_Types.Full_Queue_Action := Connector_Types.Drop) return Connector_Types.Connector_Status;
   function Ccsds_Space_Packet_T_Recv_Sync_5 (Class_Self : in out Component.Core_Instance'Class; Arg : in Ccsds_Space_Packet.T; Ignore_Index : in Connector_Index_Type; Ignore : in Connector_Types.Full_Queue_Action := Connector_Types.Drop) return Connector_Types.Connector_Status;
   function Ccsds_Space_Packet_T_Recv_Sync_6 (Class_Self : in out Component.Core_Instance'Class; Arg : in Ccsds_Space_Packet.T; Ignore_Index : in Connector_Index_Type; Ignore : in Connector_Types.Full_Queue_Action := Connector_Types.Drop) return Connector_Types.Connector_Status;

end Component.Ccsds_Router.Implementation.Tester;
