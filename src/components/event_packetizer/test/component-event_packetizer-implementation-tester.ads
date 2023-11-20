--------------------------------------------------------------------------------
-- Event_Packetizer Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Event_Packetizer_Reciprocal;
with Sys_Time;
with Printable_History;
with Packet.Representation;
with Sys_Time.Representation;
with Data_Product.Representation;
with Command_Response.Representation;
with Data_Product;
with Packed_U32.Representation;
with Packed_Natural.Representation;

-- The Event Packetizer component receives events synchronously and places them into a packet. This component receives a periodic tick. A packet is sent out upon receiving a tick if 1) the component has a full packet to send or 2) a partial packet timeout has occurred and the component has a packet with at least one event in it.
package Component.Event_Packetizer.Implementation.Tester is

   -- Invoker connector history packages:
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);

   -- Data product history packages:
   package Events_Dropped_Count_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);
   package Bytes_Available_History_Package is new Printable_History (Packed_Natural.T, Packed_Natural.Representation.Image);

   -- Packet history packages:
   package Events_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Event_Packetizer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Event_Packetizer.Implementation.Instance;
      -- Connector histories:
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      -- Data product histories:
      Events_Dropped_Count_History : Events_Dropped_Count_History_Package.Instance;
      Bytes_Available_History : Bytes_Available_History_Package.Instance;
      -- Packet histories:
      Events_Packet_History : Events_Packet_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Send a packet of events.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Event Packetizer component.
   -- The number of events dropped by the component.
   overriding procedure Events_Dropped_Count (Self : in out Instance; Arg : Packed_U32.T);
   -- The current number of bytes available for event storage within the component.
   overriding procedure Bytes_Available (Self : in out Instance; Arg : Packed_Natural.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the event packetizer
   -- This packet contains events as subpackets.
   overriding procedure Events_Packet (Self : in out Instance; Arg : Packet.T);

end Component.Event_Packetizer.Implementation.Tester;
