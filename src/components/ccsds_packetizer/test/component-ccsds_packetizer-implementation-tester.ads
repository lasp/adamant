--------------------------------------------------------------------------------
-- Ccsds_Packetizer Component Tester Spec
--------------------------------------------------------------------------------

-- Standard Includes:
with Component.Ccsds_Packetizer_Reciprocal;
with History;

-- Invokee Connector Includes:
with Ccsds_Space_Packet;

-- This component converts Adamant packets into CCSDS synchronously and sends them out.
package Component.Ccsds_Packetizer.Implementation.Tester is

   -- Invoker connector history packages:
   package Ccsds_Space_Packet_T_Recv_Sync_History_Package is new History (Ccsds_Space_Packet.T);

   -- Component class instance:
   type Instance is new Component.Ccsds_Packetizer_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Ccsds_Packetizer.Implementation.Instance;
      -- Connector histories:
      Ccsds_Space_Packet_T_Recv_Sync_History : Ccsds_Space_Packet_T_Recv_Sync_History_Package.Instance;
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
   -- The ccsds send connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T);

end Component.Ccsds_Packetizer.Implementation.Tester;
