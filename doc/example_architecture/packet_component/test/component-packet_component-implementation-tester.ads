--------------------------------------------------------------------------------
-- Packet_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Packet_Component_Reciprocal;
with Sys_Time;
with Printable_History;
with Sys_Time.Representation;
with Packet.Representation;
with Tick.Representation;

-- This is the packet component, which sends packets.
package Component.Packet_Component.Implementation.Tester is

   -- Invoker connector history packages:
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Packet history packages:
   package Counter_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Last_Tick_Received_History_Package is new Printable_History (Tick.T, Tick.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Packet_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Packet_Component.Implementation.Instance;
      -- Connector histories:
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      -- Packet histories:
      Counter_History : Counter_History_Package.Instance;
      Last_Tick_Received_History : Last_Tick_Received_History_Package.Instance;
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
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to send out packets.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    A set of packets for the Packet Component.
   -- A packet containing an incrementing 16-bit counter.
   overriding procedure Counter (Self : in out Instance; Arg : in Packet.T);
   -- A last tick that was received by the component.
   overriding procedure Last_Tick_Received (Self : in out Instance; Arg : in Tick.T);

end Component.Packet_Component.Implementation.Tester;
