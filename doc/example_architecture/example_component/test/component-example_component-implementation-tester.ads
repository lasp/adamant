--------------------------------------------------------------------------------
-- Example_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Example_Component_Reciprocal;
with Sys_Time;
with Printable_History;
with Sys_Time.Representation;
with Packet.Representation;

-- This is the example component.
package Component.Example_Component.Implementation.Tester is

   use Component.Example_Component_Reciprocal;
   -- Invoker connector history packages:
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Example_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Example_Component.Implementation.Instance;
      -- Connector histories:
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Packet_T_Send_Count : in Connector_Count_Type);
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
   -- This connector is used to send out a telemetry packet.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);

end Component.Example_Component.Implementation.Tester;
