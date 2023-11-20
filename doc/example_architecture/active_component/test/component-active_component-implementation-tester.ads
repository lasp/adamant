--------------------------------------------------------------------------------
-- Active_Component Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Active_Component_Reciprocal;
with Sys_Time;
with Printable_History;
with Sys_Time.Representation;
with Packet.Representation;

-- This is the active component.
package Component.Active_Component.Implementation.Tester is

   -- Invoker connector history packages:
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Active_Component_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Active_Component.Implementation.Instance;
      -- Connector histories:
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Packed_Byte_T_Send_Dropped : Boolean := False;
      Packed_Byte_T_Send_Dropped_Count : Natural := 0;
      Expect_Packed_U16_T_Send_Dropped : Boolean := False;
      Packed_U16_T_Send_Dropped_Count : Natural := 0;
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
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;
   -- This connector is used to send out a telemetry packet.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packed_Byte_T_Send message is dropped due to a full queue.
   overriding procedure Packed_Byte_T_Send_Dropped (Self : in out Instance; Arg : in Packed_Byte.T);

   -- This procedure is called when a Packed_U16_T_Send message is dropped due to a full queue.
   overriding procedure Packed_U16_T_Send_Dropped (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Active_Component.Implementation.Tester;
