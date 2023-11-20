--------------------------------------------------------------------------------
-- Background_Component Component Tester Body
--------------------------------------------------------------------------------

package body Component.Background_Component.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Init (Depth => 10);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 10);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Packet_T_Send (Self'Unchecked_Access, Self.Packet_T_Recv_Sync_Access);
      Self.Attach_Packed_U16_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Packed_U16_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to fetch the current system time.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- This connector is used to send out a telemetry packet.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
   end Packet_T_Recv_Sync;

   -----------------------------------------------
   -- Special primitives for running an active component
   -- with no queue:
   -----------------------------------------------
   -- Force the component to cycle from the tester
   not overriding procedure Cycle_Component (Self : in out Instance) is
   begin
      -- Tell the component to cycle.
      Self.Component_Instance.Cycle;
   end Cycle_Component;

end Component.Background_Component.Implementation.Tester;
