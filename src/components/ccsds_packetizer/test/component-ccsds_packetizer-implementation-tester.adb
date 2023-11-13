--------------------------------------------------------------------------------
-- Ccsds_Packetizer Component Tester Body
--------------------------------------------------------------------------------

package body Component.Ccsds_Packetizer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Init (Depth => 10);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Ccsds_Space_Packet_T_Send (Self'Unchecked_Access, Self.Ccsds_Space_Packet_T_Recv_Sync_Access);
      Self.Attach_Packet_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Packet_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The ccsds send connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Ccsds_Space_Packet_T_Recv_Sync_History.Push (Arg);
   end Ccsds_Space_Packet_T_Recv_Sync;

end Component.Ccsds_Packetizer.Implementation.Tester;
