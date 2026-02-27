--------------------------------------------------------------------------------
-- Initialized_Component Component Implementation Body
--------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package body Component.Initialized_Component.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This is the component's init procedure. It initializes parameters during initialization.
   --
   -- Init Parameters:
   -- Packets_Per_Tick : Natural - The number of packets to send every time the Tick_T_Recv_Sync connector is invoked.
   -- Enabled_At_Startup : Boolean - If True, packets will be produced for every call to Tick_T_Recv_Sync. If False, no packets will be produced.
   --
   overriding procedure Init (Self : in out Instance; Packets_Per_Tick : in Natural; Enabled_At_Startup : in Boolean := True) is
   begin
      -- Make sure this value is not too large!
      pragma Assert (Packets_Per_Tick < 10);

      -- Initialized the component instance variables:
      Self.Packets_Per_Tick := Packets_Per_Tick;
      Self.Enabled_At_Startup := Enabled_At_Startup;

      -- Other computations can be performed here too!
      -- Let's set the packet data to 5 + the Packets_Per_Tick
      Self.Pkt.Header.Buffer_Length := 7;
      Self.Pkt.Buffer := [others => 5 + Unsigned_8 (Self.Packets_Per_Tick)];
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      Ignore : Tick.T renames Arg;
   begin
      -- Send out the configured number of packets every time
      -- a tick is received.
      for Idx in 1 .. Self.Packets_Per_Tick loop
         Self.Packet_T_Send (Self.Pkt);
      end loop;
   end Tick_T_Recv_Sync;

end Component.Initialized_Component.Implementation;
