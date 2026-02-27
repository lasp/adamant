--------------------------------------------------------------------------------
-- Initialized_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;

-- This is the example component, with an Init procedure.
package Component.Initialized_Component.Implementation is

   -- The component class instance record:
   type Instance is new Initialized_Component.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This is the component's init procedure. It initializes parameters during initialization.
   --
   -- Init Parameters:
   -- Packets_Per_Tick : Natural - The number of packets to send every time the Tick_T_Recv_Sync connector is invoked.
   -- Enabled_At_Startup : Boolean - If True, packets will be produced for every call to Tick_T_Recv_Sync. If False, no packets will be produced.
   --
   overriding procedure Init (Self : in out Instance; Packets_Per_Tick : in Natural; Enabled_At_Startup : in Boolean := True);

private

   -- The component class instance record:
   type Instance is new Initialized_Component.Base_Instance with record
      Packets_Per_Tick : Natural := 0;
      Enabled_At_Startup : Boolean := False;
      Pkt : Packet.T;
   end record;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

end Component.Initialized_Component.Implementation;
