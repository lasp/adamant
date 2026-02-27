--------------------------------------------------------------------------------
-- Discriminated_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;

-- This is an example component with a discriminant provided.
package Component.Discriminated_Component.Implementation is

   -- The component class instance record:
   -- This is the component's discriminant. It initializes parameters during instantiation.
   --
   -- Discriminant Parameters:
   -- Packets_Per_Tick : Natural - The number of packets to send every time the Tick_T_Recv_Sync connector is invoked.
   -- Enabled_At_Startup : Boolean - If True, packets will be produced for every call to Tick_T_Recv_Sync. If False, no packets will be produced.
   --
   type Instance (Packets_Per_Tick : Natural; Enabled_At_Startup : Boolean) is new Discriminated_Component.Base_Instance with private;

private

   -- The component class instance record:
   -- This is the component's discriminant. It initializes parameters during instantiation.
   --
   -- Discriminant Parameters:
   -- Packets_Per_Tick : Natural - The number of packets to send every time the Tick_T_Recv_Sync connector is invoked.
   -- Enabled_At_Startup : Boolean - If True, packets will be produced for every call to Tick_T_Recv_Sync. If False, no packets will be produced.
   --
   type Instance (Packets_Per_Tick : Natural; Enabled_At_Startup : Boolean) is new Discriminated_Component.Base_Instance with record
      null; -- TODO
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance) is null;

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

end Component.Discriminated_Component.Implementation;
