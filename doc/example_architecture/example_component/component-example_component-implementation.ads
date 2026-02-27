--------------------------------------------------------------------------------
-- Example_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Basic_Types; use Basic_Types;

-- This is the example component.
package Component.Example_Component.Implementation is

   -- The component class instance record:
   type Instance is new Example_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Example_Component.Base_Instance with record
      -- Define the counter byte:
      Counter : Byte := 0;
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
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Index : in Packet_T_Send_Index; Arg : in Packet.T) is null;

end Component.Example_Component.Implementation;
