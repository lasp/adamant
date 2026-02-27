--------------------------------------------------------------------------------
-- Active_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Packed_Byte;
with Packed_U16;

-- This is the active component.
package Component.Active_Component.Implementation is

   -- The component class instance record:
   type Instance is new Active_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Active_Component.Base_Instance with record
      null;
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
   -- This connector receives a single byte asynchronously that is used to populate the outgoing packet.
   overriding procedure Packed_Byte_T_Recv_Async (Self : in out Instance; Arg : in Packed_Byte.T);
   -- This procedure is called when a Packed_Byte_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Packed_Byte_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Packed_Byte.T);
   -- This connector receives a 16-bit number asynchronously that is used to populate the outgoing packet.
   overriding procedure Packed_U16_T_Recv_Async (Self : in out Instance; Arg : in Packed_U16.T);
   -- This procedure is called when a Packed_U16_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Packed_U16_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Packed_U16.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;

end Component.Active_Component.Implementation;
