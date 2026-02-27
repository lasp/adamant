--------------------------------------------------------------------------------
-- Queued_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Packed_Byte;
with Packed_U16;
with Packet_Types; use Packet_Types;

-- This is the queued component.
package Component.Queued_Component.Implementation is

   -- The component class instance record:
   type Instance is new Queued_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Queued_Component.Base_Instance with record
      -- The length of the packet data to send out:
      Data_Length : Packet_Buffer_Length_Type := 0;
      -- The content of the packet data to send out:
      Data : Basic_Types.Byte_Array (0 .. 1) := [0, 0];
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

end Component.Queued_Component.Implementation;
