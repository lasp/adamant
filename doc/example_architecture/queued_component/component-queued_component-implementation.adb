--------------------------------------------------------------------------------
-- Queued_Component Component Implementation Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Component.Queued_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Ignore tick, since we don't actually use it for anything:
      Ignore : Tick.T renames Arg;
      -- Ignore the return from dispatch_all.
      Ignore2 : Natural;
   begin
      -- First, we need to service any asynchronous messages that we have received.
      Ignore2 := Self.Dispatch_All;
      -- ^ if anything is on the queue at this point, then the Packed_Byte_T_Recv_Async
      -- or Packed_U16_T_Recv_Async handlers will be called based on the data popped
      -- off the queue. The data returned is the number of items popped, which we
      -- ignore in this case, since this value is unused.

      -- Let's create a packet.
      declare
         -- Fill in the packet length with the stored packet length.
         Pkt : Packet.T := (
            Header => (
               Time => Self.Sys_Time_T_Get,
               Id => 0,
               Sequence_Count => 0,
               Buffer_Length => Self.Data_Length
            ),
            Buffer => [others => 0]
         );
      begin
         -- Set the packet data:
         Pkt.Buffer (0 .. 1) := Self.Data;

         -- Send the packet.
         Self.Packet_T_Send_If_Connected (Pkt);
      end;
   end Tick_T_Recv_Sync;

   -- This connector receives a single byte asynchronously that is used to populate the outgoing packet.
   overriding procedure Packed_Byte_T_Recv_Async (Self : in out Instance; Arg : in Packed_Byte.T) is
   begin
      -- Save off the new packet data and length:
      Self.Data (0 .. 0) := Packed_Byte.Serialization.To_Byte_Array (Arg);
      Self.Data_Length := Packed_Byte.Serialization.Serialized_Length; -- 1
   end Packed_Byte_T_Recv_Async;

   -- This connector receives a 16-bit number asynchronously that is used to populate the outgoing packet.
   overriding procedure Packed_U16_T_Recv_Async (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Save off the new packet data and length:
      Self.Data (0 .. 1) := Packed_U16.Serialization.To_Byte_Array (Arg);
      Self.Data_Length := Packed_U16.Serialization.Serialized_Length; -- 2
   end Packed_U16_T_Recv_Async;

   -- This procedure is called when a Packed_Byte_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Packed_Byte_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Packed_Byte.T) is
      Ignore : Instance renames Self;
   begin
      Put_Line ("Oh no! The queue overflowed!");
   end Packed_Byte_T_Recv_Async_Dropped;

   -- This procedure is called when a Packed_U16_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Packed_U16_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Packed_U16.T) is
      Ignore : Instance renames Self;
   begin
      Put_Line ("Oh no! The queue overflowed!");
   end Packed_U16_T_Recv_Async_Dropped;

end Component.Queued_Component.Implementation;
