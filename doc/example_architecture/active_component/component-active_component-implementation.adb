--------------------------------------------------------------------------------
-- Active_Component Component Implementation Body
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Component.Active_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector receives a single byte asynchronously that is used to populate the outgoing packet.
   overriding procedure Packed_Byte_T_Recv_Async (Self : in out Instance; Arg : in Packed_Byte.T) is
      -- Fill in the packet length with the packet length.
      Pkt : Packet.T := (
         Header => (
            Time => Self.Sys_Time_T_Get,
            Id => 0,
            Sequence_Count => 0,
            Buffer_Length => Packed_Byte.Serialization.Serialized_Length
         ),
         Buffer => [others => 0]
      );
   begin
      -- Set the packet data:
      Pkt.Buffer (0 .. 0) := Packed_Byte.Serialization.To_Byte_Array (Arg);

      -- Send the packet.
      Self.Packet_T_Send_If_Connected (Pkt);
   end Packed_Byte_T_Recv_Async;

   -- This connector receives a 16-bit number asynchronously that is used to populate the outgoing packet.
   overriding procedure Packed_U16_T_Recv_Async (Self : in out Instance; Arg : in Packed_U16.T) is
      -- Fill in the packet length with the packet length.
      Pkt : Packet.T := (
         Header => (
            Time => Self.Sys_Time_T_Get,
            Id => 0, Sequence_Count => 0,
            Buffer_Length => Packed_U16.Serialization.Serialized_Length
         ),
         Buffer => [others => 0]
      );
   begin
      -- Set the packet data:
      Pkt.Buffer (0 .. 1) := Packed_U16.Serialization.To_Byte_Array (Arg);

      -- Send the packet.
      Self.Packet_T_Send_If_Connected (Pkt);
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

end Component.Active_Component.Implementation;
