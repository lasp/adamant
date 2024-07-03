--------------------------------------------------------------------------------
-- Ccsds_Socket_Interface Component Implementation Body
--------------------------------------------------------------------------------

with Variable_Stream_Serializer;
with Ada.Real_Time;
with String_Util;
with Serializer_Types;
with Interfaces;
with GNAT.Sockets; use GNAT.Sockets;

package body Component.Ccsds_Socket_Interface.Implementation is

   ---------------------------------------
   -- Private functions:
   ---------------------------------------
   -- Function to connect to socket:
   procedure Connect (Self : in out Instance) is
   begin
      -- Try to connect to socket:
      Self.Sock.Connect (String_Util.Trim_Both (Self.Addr), Self.Port);
      if Self.Sock.Is_Connected then
         Self.Event_T_Send_If_Connected (Self.Events.Socket_Connected (Self.Sys_Time_T_Get, (Self.Sock.Get_Ip_Address, Self.Sock.Get_Port)));
      end if;
   end Connect;

   -- Function to disconnect to socket:
   procedure Disconnect (Self : in out Instance) is
   begin
      if Self.Sock.Is_Connected then
         Self.Sock.Disconnect;
         Self.Event_T_Send_If_Connected (Self.Events.Socket_Not_Connected (Self.Sys_Time_T_Get, (Self.Sock.Get_Ip_Address, Self.Sock.Get_Port)));
      end if;
   end Disconnect;

   ---------------------------------------
   -- Public functions:
   ---------------------------------------
   overriding procedure Init (Self : in out Instance; Addr : in String := "127.0.0.1"; Port : in Natural := 2_001) is
   begin
      -- Set object variables:
      pragma Assert (Addr'Length <= Self.Addr'Length, "Address string is too long.");
      Self.Addr := [others => ' ']; -- clear the string
      Self.Addr (Self.Addr'First .. Self.Addr'First + Addr'Length - 1) := Addr; -- copy addr over
      Self.Port := Port;

      -- Try to connect to socket:
      Self.Connect;
      if not Self.Sock.Is_Connected then
         Self.Event_T_Send_If_Connected (Self.Events.Socket_Not_Connected (Self.Sys_Time_T_Get, (Self.Sock.Get_Ip_Address, Self.Sock.Get_Port)));
      end if;
   end Init;

   not overriding procedure Final (Self : in out Instance) is
   begin
      Self.Sock.Disconnect;
   end Final;

   -- Serialization packages:
   package Packet_Stream_Serializer is new Variable_Stream_Serializer (Ccsds_Space_Packet.T, Ccsds_Space_Packet.Serialized_Length);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- On this connector the Socket Interface Component receives data and sends it out of the socket.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      use Serializer_Types;
   begin
      -- If we are not connected to the socket, try to connect:
      if not Self.Sock.Is_Connected then
         Self.Connect;
      end if;

      -- If we are connected to the socket then send out the packet:
      if Self.Sock.Is_Connected then
         -- Write a packet:
         declare
         begin
            if Packet_Stream_Serializer.Serialize (Self.Sock.Stream, Arg) /= Success then
               Self.Event_T_Send_If_Connected (Self.Events.Packet_Send_Failed (Self.Sys_Time_T_Get, Arg.Header));
            end if;
         exception
            -- If socket error occurs, just disconnect, and try to reconnect later...
            when Socket_Error => Self.Disconnect;
         end;
      end if;
   end Ccsds_Space_Packet_T_Recv_Async;

   -------------------------------------------------------
   -- Definition of subtasks functions for task execution:
   -------------------------------------------------------
   -- This internal task is used to listen on the socket for incoming packets.
   overriding procedure Listener (Self : in out Instance) is
      use Ada.Real_Time;
      use Serializer_Types;
      use Interfaces;

      -- Local vars:
      One_Second : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1_000);
      One_Second_Later : Ada.Real_Time.Time;
      Packet : Ccsds_Space_Packet.T;
   begin
      if Self.Sock.Is_Connected then
         -- Blocking read the packet from the socket:
         Packet.Header.Packet_Length := 0;
         declare
         begin
            if Packet_Stream_Serializer.Deserialize (Self.Sock.Stream, Minimum_Length => Ccsds_Space_Packet.Min_Serialized_Length, Output => Packet) /= Success then
               Self.Event_T_Send_If_Connected (Self.Events.Packet_Recv_Failed (Self.Sys_Time_T_Get, Packet.Header));
            else
               if Packet.Header.Packet_Length > 0 then
                  Self.Ccsds_Space_Packet_T_Send (Packet);
               end if;
            end if;
         exception
            -- If socket error occurs, just disconnect, and try to reconnect later...
            when Socket_Error => Self.Disconnect;
         end;
      else
         One_Second_Later := Ada.Real_Time.Clock + One_Second;
         -- Sleep for a bit so as to not spin the processor:
         delay until One_Second_Later;
      end if;
   end Listener;

end Component.Ccsds_Socket_Interface.Implementation;
