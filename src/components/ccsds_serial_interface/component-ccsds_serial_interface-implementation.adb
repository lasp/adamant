--------------------------------------------------------------------------------
-- Ccsds_Serial_Interface Component Implementation Body
--------------------------------------------------------------------------------

with Serializer_Types;
with Diagnostic_Uart;
with Ccsds_Primary_Header;
with Interfaces;
with Ada.Execution_Time;
with Ada.Real_Time;
with Sleep;
-- with Ada.Text_IO; use Ada.Text_IO;
-- with Basic_Types.Representation;

package body Component.Ccsds_Serial_Interface.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- Init to provide gap between packets if necessary
   --
   -- Init Parameters:
   -- Interpacket_Gap_Ms : Natural - Amount of time in milliseconds to wait in
   -- between transmission of each CCSDS packet. Some UART protocols rely on a gap to
   -- differentiate between packets, and this can be used to enforce that.
   --
   overriding procedure Init (Self : in out Instance; Interpacket_Gap_Ms : in Natural := 0) is
   begin
      Self.Interpacket_Gap_Ms := Interpacket_Gap_Ms;
   end Init;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- On this connector the Serial Interface Component receives data and sends it out of the serial port.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      use Serializer_Types;
      Num_Bytes_Serialized : Natural;
      Stat : Serialization_Status;
   begin
      Self.Count := @ + 1;

      if Self.Count > 200 then
         -- Measure execution time of listener task. This is important because the execution percentage of this task,
         -- if set to lowest priority, represents the amount execution margin available on the system.
         if Self.Task_Id_Set then
            declare
               use Ada.Real_Time;
               use Ada.Execution_Time;
               Execution_Span, Up_Span : Duration;
            begin
               Execution_Span := To_Duration (Ada.Execution_Time.Clock (Self.Listener_Task_Id) - CPU_Time_First);
               Up_Span := To_Duration (Ada.Real_Time.Clock - Time_First);
               Self.Cpu_Usage := Float (Execution_Span) / Float (Up_Span) * 100.0;
            end;
         end if;
         Self.Count := 0;
      end if;

      -- Get the length of the packet:
      Stat := Ccsds_Space_Packet.Serialized_Length (Arg, Num_Bytes_Serialized);
      if Stat /= Success then
         Self.Event_T_Send_If_Connected (Self.Events.Packet_Send_Failed (Self.Sys_Time_T_Get, Arg.Header));
      else
         -- Overlay packet with byte array and send:
         declare
            use Basic_Types;
            pragma Warnings (Off, "overlay changes scalar storage order");
            Bytes : Ccsds_Space_Packet.Serialization.Byte_Array with Import, Convention => Ada, Address => Arg'Address;
            pragma Warnings (On, "overlay changes scalar storage order");
         begin
            Diagnostic_Uart.Put (Sync_Pattern & Bytes (0 .. Num_Bytes_Serialized - 1));
            --Put_Line(Standard_Error, Basic_Types.Representation.Image(Sync_Pattern & bytes(0 .. Num_Bytes_Serialized - 1)));
            if Self.Interpacket_Gap_Ms > 0 then
               Sleep.Sleep_Ms (Self.Interpacket_Gap_Ms);
            end if;
         end;
      end if;
   end Ccsds_Space_Packet_T_Recv_Async;

   -------------------------------------------------------
   -- Definition of subtasks functions for task execution:
   -------------------------------------------------------
   -- This internal task is used to listen on the serial port for incoming packets.
   overriding procedure Listener (Self : in out Instance) is
      use Interfaces;
      A_Byte : Basic_Types.Byte;
      Count : Natural := Sync_Pattern'First;
      Bytes_Without_Sync : Unsigned_32 := 0;
   begin
      Self.Listener_Task_Id := Ada.Task_Identification.Current_Task;
      Self.Task_Id_Set := True;
      -- Put_Line(Standard_Error, "cycle serial listener");
      -- First make sure we are in sync:
      while Count <= Sync_Pattern'Last loop
         -- Read byte and see if it matches the
         -- next byte in the sync pattern.
         A_Byte := Diagnostic_Uart.Get;
         -- Put_Line(Standard_Error, "got: " & Natural'Image(Natural(A_Byte)));

         -- Check against the sync pattern:
         if A_Byte = Sync_Pattern (Count) then
            Count := @ + 1;
         elsif A_Byte = Sync_Pattern (Sync_Pattern'First) then
            Count := Sync_Pattern'First + 1;
         else
            Count := Sync_Pattern'First;
         end if;

         -- Increment bytes without sync and send out an event with every 20 bytes:
         Bytes_Without_Sync := @ + 1;
         if (Bytes_Without_Sync mod 20) = 0 then
            Self.Event_T_Send_If_Connected (Self.Events.Have_Not_Seen_Sync_Pattern (Self.Sys_Time_T_Get, (Value => Bytes_Without_Sync)));
         end if;
      end loop;

      -- OK we found an entire sync packet, now let's read
      -- a CCSDS packet:
      declare
         Packet : Ccsds_Space_Packet.T;
         pragma Warnings (Off, "overlay changes scalar storage order");
         Header_Bytes : Ccsds_Primary_Header.Serialization.Byte_Array with Import, Convention => Ada, Address => Packet.Header'Address;
         pragma Warnings (On, "overlay changes scalar storage order");
      begin
         -- First read the header:
         Diagnostic_Uart.Get (Header_Bytes);

         if (Packet.Header.Packet_Length + 1) > Packet.Data'Length then
            Self.Event_T_Send_If_Connected (Self.Events.Packet_Recv_Failed (Self.Sys_Time_T_Get, Packet.Header));
         else
            -- Now read the data:
            Diagnostic_Uart.Get (Packet.Data (0 .. Natural (Packet.Header.Packet_Length)));
            pragma Annotate (GNATSAS, False_Positive, "validity check", "Packet.Header.Packet_Length initialized by Diagnostic_Uart.Get call above");

            -- OK awesome, we got a packet, send it out of the connector:
            Self.Ccsds_Space_Packet_T_Send_If_Connected (Packet);
         end if;
      end;
   end Listener;

end Component.Ccsds_Serial_Interface.Implementation;
