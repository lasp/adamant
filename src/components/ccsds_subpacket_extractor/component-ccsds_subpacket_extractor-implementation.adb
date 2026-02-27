--------------------------------------------------------------------------------
-- Ccsds_Subpacket_Extractor Component Implementation Body
--------------------------------------------------------------------------------

with Ccsds_Primary_Header;
with Serializer_Types;
with Basic_Types;

package body Component.Ccsds_Subpacket_Extractor.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component extracts CCSDS subpackets from the data section of a larger CCSDS packet. The init function allows the component to ignore the first, n, or last, m, number of bytes during extraction. This might be useful to ignore a leading secondary header or a trailing checksum.
   --
   -- Init Parameters:
   -- Start_Offset : Natural - The number of bytes past the primary CCSDS header to start extracting subpackets from.
   -- Stop_Offset : Natural - The number of bytes at the end of CCSDS packet (calculated using the primary header length field) to not extract subpackets from. This value should be used to ignore X number of bytes at the end of a packet.
   -- Max_Subpackets_To_Extract : Integer - The maximum number of subpackets to attempt to extract from an incoming packet. A negative number indicates that there is no upper limit to the amount of subpackets that can be extracted. A value of zero disables any subpacketization, which might be useful to disable this component during testing.
   --
   overriding procedure Init (Self : in out Instance; Start_Offset : in Natural := 0; Stop_Offset : in Natural := 0; Max_Subpackets_To_Extract : in Integer := -1) is
   begin
      Self.Start_Offset := Start_Offset;
      Self.Stop_Offset := Stop_Offset;
      Self.Max_Subpackets_To_Extract := Max_Subpackets_To_Extract;
   end Init;

   procedure Drop_Packet (Self : in out Instance; Arg : in Ccsds_Space_Packet.T; Evt : in Event.T) is
   begin
      -- Throw the event:
      Self.Event_T_Send_If_Connected (Evt);
      -- Forward the packet out as an error packet:
      Self.Packet_T_Send_If_Connected (Self.Packets.Error_Packet_Truncate (Evt.Header.Time, Arg));
   end Drop_Packet;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The synchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Sync (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
      Actual_Packet_Data_Length : constant Natural := Natural (Arg.Header.Packet_Length) + 1;
      Corrected_Packet_Data_Length : constant Integer := Actual_Packet_Data_Length - Self.Start_Offset - Self.Stop_Offset;
      Packet_End_Index : constant Integer := Actual_Packet_Data_Length - Self.Stop_Offset - 1;
   begin
      -- Make sure CCSDS packet is large enough to contain subpackets, but not larger than the data size itself:
      if Corrected_Packet_Data_Length < Ccsds_Space_Packet.Min_Serialized_Length then
         Self.Drop_Packet (Arg, Self.Events.Invalid_Received_Packet_Length (Self.Sys_Time_T_Get, (
            Ccsds_Header => Arg.Header,
            Length => Corrected_Packet_Data_Length,
            Length_Bound => Ccsds_Space_Packet.Min_Serialized_Length
         )));
         -- Make sure the outer packet length is not bigger than the maximum size of a CCSDS packet data section:
      elsif Actual_Packet_Data_Length > Arg.Data'Length then
         Self.Drop_Packet (Arg, Self.Events.Invalid_Received_Packet_Length (Self.Sys_Time_T_Get, (
            Ccsds_Header => Arg.Header,
            Length => Actual_Packet_Data_Length,
            Length_Bound => Arg.Data'Length
         )));
      else
         declare
            use Serializer_Types;
            Idx : Natural := Self.Start_Offset;
            Subpacket : Ccsds_Space_Packet.T;
            Stat : Serialization_Status;
            Num_Bytes_Deserialized : Natural;
            Num_Subpackets_Sent : Natural := 0;
         begin
            -- The component should only be configured in this case, for testing, but if the
            -- max number of packets to extract is zero, then don't try to do anything:
            if Self.Max_Subpackets_To_Extract = 0 then
               return;
            end if;

            while Idx <= Packet_End_Index loop
               Stat := Ccsds_Space_Packet.Serialization.From_Byte_Array (Subpacket, Arg.Data (Idx .. Packet_End_Index), Num_Bytes_Deserialized);
               if Stat = Success then
                  -- Send out subpacket:
                  Self.Ccsds_Space_Packet_T_Send (Subpacket);
                  Idx := @ + Num_Bytes_Deserialized;

                  -- If the maximum number of packets is set positive, then we want to limit the number of packets extracted.
                  -- If we have reached this limit, then stop extraction. If the max limit is negative, then we don't limit the
                  -- number of packets extracted at all.
                  if Self.Max_Subpackets_To_Extract > 0 then
                     Num_Subpackets_Sent := @ + 1;
                     if Num_Subpackets_Sent >= Self.Max_Subpackets_To_Extract then
                        exit;
                     end if;
                  end if;
               else
                  declare
                     Num_Remaining_Bytes : constant Natural := Packet_End_Index - Idx + 1;
                  begin
                     -- Determine if there is enough room for a header to be stored in the data, if so extract it and send it in an event:
                     if Num_Remaining_Bytes >= Ccsds_Primary_Header.Size_In_Bytes then
                        declare
                           Header : constant Ccsds_Primary_Header.T := Ccsds_Primary_Header.Serialization.From_Byte_Array (Arg.Data (Idx .. Idx + Ccsds_Primary_Header.Serialization.Serialized_Length - 1));
                        begin
                           Self.Drop_Packet (Arg, Self.Events.Invalid_Extracted_Packet_Length (Self.Sys_Time_T_Get, (
                              Ccsds_Header => Header,
                              Length => Natural (Header.Packet_Length),
                              Length_Bound => Num_Remaining_Bytes
                           )));
                        end;
                     else
                        -- Send the number of extra bytes found down as an event.
                        Self.Drop_Packet (Arg, Self.Events.Dropped_Trailing_Bytes (Self.Sys_Time_T_Get, (Value => Basic_Types.Byte (Num_Remaining_Bytes))));
                     end if;
                  end;
                  -- Exit loop:
                  exit;
               end if;
            end loop;
         end;
      end if;
   end Ccsds_Space_Packet_T_Recv_Sync;

   -- The asynchronous ccsds packet receive connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      -- Call the synchronous connector handler:
      Self.Ccsds_Space_Packet_T_Recv_Sync (Arg);
   end Ccsds_Space_Packet_T_Recv_Async;

   -- Throw an event if a packet was dropped on the asynchronous connector.
   overriding procedure Ccsds_Space_Packet_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Ccsds_Space_Packet.T) is
   begin
      Self.Drop_Packet (Arg, Self.Events.Dropped_Packet (Self.Sys_Time_T_Get, Arg.Header));
   end Ccsds_Space_Packet_T_Recv_Async_Dropped;

end Component.Ccsds_Subpacket_Extractor.Implementation;
