--------------------------------------------------------------------------------
-- Event_Packetizer Component Implementation Body
--------------------------------------------------------------------------------

with Event_Header;
with Packet_Types;
with Safe_Deallocator;

package body Component.Event_Packetizer.Implementation is

   -- Adding some compilation error checks. This component makes the following
   -- assumptions about the sizes of the datatypes it is converting to/from:
   --
   -- The packet size must be bigger than the total event size. Otherwise a
   -- runtime error can occur where a very large event overflows a packet.
   pragma Compile_Time_Error (Event.Size_In_Bytes > Packet_Types.Packet_Buffer_Type'Length, "Size mismatch detected. The max size of an event is too large to fit into an Adamant packet!");

   ---------------------------------------
   -- Protected packet data structure:
   ---------------------------------------

   -- Return status for the Try_Insert_Event function:
   type Try_Insert_Status is
      (Success, -- Packet inserted successfully and there is remaining space left in the packet
       Fail_Packet_Full -- The packet is too full to insert the event
   );

   -- Return type for packet status:
   type Packet_Status is (Empty, Full, Partial);

   protected body Protected_Packet_Array is

      -- Allocate memory for the internal packets:
      procedure Init (Num_Internal_Packets : in Two_Or_More) is
      begin
         -- Allocate the packets:
         Packets := new Packet_Array (Packet_Array_Index'First .. (Packet_Array_Index'First + Num_Internal_Packets - 1));

         -- Zero out the packet memory, and set the ids:
         for Idx in Packets'Range loop
            Packets (Idx) := (Header => (Time => (0, 0), Id => 0, Sequence_Count => 0, Buffer_Length => 0), Buffer => [others => 0]);
         end loop;

         Initialized := True;
      end Init;

      procedure Destroy is
         procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Packet_Array, Name => Packet_Array_Access);
      begin
         -- Free packet array from heap:
         Free_If_Testing (Packets);

         -- Reset other variables:
         Index := Packet_Array_Index'First;
         Num_Packets_Full := 0;
         Events_Dropped := 0;
         New_Packets_Dropped := False;
      end Destroy;

      -- Private function which attempts to insert an event into the current
      -- packet, and returns a status indicating if it worked or not.
      function Try_Insert_Event (Evt : in Event.T) return Try_Insert_Status is
         -- Calculate the total event size in bytes:
         Event_Header_Size : constant Natural := Event_Header.Serialization.Serialized_Length;
         Event_Size : constant Natural := Event_Header_Size + Evt.Header.Param_Buffer_Length;
         -- Calculate the total bytes remaining in the current packet:
         Current_Index : Natural := Packets (Index).Header.Buffer_Length + Packets (Index).Buffer'First;
         Header_End_Index : constant Natural := Current_Index + Event_Header_Size - 1;
         End_Index : constant Natural := Current_Index + Event_Size - 1;
      begin
         -- If event will fit, then store it in the packet.
         if End_Index <= Packets (Index).Buffer'Last then
            -- Store event header:
            Packets (Index).Buffer (Current_Index .. Header_End_Index) := Event_Header.Serialization.To_Byte_Array (Evt.Header);
            Current_Index := Header_End_Index + 1;

            -- Store event parameters:
            Packets (Index).Buffer (Current_Index .. End_Index) := Evt.Param_Buffer (Evt.Param_Buffer'First .. (Evt.Param_Buffer'First + Evt.Header.Param_Buffer_Length - 1));
            Current_Index := End_Index + 1;

            -- Store the new buffer length:
            Packets (Index).Header.Buffer_Length := Current_Index - Packets (Index).Buffer'First;

            -- Event successfully inserted and space remaining in packet:
            return Success;
         end if;

         return Fail_Packet_Full;
      end Try_Insert_Event;

      procedure Insert_Event (Evt : in Event.T; Status : out Insert_Status) is

         -- Helper procedure to increment the dropped event counter:
         procedure Increment_Events_Dropped is
         begin
            Events_Dropped := @ + 1;
            New_Packets_Dropped := True;
         end Increment_Events_Dropped;

         -- Helper procedure to increment the next index:
         procedure Next_Packet is
         begin
            -- Increment full count:
            Num_Packets_Full := @ + 1;
            pragma Assert (Num_Packets_Full <= Packets'Length);
            -- Increment index:
            if Index < Packet_Array_Index'Last then
               Index := @ + 1;
            end if;
            -- Check for roll over:
            if Index > Packets'Last then
               Index := Packets'First;
            end if;
         end Next_Packet;

         Try_Status : Try_Insert_Status;
         Next_Try_Status : Try_Insert_Status;
      begin
         -- Initialize out parameter.
         Status := Uninitialized;

         if Initialized then
            -- If all the packets are full, alert the user and increment the dropped count:
            if Num_Packets_Full = Packets'Length then
               Increment_Events_Dropped;
               Status := Packets_Full;
            -- Otherwise, try to insert the event:
            else
               -- Try to insert the event and see what status is returned:
               Try_Status := Try_Insert_Event (Evt);
               -- Depending on the status returned, perform actions:
               case Try_Status is
                  -- Everything worked, continue on.
                  when Success =>
                     Status := Success;
                  -- The packet is too full to hold the event, proceed to the next packet and try again.
                  when Fail_Packet_Full =>
                     -- Go to next packet:
                     Next_Packet;
                     -- Make sure that not all the packets are full:
                     if Num_Packets_Full = Packets'Length then
                        Increment_Events_Dropped;
                        Status := Packets_Full;
                     -- Try to insert the event in the next packet:
                     else
                        Next_Try_Status := Try_Insert_Event (Evt);
                        -- Check status of next try:
                        case Next_Try_Status is
                           -- Everything worked, continue on.
                           when Success =>
                              Status := Success;
                           -- The packet is too full to hold the event. This should only happen if an event if
                           -- bigger than a packet, which is not an acceptable design. A compile time error
                           -- should be thrown if this is the case, see the top of this file.
                           when Fail_Packet_Full =>
                              pragma Assert (False, "Event is bigger than packet.");
                        end case;
                     end if;
               end case;
            end if;
         else
            Increment_Events_Dropped;
         end if;
      end Insert_Event;

      -- Calculate the oldest index:
      function Get_Oldest_Index (Idx : out Packet_Array_Index) return Packet_Status is
         function Calculate_Oldest_Index return Packet_Array_Index is
         begin
            if Index > Num_Packets_Full then
               return Index - Num_Packets_Full;
            else
               return Packets'Last - (Num_Packets_Full - Index);
            end if;
         end Calculate_Oldest_Index;

         -- Variable to hold oldest index:
         Oldest_Index : constant Packet_Array_Index := Calculate_Oldest_Index;
      begin
         -- Provide index to user:
         Idx := Oldest_Index;

         if Num_Packets_Full > 0 then
            return Full;
         end if;

         if Packets (Oldest_Index).Header.Buffer_Length > 0 then
            return Partial;
         else
            return Empty;
         end if;
      end Get_Oldest_Index;

      -- Return a packet:
      procedure Do_Pop_Packet (Idx : in Packet_Array_Index; The_Packet : out Packet.T) is
      begin
         -- Make sure that we are never sending an empty packet. This should never happen by design.
         pragma Assert (Packets (Idx).Header.Buffer_Length > 0, "We should NEVER return an empty packet.");
         -- Set the return packet:
         -- Just copy the buffer and length, we don't want to override the
         -- timestamp and id, since we do not know those at this level.
         The_Packet.Header.Buffer_Length := Packets (Idx).Header.Buffer_Length;
         The_Packet.Buffer (The_Packet.Buffer'First .. The_Packet.Buffer'First + The_Packet.Header.Buffer_Length - 1) := Packets (Idx).Buffer (Packets (Idx).Buffer'First .. Packets (Idx).Buffer'First + The_Packet.Header.Buffer_Length - 1);
         -- Clear the packet so that it can be filled again:
         Packets (Idx).Header.Buffer_Length := 0;
      end Do_Pop_Packet;

      -- Return a packet:
      procedure Pop_Packet (The_Packet : out Packet.T; Allow_Partial_Packet : in Boolean; Status : out Pop_Status) is
         Idx : Packet_Array_Index;
         P_Status : constant Packet_Status := Get_Oldest_Index (Idx);
      begin
         case P_Status is
            when Full =>
               Do_Pop_Packet (Idx, The_Packet);
               -- Decrement the packet full count:
               Num_Packets_Full := @ - 1;
               Status := Success;
            when Partial =>
               if Allow_Partial_Packet then
                  Do_Pop_Packet (Idx, The_Packet);
               end if;
               Status := Packet_Partially_Full;
            when Empty =>
               Status := Packet_Empty;
         end case;
      end Pop_Packet;

      -- Return the number of events dropped, and whether or not that number has changed
      -- since the last time this function was called.
      procedure Get_Dropped_Count (Count : out Unsigned_32; Status : out Dropped_Count_Status) is
      begin
         case New_Packets_Dropped is
            when True =>
               Status := Increased_Since_Last_Request;
            when False =>
               Status := Same_As_Last_Request;
         end case;
         New_Packets_Dropped := False;
         Count := Events_Dropped;
      end Get_Dropped_Count;

      -- Return the number of bytes available in all the buffers for future event storage:
      function Get_Bytes_Available return Natural is
         Num_Packets_Not_Full : constant Natural := Packets'Length - Num_Packets_Full;
         Num_Bytes_Not_Full : Natural;
         Num_Bytes_Occupied_In_Current_Packet : Natural;
      begin
         if Num_Packets_Not_Full = 0 then
            return 0;
         else
            Num_Bytes_Not_Full := Num_Packets_Not_Full * Packet_Types.Packet_Buffer_Type'Length;
            Num_Bytes_Occupied_In_Current_Packet := Packets (Index).Header.Buffer_Length + Packets (Index).Buffer'First;
            return Num_Bytes_Not_Full - Num_Bytes_Occupied_In_Current_Packet;
         end if;
      end Get_Bytes_Available;

   end Protected_Packet_Array;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------

   overriding procedure Init (Self : in out Instance; Num_Internal_Packets : in Two_Or_More; Partial_Packet_Timeout : in Natural) is
   begin
      -- Initialize space on the heap for the internal packets:
      Self.Packet_Array.Init (Num_Internal_Packets);
      -- Save off variables:
      Self.Partial_Packet_Timeout := Partial_Packet_Timeout;
   end Init;

   not overriding procedure Destroy (Self : in out Instance) is
   begin
      Self.Packet_Array.Destroy;
   end Destroy;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Upon reception the component will send out one full packet, if a full packet is contained within the component. A partial packet will be sent out if the packet timeout occurs.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      -- Send a packet:
      procedure Send_Packet (A_Packet : in out Packet.T) is
         -- Grab a dummy packet with the real id and sequence count, but unfilled data, timestamp, and length.
         Dummy_Packet : Packet.T := Self.Packets.Events_Packet_Empty ((0, 0));
      begin
         -- Fill in timestamp:
         A_Packet.Header.Time := Self.Sys_Time_T_Get;
         -- Fill in sequence count and id:
         A_Packet.Header.Id := Dummy_Packet.Header.Id;
         A_Packet.Header.Sequence_Count := Dummy_Packet.Header.Sequence_Count;
         -- Send out the packet:
         Self.Packet_T_Send (A_Packet);
         -- Reset the partial packet count.
         Self.Partial_Packet_Count := 0;
      end Send_Packet;

      Ignore : Tick.T renames Arg;
      Dropped_Count : Unsigned_32;
      Bytes_Available : Natural;
      Dropped_Count_Stat : Dropped_Count_Status;
      Status : Pop_Status;
      The_Packet : Packet.T;
   begin
      -- Get the current amount of storage in the event buffer for use later:
      Bytes_Available := Self.Packet_Array.Get_Bytes_Available;

      -- Grab a partial packet if commanded, or it the partial packet count has reached the timeout.
      if Self.Send_Packet_Next_Tick or else
          (Self.Partial_Packet_Timeout > 0 and then Self.Partial_Packet_Count >= (Self.Partial_Packet_Timeout - 1))
      then
         -- Try to grab a partial packet:
         Self.Packet_Array.Pop_Packet (The_Packet, Allow_Partial_Packet => True, Status => Status);
         Self.Send_Packet_Next_Tick := False;

         -- Check the status:
         case Status is
            -- Send out the packet, even if partially full:
            when Success =>
               Send_Packet (The_Packet);
            when Packet_Partially_Full =>
               Send_Packet (The_Packet);
            when Packet_Empty =>
               null;
         end case;
      else
         -- Try to grab a full packet.
         Self.Packet_Array.Pop_Packet (The_Packet, Allow_Partial_Packet => False, Status => Status);

         -- Check the status:
         case Status is
            -- Send out the packet:
            when Success =>
               Send_Packet (The_Packet);
            -- Only increment the partial packet count if a full packet
            -- is requested, but not available.
            when Packet_Partially_Full =>
               if Self.Partial_Packet_Count < Natural'Last then
                  Self.Partial_Packet_Count := @ + 1;
               end if;
            when Packet_Empty =>
               null;
         end case;
      end if;

      -- Send out data products:
      -- Send out the free space within the component before the packet was popped, only if it has changed since the last time:
      if Self.Previous_Bytes_Available /= Bytes_Available then
         Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Bytes_Available (Self.Sys_Time_T_Get, (Value => Bytes_Available)));
         Self.Previous_Bytes_Available := Bytes_Available;
      end if;

      -- If any events were dropped, send out a dropped event count:
      Self.Packet_Array.Get_Dropped_Count (Dropped_Count, Dropped_Count_Stat);
      case Dropped_Count_Stat is
         -- Do nothing, no new events dropped.
         when Same_As_Last_Request =>
            null;
            -- Send out telemetry with new count:
         when Increased_Since_Last_Request =>
            Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Events_Dropped_Count (Self.Sys_Time_T_Get, (Value => Dropped_Count)));
      end case;

   end Tick_T_Recv_Sync;

   -- Events are received synchronously on this connector and stored into an internal packet.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
      -- We don't care if insertion fails here, we will handle that in the Tick handler
      -- by requesting the dropped event count:
      Ignore : Insert_Status;
   begin
      -- Store the event:
      Self.Packet_Array.Insert_Event (Arg, Ignore);
   end Event_T_Recv_Sync;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the event packetizer component.

   -- Send a packet out next tick, unless there are no events stored within the component.
   overriding function Send_Packet (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      Self.Send_Packet_Next_Tick := True;
      return Success;
   end Send_Packet;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
      pragma Unreferenced (Cmd);
      pragma Unreferenced (Errant_Field_Number);
      pragma Unreferenced (Errant_Field);
   begin
      -- The command was invalid. The only way this can happen is if the length of the command was invalid. In this case,
      -- there is only a single command, send_packet, so let's just forget this error happened and send the packet
      -- anyways. This shouldn't hurt anything.
      Self.Send_Packet_Next_Tick := True;
   end Invalid_Command;

end Component.Event_Packetizer.Implementation;
