--------------------------------------------------------------------------------
-- Memory_Packetizer Component Implementation Body
--------------------------------------------------------------------------------

with Safe_Deallocator;
with Byte_Array_Pointer.Packed;
with Memory_Region;

package body Component.Memory_Packetizer.Implementation is

   -- Private method for setting the maximum packet rate:
   procedure Do_Set_Max_Packet_Rate (Self : in out Instance; Max_Packets_Per_Time_Period : in Natural; Time_Period_In_Seconds : in Positive) is
   begin
      Self.Time_Period_S := Time_Period_In_Seconds;
      Self.Time_Period := Ada.Real_Time.Milliseconds (Time_Period_In_Seconds * 1_000);
      Self.Max_Packets_Per_Time_Period := Max_Packets_Per_Time_Period;
   end Do_Set_Max_Packet_Rate;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization function is used to set a threshold for the maximum number of packets that the component will produce in a single time period. A time period is measured in an integer number of seconds. The component also needs to keep track of the sequence counts for each packet ID that it receives. To do this, it needs to allocate internal memory to keep track of the last sequence count for each packet. A maximum number of unique packet IDs is provided in this function to allocate the necessary memory to keep track of this information.
   --
   -- Init Parameters:
   -- Max_Packets_Per_Time_Period : Natural - The maximum number of packets that this component will produce in a single second. The component will stop producing packets if the threshold is met, until the end of a second period has elapsed.
   -- Time_Period_In_Seconds : Positive - The time period in seconds over which the measure the number of packets produced.
   -- Max_Packet_Ids : Positive - The maximum number of unique packet IDs that this component is expected to receive during operations. This value is used to allocate a small amount of memory at initialization to keep track of the sequence count for each produced packet. If this memory becomes fully used, any new unique packet IDs received will trigger an event and will be emitted with a sequence count of zero. This misconfiguration should easily be detectable during test.
   --
   overriding procedure Init (Self : in out Instance; Max_Packets_Per_Time_Period : in Natural; Time_Period_In_Seconds : in Positive := 1; Max_Packet_Ids : in Positive := 10) is
   begin
      -- Allocate space to store the sequence number list:
      Self.Sequence_Count_List := new Sequence_Count_Tracker_List (1 .. Max_Packet_Ids);
      -- Sent the maximum packet rate:
      Do_Set_Max_Packet_Rate (Self, Max_Packets_Per_Time_Period, Time_Period_In_Seconds);
   end Init;

   -- Send out max sends per tick data product:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Max_Packets_Per_Time_Period (Self.Sys_Time_T_Get, (Max_Packets => Self.Max_Packets_Per_Time_Period, Period => Self.Time_Period_S)));
   end Set_Up;

   not overriding procedure Final (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (Object => Sequence_Count_Tracker_List, Name => Sequence_Count_Tracker_List_Access);
   begin
      if Self.Sequence_Count_List /= null then
         Free_If_Testing (Self.Sequence_Count_List);
      end if;
   end Final;

   function Get_Sequence_Count_Entry_Index (Self : in out Instance; Id : in Packet_Types.Packet_Id) return Natural is
      use Packet_Types;
   begin
      -- See if the id for this memory dump is already tracked in our sequence
      -- number tracking list, or track the new id:
      for Idx in Self.Sequence_Count_List'Range loop
         if Self.Sequence_Count_List (Idx).Used then
            if Self.Sequence_Count_List (Idx).Id = Id then
               -- We found it.
               return Idx;
            end if;
         else
            -- We have reached unused entries. This must be a new packet id to keep
            -- track of, so let's use it.
            Self.Sequence_Count_List (Idx).Used := True;
            Self.Sequence_Count_List (Idx).Id := Id;
            Self.Sequence_Count_List (Idx).Sequence_Count := 0;
            return Idx;
         end if;
      end loop;

      -- If the id was not found in the sequence count tracking list, then throw
      -- an event, but continue on anyways, just sending out a sequence count of zero.
      Self.Event_T_Send_If_Connected (Self.Events.Max_Packet_Id_Exceeded (Self.Sys_Time_T_Get, (Id => Id)));
      return 0;
   end Get_Sequence_Count_Entry_Index;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- A memory dump pointer and id queued up for packetization on this connector.
   overriding procedure Memory_Dump_Recv_Async (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is
      use Ada.Real_Time;
      use Packet_Types;
      -- Get the sequence count entry index:
      Sequence_Count_Entry_Index : constant Natural := Get_Sequence_Count_Entry_Index (Self, Arg.Id);
      -- Save off the length of memory we need to packetize:
      Memory_Length : constant Natural := Byte_Array_Pointer.Length (Arg.Memory_Pointer);
      -- The current sequence count:
      Sequence_Count : Sequence_Count_Mod_Type := 0;
      -- The current memory index:
      Memory_Index : Natural := 0;
   begin
      -- Set the local sequence count if we found a valid sequence count entry:
      if Sequence_Count_Entry_Index >= Self.Sequence_Count_List'First and then
          Sequence_Count_Entry_Index <= Self.Sequence_Count_List'Last
      then
         Sequence_Count := Self.Sequence_Count_List (Sequence_Count_Entry_Index).Sequence_Count;
      end if;

      -- While there is data still left in memory dump:
      while Memory_Index < Memory_Length loop
         -- If the start of the next period is in the past, then update it to be in the
         -- future and reset the counter. This allows us to adjust the period start in
         -- times of sparse packet load.
         declare
            Current_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
         begin
            if Self.Next_Period_Start < Current_Time then
               Self.Next_Period_Start := Current_Time + Self.Time_Period;
               Self.Num_Packets_Sent := 0;
            end if;
         end;

         -- If we have sent the maximum number of packets this period then sleep
         -- until the next period.
         if Self.Num_Packets_Sent >= Self.Max_Packets_Per_Time_Period then
            -- Sleep until the end of this period:
            delay until Self.Next_Period_Start;
            -- Set the next period relative to the current one to avoid period drift
            -- during high packet load.
            Self.Next_Period_Start := @ + Self.Time_Period;
            Self.Num_Packets_Sent := 0;
         end if;

         -- Determine the length of the packet to send and send it:
         declare
            use Byte_Array_Pointer;
            -- Figure out how much data we can store. We need to subtract the size of the memory region record
            -- which will be stored first:
            Mem_Header_Length : constant Natural := Memory_Region.Serialization.Serialized_Length;
            -- Start by assuming we can send the maximum packet length:
            Buffer_Length : Natural := Packet_Types.Packet_Buffer_Length_Type'Last - Mem_Header_Length;
            -- Calculate how much memory we have left to send:
            Memory_Left : constant Natural := Memory_Length - Memory_Index;
         begin
            -- If we have more bytes to fill in our buffer than in memory, then
            -- set the buffer length to equal the amount of memory we have left.
            if Buffer_Length > Memory_Left then
               Buffer_Length := Memory_Left;
            end if;

            -- Send packet:
            declare
               use Byte_Array_Pointer.Packed;
               -- Define a slice of the pointer to copy:
               The_Slice : constant Byte_Array_Pointer.Instance := Slice (Arg.Memory_Pointer, Start_Index => Memory_Index, End_Index => Memory_Index + Buffer_Length - 1);
               -- Define the packet:
               Packet_To_Send : Packet.T := (
                  Header => (
                     Time => Self.Sys_Time_T_Get,
                     Id => Arg.Id,
                     Sequence_Count => Sequence_Count,
                     Buffer_Length => Buffer_Length + Mem_Header_Length
                  ),
                  Buffer => [others => 0]
               );
            begin
               -- Serialize the slice into a memory region packed record that includes address and length information:
               Packet_To_Send.Buffer (0 .. Mem_Header_Length - 1) := Memory_Region.Serialization.To_Byte_Array (Pack (The_Slice));
               -- Copy over data from the slice into the packet:
               Packet_To_Send.Buffer (Mem_Header_Length .. Mem_Header_Length + Buffer_Length - 1) := To_Byte_Array (The_Slice);
               -- Send the packet:
               Self.Packet_T_Send_If_Connected (Packet_To_Send);
               -- Increment the memory index:
               Memory_Index := @ + Buffer_Length;
            end;

            -- Increment the number of packets:
            Self.Num_Packets_Sent := @ + 1;

            -- Increment the sequence count, only if we are tracking this id's sequence count:
            if Sequence_Count_Entry_Index >= Self.Sequence_Count_List'First and then
                Sequence_Count_Entry_Index <= Self.Sequence_Count_List'Last
            then
               Sequence_Count := @ + 1;
            end if;
         end;
      end loop;

      -- Save the new sequence count if we found a valid sequence count entry:
      if Sequence_Count_Entry_Index >= Self.Sequence_Count_List'First and then
          Sequence_Count_Entry_Index <= Self.Sequence_Count_List'Last
      then
         Self.Sequence_Count_List (Sequence_Count_Entry_Index).Sequence_Count := Sequence_Count;
      end if;

   end Memory_Dump_Recv_Async;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Async;

   -----------------------------------------------
   -- Command handler primitive:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the memory packetizer component.

   -- Set a new value for the Max_Packets_Per_Time_Period and the Time_Period_In_Seconds to control the output rate of the emitted packets.
   overriding function Set_Max_Packet_Rate (Self : in out Instance; Arg : in Packets_Per_Period.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set the rate:
      Do_Set_Max_Packet_Rate (Self, Arg.Max_Packets, Arg.Period);
      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Max_Packets_Per_Time_Period (The_Time, (Max_Packets => Self.Max_Packets_Per_Time_Period, Period => Self.Time_Period_S)));
      -- Send event:
      Self.Event_T_Send_If_Connected (Self.Events.Max_Packet_Rate_Set (The_Time, Arg));
      return Success;
   end Set_Max_Packet_Rate;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- Perform action to handle an invalid command.
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (Id => Cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)));
   end Invalid_Command;

   -- This procedure is called when a Memory_Dump_Recv_Async message is dropped due to a full queue.
   overriding procedure Memory_Dump_Recv_Async_Dropped (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is
   begin
      -- Throw an event:
      Self.Event_T_Send_If_Connected (Self.Events.Memory_Dump_Request_Dropped (Self.Sys_Time_T_Get, (Id => Arg.Id)));
   end Memory_Dump_Recv_Async_Dropped;

end Component.Memory_Packetizer.Implementation;
