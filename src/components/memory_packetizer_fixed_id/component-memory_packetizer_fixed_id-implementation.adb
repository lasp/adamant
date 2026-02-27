--------------------------------------------------------------------------------
-- Memory_Packetizer_Fixed_Id Component Implementation Body
--------------------------------------------------------------------------------

with Byte_Array_Pointer.Packed;
with Memory_Region;

package body Component.Memory_Packetizer_Fixed_Id.Implementation is

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
   -- This initialization function is used to set a threshold for the maximum number of packets that the component will produce in a single time period. A time period is measured in an integer number of seconds.
   --
   -- Init Parameters:
   -- Max_Packets_Per_Time_Period : Natural - The maximum number of packets that this component will produce in a single second. The component will stop producing packets if the threshold is met, until the end of a second period has elapsed.
   -- Time_Period_In_Seconds : Positive - The time period in seconds over which to measure the number of packets produced.
   --
   overriding procedure Init (Self : in out Instance; Max_Packets_Per_Time_Period : in Natural; Time_Period_In_Seconds : in Positive := 1) is
   begin
      -- Set the maximum packet rate:
      Do_Set_Max_Packet_Rate (Self, Max_Packets_Per_Time_Period, Time_Period_In_Seconds);
   end Init;

   -- Send out max sends per tick data product:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Max_Packets_Per_Time_Period (Self.Sys_Time_T_Get, (Max_Packets => Self.Max_Packets_Per_Time_Period, Period => Self.Time_Period_S)));
   end Set_Up;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- A memory dump pointer and id queued up for packetization on this connector.
   overriding procedure Memory_Dump_Recv_Async (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is
      use Ada.Real_Time;
      -- Save off the length of memory we need to packetize:
      Memory_Length : constant Natural := Byte_Array_Pointer.Length (Arg.Memory_Pointer);
      -- The current memory index:
      Memory_Index : Natural := 0;
   begin
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
               -- Grab an empty packet with timestamp, id, and sequence count populated:
               Packet_To_Send : Packet.T := Self.Packets.Memory_Dump_Packet_Empty (Self.Sys_Time_T_Get);
            begin
               -- Set the length:
               Packet_To_Send.Header.Buffer_Length := Buffer_Length + Mem_Header_Length;
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
         end;
      end loop;
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
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the memory packetizer fixed id component.
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

end Component.Memory_Packetizer_Fixed_Id.Implementation;
