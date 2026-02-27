--------------------------------------------------------------------------------
-- Stack_Monitor Component Implementation Body
--------------------------------------------------------------------------------

with Basic_Types;
with Packet_Types;
with System.Storage_Elements;
with Safe_Deallocator;

package body Component.Stack_Monitor.Implementation is

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of tasks to monitor.
   --
   -- Init Parameters:
   -- Task_List : Task_Types.Task_Info_List_Access - A list of task info records to monitor.
   -- Packet_Period : Interfaces.Unsigned_16 - The period (in ticks) of how often to calculate value for and send out the packet. A period of zero disables sending of the packet.
   --
   overriding procedure Init (Self : in out Instance; Task_List : in not null Task_Types.Task_Info_List_Access; Packet_Period : in Interfaces.Unsigned_16 := 1) is
   begin
      -- Set the type variables.
      Self.Counter.Set_Period_And_Reset_Count (Packet_Period);
      Self.Tasks := Task_List;

      -- Dynamically create a list of cached indexes to speed up the calculation of the stack size. This
      -- list is the same size as the task list passed in.
      Self.Stack_Indexes := new Index_Array_Type (Task_List.all'First .. Task_List.all'Last);
      Self.Stack_Indexes.all := [others => 0];

      -- Set the packet length. Each data point for
      -- stack usage is a percentage that is 1 byte large. We store 2 of these per
      -- task to account for stack and secondary stack.
      Self.Packet_To_Send := (
         Header => (
            Time => (0, 0),
            Id => Self.Packets.Get_Stack_Usage_Packet_Id,
            Sequence_Count => 0,
            Buffer_Length => 2 * Self.Tasks.all'Length
         ),
         Buffer => [others => 0]
      );
   end Init;

   -- Send out packet period data product:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packet_Period (Self.Sys_Time_T_Get, (Value => Self.Counter.Get_Period)));
   end Set_Up;

   not overriding procedure Final (Self : in out Instance) is
      procedure Free_If_Testing is new Safe_Deallocator.Deallocate_If_Testing (
         Object => Index_Array_Type,
         Name => Index_Array_Type_Access
      );
   begin
      Free_If_Testing (Self.Stack_Indexes);
   end Final;

   ---------------------------------------
   -- Helper functions:
   ---------------------------------------

   function Calculate_Stack_Percent_Usage (Task_Data : in not null Task_Types.Task_Info_Access; Stack_Index : in out Natural) return Basic_Types.Byte is
      use System;
      Stack_Size : Natural renames Task_Data.all.Stack_Size;
   begin
      -- Some basic checks to prevent errant calculations below:
      if Stack_Size = 0 or else Task_Data.all.Stack_Address = System.Null_Address then
         return 0;
      end if;

      -- The algorithm below assumes that stack sizes are at least 100 bytes, so just return
      -- full if the stack is too small.
      if Stack_Size <= 100 then
         return 100;
      end if;

      -- Make sure the stack index is never greater than the stack size. This should never happen.
      pragma Assert (Stack_Index < Stack_Size, "Software bug or bit flip.");

      declare
         use System.Storage_Elements;
         use Basic_Types;
         -- Overlay a byte array over this component's stack.
         Stack_Low_Address : constant System.Address := Task_Data.all.Stack_Address - Storage_Offset (Stack_Size);
         Stack_Bytes : constant Byte_Array (0 .. Stack_Size - 1) with Import, Convention => Ada, Address => Stack_Low_Address;
         Start_Found : Boolean := False;
         Start_Index : Integer := Stack_Index;
      begin
         -- Figure out where we should start search for the pattern on the stack. We try to be kind of smart about this
         -- to save execution time.
         while not Start_Found loop
            -- Move the stack index back 100 bytes.
            Start_Index := @ - 100;

            -- If the start index went negative then reset it to zero and we will start from there.
            if Start_Index <= 0 then
               Start_Index := 0;
               exit;
            end if;

            -- If we read 21 bytes of the pattern in a row, then we know we have found a good starting point.
            for Idx in Natural range 1 .. 21 loop
               if Stack_Bytes (Start_Index) /= 16#CC# then
                  -- If this is ever true, then we are still in the used stack space, we need
                  -- to backtrack further until we see the pattern.
                  Start_Found := False;
                  exit;
               else
                  Start_Found := True;
               end if;

               Start_Index := @ + 1;
            end loop;
         end loop;

         -- OK we now have a valid start index that is positive. We need to start searching up in memory.
         -- As soon as we see a single byte that does not match the pattern we know we have reached the
         -- top of the stack.
         declare
            Index : Natural := Natural (Start_Index);
         begin
            while Index < Stack_Bytes'Last loop
               if Stack_Bytes (Index) /= 16#CC# then
                  exit;
               end if;
               Index := @ + 1;
            end loop;

            -- Ok, we found the first byte not matching the pattern. Save the index we found to speed up this
            -- calculation next time and return the result as a percentage.
            Stack_Index := Index;
            return Byte ((Unsigned_32 (Stack_Bytes'Last - Index) * 100) / Unsigned_32 (Stack_Bytes'Last));
         end;
      end;
   end Calculate_Stack_Percent_Usage;

   function Calculate_Secondary_Stack_Percent_Usage (Task_Data : in not null Task_Types.Task_Info_Access) return Basic_Types.Byte is
   begin
      -- Safely return the secondary stack usage, returning 0 if there is no secondary stack and 100
      -- if somehow the usage is greater than the size of the secondary stack.
      if Task_Data.all.Secondary_Stack_Size = 0 then
         return 0;
      elsif Task_Data.all.Secondary_Stack_Max_Usage >= Task_Data.all.Secondary_Stack_Size then
         return 100;
      else
         return Basic_Types.Byte ((Task_Data.all.Secondary_Stack_Max_Usage * 100) / Task_Data.all.Secondary_Stack_Size);
      end if;
   end Calculate_Secondary_Stack_Percent_Usage;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Packet_Types;
      Ignore : Tick.T renames Arg;
      Idx : Natural := Self.Packet_To_Send.Buffer'First;
   begin
      -- Only calculate stack usage and send out the packet if the period is not set to
      -- zero, and it is time.
      if Self.Counter.Is_Count_At_Period then
         -- Calculate the task usage for each task in our list:
         for Task_Info_Idx in Self.Tasks'Range loop
            Self.Packet_To_Send.Buffer (Idx) := Calculate_Stack_Percent_Usage (Self.Tasks (Task_Info_Idx), Self.Stack_Indexes.all (Task_Info_Idx));
            Idx := @ + 1;
            Self.Packet_To_Send.Buffer (Idx) := Calculate_Secondary_Stack_Percent_Usage (Self.Tasks (Task_Info_Idx));
            Idx := @ + 1;
         end loop;
         -- Make sure the right number of bytes were filled in in the packet.
         pragma Assert (Idx - Self.Packet_To_Send.Buffer'First = Self.Packet_To_Send.Header.Buffer_Length);

         -- Timestamp and send the packet:
         Self.Packet_To_Send.Header.Time := Self.Sys_Time_T_Get;
         Self.Packet_T_Send_If_Connected (Self.Packet_To_Send);
         Self.Packet_To_Send.Header.Sequence_Count := @ + 1;
      end if;

      -- Increment the count:
      Self.Counter.Increment_Count;
   end Tick_T_Recv_Sync;

   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
      -- Execute the command:
      Stat : constant Command_Response_Status.E := Self.Execute_Command (Arg);
   begin
      -- Send the return status:
      Self.Command_Response_T_Send_If_Connected ((Source_Id => Arg.Header.Source_Id, Registration_Id => Self.Command_Reg_Id, Command_Id => Arg.Header.Id, Status => Stat));
   end Command_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Stack Monitor component.
   -- Set the period of the packet. A period of zero disables the sending of the packet.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new period:
      Self.Counter.Set_Period_And_Reset_Count (Arg.Value);
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packet_Period (The_Time, (Value => Self.Counter.Get_Period)));
      -- Send out event:
      Self.Event_T_Send_If_Connected (Self.Events.Packet_Period_Set (The_Time, Arg));
      return Success;
   end Set_Packet_Period;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      Self.Event_T_Send_If_Connected (Self.Events.Invalid_Command_Received (Self.Sys_Time_T_Get, (
         Id => Cmd.Header.Id,
         Errant_Field_Number => Errant_Field_Number,
         Errant_Field => Errant_Field)
      ));
   end Invalid_Command;

end Component.Stack_Monitor.Implementation;
