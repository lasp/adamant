--------------------------------------------------------------------------------
-- Cpu_Monitor Component Implementation Body
--------------------------------------------------------------------------------

with Interrupt_Cpu_Usage;
with Basic_Types;
with Packet_Types;
with Ada.Task_Identification;

package body Component.Cpu_Monitor.Implementation is

   use Ada.Real_Time;
   use Ada.Execution_Time;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of interrupts and tasks ids to monitor.
   --
   -- Init Parameters:
   -- Task_List : Task_Types.Task_Info_List_Access - A list of task info records to monitor.
   -- Interrupt_List : Interrupt_Types.Interrupt_Id_List_Access - A list of interrupt ids to monitor.
   -- Execution_Periods : Execution_Periods_Type - The period (in ticks) that specify the duration of time that each CPU measurement is taken over.
   -- Packet_Period : Interfaces.Unsigned_16 - The period (in ticks) of how often to send out the cpu execution packet. A value of zero disables sending of the packet.
   --
   overriding procedure Init
      (Self : in out Instance; Task_List : in not null Task_Types.Task_Info_List_Access; Interrupt_List : in not null Interrupt_Types.Interrupt_Id_List_Access; Execution_Periods : in Execution_Periods_Type := [1, 6, 30]; Packet_Period : in Interfaces.Unsigned_16 := 1)
   is
      Period : Natural;
      Current_Up_Time : constant Time := Ada.Real_Time.Clock;
   begin

      -- Set component variables:
      Self.Tasks := Task_List;
      Self.Interrupts := Interrupt_List;
      Self.Execution_Periods := Execution_Periods;
      Self.Packet_Counter.Set_Period_And_Reset_Count (Packet_Period);

      -- Allocate space on the heap to store the last measured CPU time for all of
      -- the tasks and interrupts.
      Self.Task_Cpu_Time_List := new Last_Cpu_Time_Array (Self.Tasks'Range);
      Self.Task_Cpu_Time_List.all := [others => [others => Time_Of (0)]];
      Self.Task_Up_Time_List := new Last_Time_Array (Self.Tasks'Range);
      Self.Task_Up_Time_List.all := [others => [others => Current_Up_Time]];
      Self.Interrupt_Cpu_Time_List := new Last_Cpu_Time_Array (Self.Interrupts'Range);
      Self.Interrupt_Cpu_Time_List.all := [others => [others => Time_Of (0)]];
      Self.Interrupt_Up_Time_List := new Last_Time_Array (Self.Interrupts'Range);
      Self.Interrupt_Up_Time_List.all := [others => [others => Current_Up_Time]];

      -- Set the packet length. Each data point for
      -- cpu usage is a percentage that is 1 byte large. We store 3 of these per
      -- task and interrupt.
      Self.Packet_To_Send := (
         Header => (
            Time => (0, 0),
            Id => Self.Packets.Get_Cpu_Usage_Packet_Id,
            Sequence_Count => 0,
            Buffer_Length => Self.Execution_Periods'Length * (Self.Task_Cpu_Time_List'Length + Self.Interrupt_Cpu_Time_List'Length)
         ),
         Buffer => [others => 0]
      );

      -- Calculate the Max_Count. This is the rollover value for count. To make sure
      -- rollover does not skip ticks we need to select a Max_Count
      -- value that is divisible by execution periods. The simplest way to do this is
      -- to simply multiply all the periods together.
      Self.Max_Count := 1;
      for Index in Self.Execution_Periods'Range loop
         -- Ignore zero entries, since this special value means that the connector index
         -- is disabled, thus it should not be included in the calculation.
         Period := Self.Execution_Periods (Index);
         Self.Max_Count := @ * Period;
      end loop;
   end Init;

   -- Send out packet period data product:
   overriding procedure Set_Up (Self : in out Instance) is
   begin
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packet_Period (Self.Sys_Time_T_Get, (Value => Self.Packet_Counter.Get_Period)));
   end Set_Up;

   -- Calculate the cpu percent usage as a byte value from 0 to 100:
   function Cpu_Percentage (Current_Time : in Time; Previous_Time : in Time; Current_Execution_Time : in CPU_Time; Previous_Execution_Time : in CPU_Time) return Basic_Types.Byte is
      use Basic_Types;
      Wall_Time_Span : constant Time_Span := Current_Time - Previous_Time;
      Execution_Time_Span : constant Time_Span := Current_Execution_Time - Previous_Execution_Time;
      Usage : constant Integer := (Execution_Time_Span * 100) / Wall_Time_Span;
   begin
      if Usage > Integer (Byte'Last) then
         return Byte'Last;
      elsif Usage < Integer (Byte'First) then
         return Byte'First;
      else
         return Byte (Usage);
      end if;
   exception
      -- Handle divide by zero error, which may happen on start up.
      -- A constraint error could occur too if the times are wonky.
      when others =>
         return 0;
   end Cpu_Percentage;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
      use Packet_Types;
      use Ada.Task_Identification;
      Ignore : Tick.T renames Arg;
      Prev_Up_Time : Time;
      Curr_Up_Time : Time;
      Prev_Cpu_Time : CPU_Time;
      Curr_Cpu_Time : CPU_Time;
   begin
      -- If any of the three time periods have elapsed then take a new measurement for the CPU times
      -- for each task/interrupt during that time period.
      for Idx in Self.Execution_Periods'Range loop
         if (Self.Count mod Self.Execution_Periods (Idx)) = 0 then
            -- Update the execution times of all the tasks:
            for Task_Num in Self.Task_Cpu_Time_List'Range loop
               -- Make sure the task id is not null:
               if Self.Tasks (Task_Num).Id /= Ada.Task_Identification.Null_Task_Id then
                  -- Get the current and previously measured cpu times and current time:
                  Prev_Cpu_Time := Self.Task_Cpu_Time_List (Task_Num) (Idx);
                  Prev_Up_Time := Self.Task_Up_Time_List (Task_Num) (Idx);
                  Curr_Cpu_Time := Ada.Execution_Time.Clock (Self.Tasks (Task_Num).Id);
                  Curr_Up_Time := Ada.Real_Time.Clock;

                  -- Calculate the cpu usage and store it in the packet:
                  Self.Packet_To_Send.Buffer (
                     Self.Execution_Periods'Length * (Task_Num - Self.Task_Cpu_Time_List'First) + Natural (Idx - Self.Execution_Periods'First)
                  ) := Cpu_Percentage (Curr_Up_Time, Prev_Up_Time, Curr_Cpu_Time, Prev_Cpu_Time);

                  -- Update the last measured cpu time and wall time:
                  Self.Task_Cpu_Time_List (Task_Num) (Idx) := Curr_Cpu_Time;
                  Self.Task_Up_Time_List (Task_Num) (Idx) := Curr_Up_Time;
               end if;
            end loop;

            -- Update the execution times of all the interrupts:
            for Interrupt_Num in Self.Interrupt_Cpu_Time_List'Range loop
               -- Get the current and previously measured cpu times:
               Prev_Cpu_Time := Self.Interrupt_Cpu_Time_List (Interrupt_Num) (Idx);
               Prev_Up_Time := Self.Interrupt_Up_Time_List (Interrupt_Num) (Idx);
               Curr_Cpu_Time := Interrupt_Cpu_Usage.Interrupt_Clock (Self.Interrupts (Interrupt_Num));
               Curr_Up_Time := Ada.Real_Time.Clock;

               -- Calculate the cpu usage and store it in the packet:
               Self.Packet_To_Send.Buffer (
                  Self.Execution_Periods'Length * Self.Task_Cpu_Time_List'Length + Self.Execution_Periods'Length * (Interrupt_Num - Self.Interrupt_Cpu_Time_List'First) + Natural (Idx - Self.Execution_Periods'First)
               ) := Cpu_Percentage (Curr_Up_Time, Prev_Up_Time, Curr_Cpu_Time, Prev_Cpu_Time);

               -- Update the last measured cpu time:
               Self.Interrupt_Cpu_Time_List (Interrupt_Num) (Idx) := Curr_Cpu_Time;
               Self.Interrupt_Up_Time_List (Interrupt_Num) (Idx) := Curr_Up_Time;
            end loop;
         end if;
      end loop;

      -- Send out the packet with the appropriate time:
      if Self.Packet_Counter.Is_Count_At_Period then
         Self.Packet_To_Send.Header.Time := Self.Sys_Time_T_Get;
         Self.Packet_T_Send_If_Connected (Self.Packet_To_Send);
         Self.Packet_To_Send.Header.Sequence_Count := @ + 1;
      end if;

      -- Increment the packet count:
      Self.Packet_Counter.Increment_Count;

      -- Roll over the count if necessary. The rollover value is the
      -- product of the divisors.
      -- Note: this will fail with a divide by zero error if init is
      -- never called. This behavior is by design.
      Self.Count := (@ + 1) mod Self.Max_Count;
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
   --    These are the commands for the CPU Monitor component.
   -- Set the period of the packet. A period of zero disables the sending of the packet.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
      The_Time : constant Sys_Time.T := Self.Sys_Time_T_Get;
   begin
      -- Set new period:
      Self.Packet_Counter.Set_Period_And_Reset_Count (Arg.Value);
      -- Update period data product:
      Self.Data_Product_T_Send_If_Connected (Self.Data_Products.Packet_Period (The_Time, (Value => Self.Packet_Counter.Get_Period)));
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
         Errant_Field => Errant_Field
      )));
   end Invalid_Command;

end Component.Cpu_Monitor.Implementation;
