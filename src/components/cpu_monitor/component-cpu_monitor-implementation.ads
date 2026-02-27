--------------------------------------------------------------------------------
-- Cpu_Monitor Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Ada.Real_Time;
with Ada.Execution_Time;
with Packet;
with Protected_Variables;

-- This component produces a packet holding the CPU execution time for all tasks and interrupts configured for a particular assembly. It is provided an autocoded data structure upon initialization that contains the tasks and interrupts which it is to monitor. The packet produced contains 3 CPU execution numbers (1 bytes in size ranging from 0 - 100) for each task/interrupt, corresponding to different length time periods. The length of these time periods is also specified at initialization as multiples of the master tick driving the component.
package Component.Cpu_Monitor.Implementation is

   -- The component class instance record:
   type Instance is new Cpu_Monitor.Base_Instance with private;

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
   overriding procedure Init (Self : in out Instance; Task_List : in not null Task_Types.Task_Info_List_Access; Interrupt_List : in not null Interrupt_Types.Interrupt_Id_List_Access; Execution_Periods : in Execution_Periods_Type := [1, 6, 30]; Packet_Period : in Interfaces.Unsigned_16 := 1);

private

   -- Array of times representing the last time that cpu measurements were taken:
   type Last_Time_Type is array (Num_Measurement_Periods) of Ada.Real_Time.Time;
   -- Time measurement array, so we can store this for each task/interrupt:
   type Last_Time_Array is array (Natural range <>) of Last_Time_Type;
   type Last_Time_Array_Access is access all Last_Time_Array;
   -- Array of cpu times representing the last cpu measurements that were taken:
   type Last_Cpu_Time_Type is array (Num_Measurement_Periods) of Ada.Execution_Time.CPU_Time;
   -- Cpu time measurement array, so we can store this for each task/interrupt:
   type Last_Cpu_Time_Array is array (Natural range <>) of Last_Cpu_Time_Type;
   type Last_Cpu_Time_Array_Access is access all Last_Cpu_Time_Array;

   -- Protected object for protecting data that can be modified by synchronous command.
   package Protected_Unsigned_16_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (Interfaces.Unsigned_16);

   -- The component class instance record:
   type Instance is new Cpu_Monitor.Base_Instance with record
      Tasks : Task_Types.Task_Info_List_Access := null;
      Interrupts : Interrupt_Types.Interrupt_Id_List_Access := null;
      Task_Up_Time_List : Last_Time_Array_Access := null;
      Task_Cpu_Time_List : Last_Cpu_Time_Array_Access := null;
      Interrupt_Up_Time_List : Last_Time_Array_Access := null;
      Interrupt_Cpu_Time_List : Last_Cpu_Time_Array_Access := null;
      Execution_Periods : Execution_Periods_Type := [1, 6, 30];
      Count : Natural := 0;
      Max_Count : Natural := 0;
      Packet_Counter : Protected_Unsigned_16_Counter.Counter;
      Packet_To_Send : Packet.T;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- Null method which can be implemented to provide some component
   -- set up code. This method is generally called by the assembly
   -- main.adb after all component initialization and tasks have been started.
   -- Some activities need to only be run once at startup, but cannot be run
   -- safely until everything is up and running, i.e. command registration, initial
   -- data product updates. This procedure should be implemented to do these things
   -- if necessary.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the CPU Monitor component.
   -- Set the period of the packet. A period of zero disables the sending of the packet.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Cpu_Monitor.Implementation;
