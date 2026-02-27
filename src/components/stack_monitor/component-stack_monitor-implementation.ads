--------------------------------------------------------------------------------
-- Stack_Monitor Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Command;
with Interfaces;
with Protected_Variables;

-- This component produces a packet holding the stack and secondary stack usage for all tasks configured for a particular assembly. It is provided an autocoded data structure upon initialization that contains the tasks which it is to monitor. The packet produced contains a byte representing the percent usage of the stack and secondary stack for each task. Task usage is recalculated on every tick.
package Component.Stack_Monitor.Implementation is

   -- The component class instance record:
   type Instance is new Stack_Monitor.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of tasks to monitor.
   --
   -- Init Parameters:
   -- Task_List : Task_Types.Task_Info_List_Access - A list of task info records to monitor.
   -- Packet_Period : Interfaces.Unsigned_16 - The period (in ticks) of how often to calculate value for and send out the packet. A period of zero disables sending of the packet.
   --
   overriding procedure Init (Self : in out Instance; Task_List : in not null Task_Types.Task_Info_List_Access; Packet_Period : in Interfaces.Unsigned_16 := 1);
   not overriding procedure Final (Self : in out Instance);

   -- Create a type that is an array of naturals:
   -- This is public so the tester component can use this type.
   type Index_Array_Type is array (Natural range <>) of Natural;
   type Index_Array_Type_Access is access all Index_Array_Type;

private

   -- Protected object for protecting data that can be modified by synchronous command.
   package Protected_Unsigned_16_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (Interfaces.Unsigned_16);

   -- The component class instance record:
   type Instance is new Stack_Monitor.Base_Instance with record
      Counter : Protected_Unsigned_16_Counter.Counter;
      Tasks : Task_Types.Task_Info_List_Access := null;
      Packet_To_Send : Packet.T;
      Stack_Indexes : Index_Array_Type_Access := null;
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
   --    These are the commands for the Stack Monitor component.
   -- Set the period of the packet. A period of zero disables the sending of the packet.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Stack_Monitor.Implementation;
