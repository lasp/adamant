--------------------------------------------------------------------------------
-- Queue_Monitor Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;
with Protected_Variables;

-- This component produces a packet holding the queue current percent usage and maximum usage (high water mark) for each queued component in a particular assembly. It is provided an autocoded data structure upon initialization that contains the queued components that it will monitor. The component is designed to operate on a lower priority rate group running in the background.
package Component.Queue_Monitor.Implementation is

   -- The component class instance record:
   type Instance is new Queue_Monitor.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires a list of components whose queues it needs to monitor.
   --
   -- Init Parameters:
   -- Queued_Component_List : Component.Component_List_Access - A list of components to monitor.
   -- Packet_Period : Interfaces.Unsigned_16 - The period (in ticks) of how often to send out the queue usage packet. A value of zero disables sending of the packet.
   --
   overriding procedure Init (Self : in out Instance; Queued_Component_List : in not null Component.Component_List_Access; Packet_Period : in Interfaces.Unsigned_16 := 1);

private

   -- Protected object for protecting data that can be modified by synchronous command.
   package Protected_Unsigned_16_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (Interfaces.Unsigned_16);

   -- The component class instance record:
   type Instance is new Queue_Monitor.Base_Instance with record
      Queued_Component_List : Component.Component_List_Access := null;
      Packet_To_Send : Packet.T;
      Packet_Counter : Protected_Unsigned_16_Counter.Counter;
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
   --    These are the commands for the Queue Monitor component.
   -- Set the period of the packet. A period of zero disables the sending of the packet.
   overriding function Set_Packet_Period (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Queue_Monitor.Implementation;
