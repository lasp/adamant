--------------------------------------------------------------------------------
-- Command_Router Component Implementation Spec
--------------------------------------------------------------------------------

-- Invokee Connector Includes:
with Command;

-- Custom Includes:
with Router_Table;
with Interfaces;
with Protected_Variables;
with Command_Router_Commands;

-- This component routes commands to the appropriate component for execution.
package Component.Command_Router.Implementation is

   type Instance is new Command_Router.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This component requires the maximum number of unique commands that it expects to need to route. This number will be used to size the internal router table on the heap.
   overriding procedure Init (Self : in out Instance; Max_Number_Of_Commands : in Natural);
   not overriding procedure Final (Self : in out Instance);

private

   -- Protected counter type for safely incrementing counts among threads.
   package Protected_U16_Counter is new Protected_Variables.Generic_Protected_Counter (Interfaces.Unsigned_16);

   -- The component class instance record:
   type Instance is new Command_Router.Base_Instance with record
      -- Unprotected binary tree, which holds command registrations. As long as the
      -- complete registration process happens before any commands are received, there is
      -- no race conditions possible, since the tree is read-only after that point.
      Table : Router_Table.Instance;
      -- Counters for data products, protected where needed.
      Command_Receive_Count : Protected_U16_Counter.Counter;
      Command_Failure_Count : Protected_U16_Counter.Counter;
      Command_Success_Count : Interfaces.Unsigned_16 := 0;
      -- An instance of the command router command sending object. This is used for
      -- self testing the command response forwarding feature of the command router.
      Commands : Command_Router_Commands.Instance;
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
   -- For the command router this is the initialization function used to
   -- register component commands and command sources:
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- On this connector the Command Router receives incoming commands that need to be routed to the correct destination component.
   overriding procedure Command_T_To_Route_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_To_Route_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_To_Route_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- On this connector the Command Router receives incoming commands that need to be routed to the correct destination component. This connector is synchronous, and thus bypasses the internal queue. It should be used by components that need high priority command execution. It should only be called after command registration has occurred, or a race condition is present.
   overriding procedure Command_T_To_Route_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- This is the command receive connector for the Command Router. The NOOP commands sent on this connector will be executed by the command router. This connector will usually be connected in loopback from the Command_T_Send connector in order to provide aliveness test capabilities, or disconnected completely.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);
   -- Command registrations are received on this connector during initialization. Command responses from connected components are received on this connector during execution.
   overriding procedure Command_Response_T_Recv_Async (Self : in out Instance; Arg : in Command_Response.T);
   -- This procedure is called when a Command_Response_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_Response_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command_Response.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Index : in Command_T_Send_Index; Arg : in Command.T);
   -- This procedure is called when a Command_Response_T_To_Forward_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_To_Forward_Send_Dropped (Self : in out Instance; Index : in Command_Response_T_To_Forward_Send_Index; Arg : in Command_Response.T);
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Command Router component. They are NOOP commands that produce events to facilitate unit testing and aliveness checks during flight.
   -- Simple Noop command which returns an event saying that it was triggered.
   overriding function Noop (Self : in out Instance) return Command_Execution_Status.E;
   -- Simple Noop command which returns an event saying that it was triggered with a certain Arg.
   overriding function Noop_Arg (Self : in out Instance; Arg : in Command_Router_Arg.T) return Command_Execution_Status.E;
   -- A Noop command which self tests the command response forwarding mechanism. The command handler itself acts as a command sender component, and sends out a Noop command with a registered Source Id. The Command Router should then send out an event saying that the command response was forwarded and received.
   overriding function Noop_Response (Self : in out Instance) return Command_Execution_Status.E;
   -- This command resets the values of all the component's data product to the values at initialization.
   overriding function Reset_Data_Products (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Command_Router.Implementation;
