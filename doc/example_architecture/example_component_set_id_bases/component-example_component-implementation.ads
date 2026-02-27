--------------------------------------------------------------------------------
-- Example_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Tick;

-- This is the example component.
package Component.Example_Component.Implementation is

   -- The component class instance record:
   type Instance is new Example_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Example_Component.Base_Instance with record
      null; -- TODO
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
   overriding procedure Set_Up (Self : in out Instance) is null;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Index : in Packet_T_Send_Index; Arg : in Packet.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Command Router component. They are NOOP commands that produce events to facilitate unit testing and aliveness checks during flight.
   -- Simple NOOP command which produces an event saying that it was triggered. This can be used to self test the command routing system and verify system aliveness.
   overriding function Noop (Self : in out Instance) return Command_Execution_Status.E;
   -- Simple NOOP command which produces an event saying that it was triggered with a certain Arg. This can be used to self test the command argument validation system. Sending a command with an Arg value of 868 will cause the component to Fail the command. Any other value will produce a successfully executed command.
   overriding function Noop_Arg (Self : in out Instance; Arg : in Command_Router_Arg.T) return Command_Execution_Status.E;
   -- A NOOP command which self tests the command response forwarding mechanism. The command handler itself acts as a command sender component, and sends out a NOOP command with a registered Source Id. The Command Router should then send out an event saying that the command response was forwarded and received again by the Command Router.
   overriding function Noop_Response (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Example_Component.Implementation;
