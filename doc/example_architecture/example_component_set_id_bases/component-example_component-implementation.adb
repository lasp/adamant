--------------------------------------------------------------------------------
-- Example_Component Component Implementation Body
--------------------------------------------------------------------------------

package body Component.Example_Component.Implementation is

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector provides the schedule tick for the component.
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T) is
   -- TODO declarations
   begin
      null; -- TODO statements
   end Tick_T_Recv_Sync;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Command Router component. They are NOOP commands that produce events to facilitate unit testing and aliveness checks during flight.
   -- Simple NOOP command which produces an event saying that it was triggered. This can be used to self test the command routing system and verify system aliveness.
   overriding function Noop (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- TODO statements
      return Success;
   end Noop;

   -- Simple NOOP command which produces an event saying that it was triggered with a certain Arg. This can be used to self test the command argument validation system. Sending a command with an Arg value of 868 will cause the component to Fail the command. Any other value will produce a successfully executed command.
   overriding function Noop_Arg (Self : in out Instance; Arg : in Command_Router_Arg.T) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- TODO statements
      return Success;
   end Noop_Arg;

   -- A NOOP command which self tests the command response forwarding mechanism. The command handler itself acts as a command sender component, and sends out a NOOP command with a registered Source Id. The Command Router should then send out an event saying that the command response was forwarded and received again by the Command Router.
   overriding function Noop_Response (Self : in out Instance) return Command_Execution_Status.E is
      use Command_Execution_Status;
   begin
      -- TODO statements
      return Success;
   end Noop_Response;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type) is
   begin
      -- TODO: Perform action to handle an invalid command.
      -- Example:
      -- -- Throw event:
      -- self.Event_T_Send_If_Connected(self.events.Invalid_Command_Received(
      --    self.Sys_Time_T_Get,
      --    (Id => cmd.Header.Id, Errant_Field_Number => Errant_Field_Number, Errant_Field => Errant_Field)
      -- ));
      null;
   end Invalid_Command;

end Component.Example_Component.Implementation;
