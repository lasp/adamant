--------------------------------------------------------------------------------
-- Command_Component Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;

-- This is the command component, which executes commands.
package Component.Command_Component.Implementation is

   -- The component class instance record:
   type Instance is new Command_Component.Base_Instance with private;

private

   -- The component class instance record:
   type Instance is new Command_Component.Base_Instance with record
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
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    A set of commands for the Command Component.
   -- Prints Hello World when received.
   overriding function Hello_World (Self : in out Instance) return Command_Execution_Status.E;
   -- A command that when received checks to see if the commanded argument is equal to 5. If so the command succeeds, otherwise it fails.
   overriding function Was_Five_Sent (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Command_Component.Implementation;
