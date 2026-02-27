--------------------------------------------------------------------------------
-- Zero_Divider Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;

-- The purpose of this component is to provide a safe, commandable way to cause the Ada Last Chance Handler to be called. To accomplish this, this component provides a Divide_By_Zero command which divides an integer by zero, which causes an Ada exception to be thrown, which is purposely not handled. The Divide_By_Zero command must be passed a magic number as an argument. If the magic number does not match the number that this component is instantiated with at initialization, then the Divide_By_Zero is not executed. This feature prevents inadvertent execution of this command.
package Component.Zero_Divider.Implementation is

   -- The component class instance record:
   type Instance is new Zero_Divider.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The magic number is provided at instantiation.
   --
   -- Init Parameters:
   -- Magic_Number : Magic_Number_Type - Pick a number that must be provided with the Divide_By_Zero command for it to be executed. If any other number is provided, the command is failed and no divide by zero instruction is executed. Note - The values of 0 and 1 are not accepted as magic numbers.
   -- Sleep_Before_Divide_Ms : Natural - The number of milliseconds to sleep after receiving the command but before performing the divide by zero. This allows time for any events to be written by the component, if desired.
   --
   overriding procedure Init (Self : in out Instance; Magic_Number : in Magic_Number_Type; Sleep_Before_Divide_Ms : in Natural := 1_000);

private

   -- The component class instance record:
   type Instance is new Zero_Divider.Base_Instance with record
      -- Set reasonable defaults for these if Init is not called for some reason.
      Magic_Number : Magic_Number_Type := Magic_Number_Type'Last - 5;
      Sleep_Before_Divide_Ms : Natural := 1_000; -- 1 second
      Zero : Natural := 0;
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
   -- The command receive connector
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    Commands for the Zero Divider component.
   -- You must provide the correct magic number as argument to this command for it to be executed.
   overriding function Divide_By_Zero (Self : in out Instance; Arg : in Packed_U32.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Zero_Divider.Implementation;
