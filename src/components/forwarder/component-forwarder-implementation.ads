--------------------------------------------------------------------------------
-- Forwarder Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Protected_Variables;
with Basic_Enums;

-- This is a generic component that can be used to forward a single connector of any type. The component that synchronously forwards any type that it receives. It includes commands to enable or disable this forwarding, so can be effectively used as a stream on/off switch.
generic
package Component.Forwarder.Implementation is

   -- The component class instance record:
   type Instance is new Forwarder.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   --
   -- Init Parameters:
   -- Startup_Forwarding_State : Basic_Enums.Enable_Disable_Type.E - Is the data stream enabled or disabled on startup. Disable means that the component does not forward any data it receives at startup.
   --
   overriding procedure Init (Self : in out Instance; Startup_Forwarding_State : in Basic_Enums.Enable_Disable_Type.E);

private

   -- Protected type for the enabled/disabled state for thread safety.
   package Protected_Enable_Disable_Type is new Protected_Variables.Generic_Variable (Basic_Enums.Enable_Disable_Type.E);

   -- The component class instance record:
   type Instance is new Forwarder.Base_Instance with record
      State : Protected_Enable_Disable_Type.Variable;
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
   -- This connector is the input connector for the data that is coming in.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T) is null;
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
   --    These are the commands for the Forwarder component.
   -- Enable the flow of data.
   overriding function Enable_Forwarding (Self : in out Instance) return Command_Execution_Status.E;
   -- Disable the flow of data.
   overriding function Disable_Forwarding (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Forwarder.Implementation;
