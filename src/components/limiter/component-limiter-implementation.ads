--------------------------------------------------------------------------------
-- Limiter Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Parameter;
with Protected_Variables;

-- This is the limiter component. This component receives a generic type of data and queues that data. It then meters the output of the data through a "send" connector at a commandable rate. The rate is set upon initialization, can be changed by command, or by parameter. The command or parameter connections may be omitted if these features are not used.
generic
package Component.Limiter.Implementation is

   -- The component class instance record:
   type Instance is new Limiter.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- This initialization function is used to set a threshold for the maximum number of data sends that the component will produce when a tick is received.
   --
   -- Init Parameters:
   -- Max_Sends_Per_Tick : Interfaces.Unsigned_16 - The maximum number of sends that this component will produce when a tick is received. The component will stop producing packets if the threshold is met or when the queue is empty, whichever happens first.
   --
   overriding procedure Init (Self : in out Instance; Max_Sends_Per_Tick : in Interfaces.Unsigned_16);

private

   -- Protect the variable that stores the maximum number of sends per tick, since this
   -- can be changed synchronously by command, or by parameter update on two different
   -- tasks.
   package Protected_Sends_Per_Tick is new Protected_Variables.Generic_Variable (Unsigned_16);

   -- The component class instance record:
   type Instance is new Limiter.Base_Instance with record
      -- Protected max sends per tick variable:
      P_Max_Sends_Per_Tick : Protected_Sends_Per_Tick.Variable;
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
   -- The generic asynchronous invokee connector.
   overriding procedure T_Recv_Async (Self : in out Instance; Arg : in T);
   -- This procedure is called when a T_Recv_Async message is dropped due to a full queue.
   overriding procedure T_Recv_Async_Dropped (Self : in out Instance; Arg : in T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- The parameter update connector. This does not need to be connected if the parameter for this component will not be used.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T);

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
   --    These are the commands for the limiter component.
   -- Set a new value for the maximum number of sends this component will produce per tick.
   overriding function Sends_Per_Tick (Self : in out Instance; Arg : in Packed_U16.T) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

   -----------------------------------------------
   -- Parameter primitives:
   -----------------------------------------------
   -- Description:
   --    Parameters for the Limiter component.

   -- Invalid parameter handler. This procedure is called when a parameter's type is found to be invalid:
   overriding procedure Invalid_Parameter (Self : in out Instance; Par : in Parameter.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);
   -- This procedure is called when the parameters of a component have been updated. The default implementation of this
   -- subprogram in the implementation package is a null procedure. However, this procedure can, and should be implemented if
   -- something special needs to happen after a parameter update. Examples of this might be copying certain parameters to
   -- hardware registers, or performing other special functionality that only needs to be performed after parameters have
   -- been updated.
   overriding procedure Update_Parameters_Action (Self : in out Instance);
   -- This function is called when the parameter operation type is "Validate". The default implementation of this
   -- subprogram in the implementation package is a function that returns "Valid". However, this function can, and should be
   -- overridden if something special needs to happen to further validate a parameter. Examples of this might be validation of
   -- certain parameters beyond individual type ranges, or performing other special functionality that only needs to be
   -- performed after parameters have been validated. Note that range checking is performed during staging, and does not need
   -- to be implemented here.
   overriding function Validate_Parameters (
      Self : in out Instance;
      Max_Sends_Per_Tick : in Packed_U16.U
   ) return Parameter_Validation_Status.E is (Parameter_Validation_Status.Valid);

end Component.Limiter.Implementation;
