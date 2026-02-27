--------------------------------------------------------------------------------
-- Connector_Counter_8 Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Command;
with Protected_Variables;

-- This generic component attaches to a connector and counts the invocations occurring on that connector. It then reports this count as a data product and passes the connector invocation through to a connected component on the other side. The count rolls over at 255 and is 1 byte wide.
generic
package Component.Connector_Counter_8.Implementation is

   -- The component class instance record:
   type Instance is new Connector_Counter_8.Base_Instance with private;

private

   -- Instantiate protected 8 bit counter:
   package Byte_Counter is new Protected_Variables.Generic_Protected_Counter (Basic_Types.Byte);

   -- The component class instance record:
   type Instance is new Connector_Counter_8.Base_Instance with record
      Count : Byte_Counter.Counter;
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
   -- The generic invokee connector.
   overriding procedure T_Recv_Sync (Self : in out Instance; Arg : in T);
   -- The command receive connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a T_Send message is dropped due to a full queue.
   overriding procedure T_Send_Dropped (Self : in out Instance; Arg : in T) is null;
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the component.
   -- This command resets the internal count.
   overriding function Reset_Count (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Connector_Counter_8.Implementation;
