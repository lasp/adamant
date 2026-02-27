--------------------------------------------------------------------------------
-- Command_Router Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Command_Router_Reciprocal;
with Printable_History;
with Command.Representation;
with Command_Response.Representation;
with Event.Representation;
with Data_Product.Representation;
with Sys_Time.Representation;
with Event;
with Command_Header.Representation;
with Command_Id.Representation;
with Command_Router_Arg.Representation;
with Invalid_Command_Info.Representation;
with Data_Product;
with Packed_U16.Representation;
with Command_Id_Status.Representation;

-- The Command Router component receives incoming commands and routes them to the appropriate component for execution. Commands IDs are registered by components connected to the command router on initialization. These registrations are used to populate a router table (binary tree) which is used to translate incoming Command IDs to their destination component. When a command is received by the Command Router, its ID is looked up in the table, which returns the connector index on which the destination component is attached. The Command Router will then forward the command out of the appropriate index. Event errors are thrown if a Command ID is not found in the table.
--
-- In addition to routing commands the Command Router also receives command responses from the downstream components who execute commands. These responses are used to report data products on the command success and failure counts. Responses can also be forwarded to the sourcing command components, allowing command sources to check command responses or wait until a command response is received before sending a subsequent command.
--
-- The Command Router also has some of its own internal NOOP commands, to which it responds with Events. These commands can be useful for testing system aliveness.
--
-- It is advised to connect one index of the Command_T_Send connectors to the Command Router's own Command_T_Recv_Async connector in order to utilize the NOOP commands to enable self testing of command routing. Likewise, it is advisable to connect the Command Router's Command_Response_T_Send to the Command_Response_T_Recv_Async connector and one index of the Command_Response_T_To_Forward_Send connector to the Command_Response_T_Recv_Async connector in order to fully utilize the component's ability to self test command response forwarding (see the Noop_Response command).
package Component.Command_Router.Implementation.Tester is

   use Component.Command_Router_Reciprocal;
   -- Invoker connector history packages:
   package Command_T_Recv_Sync_History_Package is new Printable_History (Command.T, Command.Representation.Image);
   package Command_Response_T_To_Forward_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Command_Received_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Command_Execution_Successful_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Command_Execution_Failure_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Command_Id_Not_Registered_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Registration_Id_Conflict_History_Package is new Printable_History (Command_Id.T, Command_Id.Representation.Image);
   package Router_Table_Full_History_Package is new Printable_History (Command_Id.T, Command_Id.Representation.Image);
   package Outgoing_Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Incoming_Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Noop_Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Command_Response_Dropped_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Noop_Received_History_Package is new Printable_History (Natural, Natural'Image);
   package Noop_Arg_Received_History_Package is new Printable_History (Command_Router_Arg.T, Command_Router_Arg.Representation.Image);
   package Noop_Response_Received_History_Package is new Printable_History (Natural, Natural'Image);
   package Noop_Response_Forwarding_Success_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Forwarded_Command_Response_Dropped_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Invalid_Command_Source_Id_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Data_Products_Reset_History_Package is new Printable_History (Natural, Natural'Image);

   -- Data product history packages:
   package Command_Receive_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Command_Success_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Command_Failure_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Last_Received_Command_History_Package is new Printable_History (Command_Id.T, Command_Id.Representation.Image);
   package Last_Successful_Command_History_Package is new Printable_History (Command_Id.T, Command_Id.Representation.Image);
   package Last_Failed_Command_History_Package is new Printable_History (Command_Id_Status.T, Command_Id_Status.Representation.Image);
   package Noop_Arg_Last_Value_History_Package is new Printable_History (Command_Router_Arg.T, Command_Router_Arg.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Command_Router_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Command_Router.Implementation.Instance;
      -- Connector histories:
      Command_T_Recv_Sync_History : Command_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_To_Forward_Recv_Sync_History : Command_Response_T_To_Forward_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Command_Received_History : Command_Received_History_Package.Instance;
      Command_Execution_Successful_History : Command_Execution_Successful_History_Package.Instance;
      Command_Execution_Failure_History : Command_Execution_Failure_History_Package.Instance;
      Command_Id_Not_Registered_History : Command_Id_Not_Registered_History_Package.Instance;
      Registration_Id_Conflict_History : Registration_Id_Conflict_History_Package.Instance;
      Router_Table_Full_History : Router_Table_Full_History_Package.Instance;
      Outgoing_Command_Dropped_History : Outgoing_Command_Dropped_History_Package.Instance;
      Incoming_Command_Dropped_History : Incoming_Command_Dropped_History_Package.Instance;
      Noop_Command_Dropped_History : Noop_Command_Dropped_History_Package.Instance;
      Command_Response_Dropped_History : Command_Response_Dropped_History_Package.Instance;
      Noop_Received_History : Noop_Received_History_Package.Instance;
      Noop_Arg_Received_History : Noop_Arg_Received_History_Package.Instance;
      Noop_Response_Received_History : Noop_Response_Received_History_Package.Instance;
      Noop_Response_Forwarding_Success_History : Noop_Response_Forwarding_Success_History_Package.Instance;
      Forwarded_Command_Response_Dropped_History : Forwarded_Command_Response_Dropped_History_Package.Instance;
      Invalid_Command_Source_Id_History : Invalid_Command_Source_Id_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Data_Products_Reset_History : Data_Products_Reset_History_Package.Instance;
      -- Data product histories:
      Command_Receive_Count_History : Command_Receive_Count_History_Package.Instance;
      Command_Success_Count_History : Command_Success_Count_History_Package.Instance;
      Command_Failure_Count_History : Command_Failure_Count_History_Package.Instance;
      Last_Received_Command_History : Last_Received_Command_History_Package.Instance;
      Last_Successful_Command_History : Last_Successful_Command_History_Package.Instance;
      Last_Failed_Command_History : Last_Failed_Command_History_Package.Instance;
      Noop_Arg_Last_Value_History : Noop_Arg_Last_Value_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_To_Route_Send_Dropped : Boolean := False;
      Command_T_To_Route_Send_Dropped_Count : Natural := 0;
      Expect_Command_Response_T_Send_Dropped : Boolean := False;
      Command_Response_T_Send_Dropped_Count : Natural := 0;
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural; Command_T_Send_Count : in Connector_Count_Type; Command_Response_T_To_Forward_Send_Count : in Connector_Count_Type);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector has an unconstrained size that is determined by the assembly in which the Command Router is instantiated. Each index of the connector should connect to different destination component that receives commands. The Command Router will route commands destined for each component on the appropriate index of this connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- Command responses received from command executing components are forwarded back to their command sources using this arrayed connector. One index of this connector can be connected in loopback to the Command_Response_T_Recv_Async connector in order to command forwarding self test capabilities (see the Noop_Response command).
   overriding procedure Command_Response_T_To_Forward_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- This connector is used to register the Command Router's NOOP commands at initialization, and respond to NOOP commands during execution. It is usually connected in loopback to the Command_Response_T_Recv_Async connector.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_To_Route_Send message is dropped due to a full queue.
   overriding procedure Command_T_To_Route_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T);

   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was received by the command router to be routed.
   overriding procedure Command_Received (Self : in out Instance; Arg : in Command_Header.T);
   -- A command was routed, executed, and returned a response saying it was executed successfully
   overriding procedure Command_Execution_Successful (Self : in out Instance; Arg : in Command_Response.T);
   -- A command execution failed.
   overriding procedure Command_Execution_Failure (Self : in out Instance; Arg : in Command_Response.T);
   -- A command was sent to the router, but it was not found in the router table.
   overriding procedure Command_Id_Not_Registered (Self : in out Instance; Arg : in Command_Header.T);
   -- The command Id has already been registered.
   overriding procedure Registration_Id_Conflict (Self : in out Instance; Arg : in Command_Id.T);
   -- Cannot add command Id to router table because it is full.
   overriding procedure Router_Table_Full (Self : in out Instance; Arg : in Command_Id.T);
   -- A command was dropped because the recipient's queue was full.
   overriding procedure Outgoing_Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A command was dropped because the command router's queue was full.
   overriding procedure Incoming_Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A noop command was dropped because the command router's queue was full.
   overriding procedure Noop_Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A command response was dropped because the command router's queue was full.
   overriding procedure Command_Response_Dropped (Self : in out Instance; Arg : in Command_Response.T);
   -- A Noop command was received.
   overriding procedure Noop_Received (Self : in out Instance);
   -- A Noop command was received with an argument.
   overriding procedure Noop_Arg_Received (Self : in out Instance; Arg : in Command_Router_Arg.T);
   -- A noop response self test command was received.
   overriding procedure Noop_Response_Received (Self : in out Instance);
   -- If this event is sent then the noop response self test command succeeded.
   overriding procedure Noop_Response_Forwarding_Success (Self : in out Instance; Arg : in Command_Response.T);
   -- A forwarded command response was dropped because the receiving component's queue overflowed.
   overriding procedure Forwarded_Command_Response_Dropped (Self : in out Instance; Arg : in Command_Response.T);
   -- A command response contained an invalid source id. This is a software bug and should be corrected.
   overriding procedure Invalid_Command_Source_Id (Self : in out Instance; Arg : in Command_Response.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- The component's data products have been reset to initialization values.
   overriding procedure Data_Products_Reset (Self : in out Instance);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Command Router component.
   -- The number of commands received by the component.
   overriding procedure Command_Receive_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of commands that successfully executed.
   overriding procedure Command_Success_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of commands that failed to execute.
   overriding procedure Command_Failure_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The ID of the last received command by the command router.
   overriding procedure Last_Received_Command (Self : in out Instance; Arg : in Command_Id.T);
   -- The ID of the last successful command routed by the command router.
   overriding procedure Last_Successful_Command (Self : in out Instance; Arg : in Command_Id.T);
   -- The ID and status of the last failed command routed by the command router.
   overriding procedure Last_Failed_Command (Self : in out Instance; Arg : in Command_Id_Status.T);
   -- The last value sent with the Noop_Arg command. This data product can be useful for testing purposes.
   overriding procedure Noop_Arg_Last_Value (Self : in out Instance; Arg : in Command_Router_Arg.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Command_Router.Implementation.Tester;
