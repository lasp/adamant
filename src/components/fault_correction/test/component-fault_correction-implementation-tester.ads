--------------------------------------------------------------------------------
-- Fault_Correction Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Fault_Correction_Reciprocal;
with Sys_Time;
with Printable_History;
with Command_Response.Representation;
with Command.Representation;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Data_Product;
with Packed_U16.Representation;
with Packed_Fault_Id.Representation;
with Packed_U32.Representation;
with Event;
with Fault_Static.Representation;
with Command_Header.Representation;
with Invalid_Command_Info.Representation;
with Fault_Header.Representation;

-- The Fault Correction component receives faults asynchronously. When it processes a fault, it determines the correct command response to send and sends it.
package Component.Fault_Correction.Implementation.Tester is

   use Component.Fault_Correction_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Command_T_Recv_Sync_History_Package is new Printable_History (Command.T, Command.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Fault_Received_History_Package is new Printable_History (Fault_Static.T, Fault_Static.Representation.Image);
   package Fault_Response_Sent_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Fault_Response_Cleared_History_Package is new Printable_History (Packed_Fault_Id.T, Packed_Fault_Id.Representation.Image);
   package Fault_Response_Disabled_History_Package is new Printable_History (Packed_Fault_Id.T, Packed_Fault_Id.Representation.Image);
   package Fault_Response_Enabled_History_Package is new Printable_History (Packed_Fault_Id.T, Packed_Fault_Id.Representation.Image);
   package All_Fault_Responses_Cleared_History_Package is new Printable_History (Natural, Natural'Image);
   package Unrecognized_Fault_Id_History_Package is new Printable_History (Packed_Fault_Id.T, Packed_Fault_Id.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Fault_Dropped_History_Package is new Printable_History (Fault_Header.T, Fault_Header.Representation.Image);
   package Data_Products_Reset_History_Package is new Printable_History (Natural, Natural'Image);

   -- Data product history packages:
   package Fault_Counter_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Last_Fault_Id_Received_History_Package is new Printable_History (Packed_Fault_Id.T, Packed_Fault_Id.Representation.Image);
   package Time_Of_Last_Fault_Received_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);
   package Fault_Response_Statuses_History_Package is new Printable_History (Packed_U32.T, Packed_U32.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Fault_Correction_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Fault_Correction.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Command_T_Recv_Sync_History : Command_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Fault_Received_History : Fault_Received_History_Package.Instance;
      Fault_Response_Sent_History : Fault_Response_Sent_History_Package.Instance;
      Fault_Response_Cleared_History : Fault_Response_Cleared_History_Package.Instance;
      Fault_Response_Disabled_History : Fault_Response_Disabled_History_Package.Instance;
      Fault_Response_Enabled_History : Fault_Response_Enabled_History_Package.Instance;
      All_Fault_Responses_Cleared_History : All_Fault_Responses_Cleared_History_Package.Instance;
      Unrecognized_Fault_Id_History : Unrecognized_Fault_Id_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Command_Dropped_History : Command_Dropped_History_Package.Instance;
      Fault_Dropped_History : Fault_Dropped_History_Package.Instance;
      Data_Products_Reset_History : Data_Products_Reset_History_Package.Instance;
      -- Data product histories:
      Fault_Counter_History : Fault_Counter_History_Package.Instance;
      Last_Fault_Id_Received_History : Last_Fault_Id_Received_History_Package.Instance;
      Time_Of_Last_Fault_Received_History : Time_Of_Last_Fault_Received_History_Package.Instance;
      Fault_Response_Statuses_History : Fault_Response_Statuses_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Fault_T_Send_Dropped : Boolean := False;
      Fault_T_Send_Dropped_Count : Natural := 0;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The command send connector, for sending correction commands for faults.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Fault_T_Send message is dropped due to a full queue.
   overriding procedure Fault_T_Send_Dropped (Self : in out Instance; Arg : in Fault.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Fault Correction component.
   -- A fault was received.
   overriding procedure Fault_Received (Self : in out Instance; Arg : in Fault_Static.T);
   -- A fault response was sent with the included command header.
   overriding procedure Fault_Response_Sent (Self : in out Instance; Arg : in Command_Header.T);
   -- A fault response was cleared.
   overriding procedure Fault_Response_Cleared (Self : in out Instance; Arg : in Packed_Fault_Id.T);
   -- A fault response has been disabled
   overriding procedure Fault_Response_Disabled (Self : in out Instance; Arg : in Packed_Fault_Id.T);
   -- A fault response has been enabled.
   overriding procedure Fault_Response_Enabled (Self : in out Instance; Arg : in Packed_Fault_Id.T);
   -- Any latched faults have been unlatched by command.
   overriding procedure All_Fault_Responses_Cleared (Self : in out Instance);
   -- A fault response entry with the included fault ID was not found in the table.
   overriding procedure Unrecognized_Fault_Id (Self : in out Instance; Arg : in Packed_Fault_Id.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A fault was dropped due to a full queue.
   overriding procedure Fault_Dropped (Self : in out Instance; Arg : in Fault_Header.T);
   -- The component's data products have been reset to initialization values.
   overriding procedure Data_Products_Reset (Self : in out Instance);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Fault Correction component.
   -- The number of faults received by the component.
   overriding procedure Fault_Counter (Self : in out Instance; Arg : in Packed_U16.T);
   -- The ID of the last fault received.
   overriding procedure Last_Fault_Id_Received (Self : in out Instance; Arg : in Packed_Fault_Id.T);
   -- The system time of the last fault received.
   overriding procedure Time_Of_Last_Fault_Received (Self : in out Instance; Arg : in Sys_Time.T);
   -- 2-bits of status for each fault response that this component is managing. Note that Packed_U32.T is just a placeholder type for this data product. The actual type of this data product will be autocoded and at assembly model ingest time.
   overriding procedure Fault_Response_Statuses (Self : in out Instance; Arg : in Packed_U32.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Fault_Correction.Implementation.Tester;
