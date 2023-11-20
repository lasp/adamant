--------------------------------------------------------------------------------
-- Logger Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Logger_Reciprocal;
with Sys_Time;
with History;
with Printable_History;
with Memory_Packetizer_Types;
with Command_Response.Representation;
with Event.Representation;
with Data_Product.Representation;
with Sys_Time.Representation;
with Data_Product;
with Logger_Status.Representation;
with Packet.Representation;
with Event;
with Logger_Error.Representation;
with Circular_Buffer_Meta.Representation;
with Logger_Info.Representation;
with Memory_Region.Representation;
with Invalid_Command_Info.Representation;

-- The Logger component receives data of generic statically-sized, or variable-sized type. This data is synchronously added to an internal circular buffer. By default, the logging of this data can be disabled at component start and can be enabled via command. Various commands also exist to dump the internal circular buffer. The circular buffer of the logger can either be declared on the heap or in a static memory location, via the component's Init subprogram.
generic
package Component.Logger.Implementation.Tester is

   package Logger_Package is new Component.Logger_Reciprocal (T, Serialized_Length);
   -- Invoker connector history packages:
   package Memory_Dump_Recv_Sync_History_Package is new History (Memory_Packetizer_Types.Memory_Dump);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Log_Attempt_Failed_History_Package is new Printable_History (Logger_Error.T, Logger_Error.Representation.Image);
   package Log_Disabled_History_Package is new Printable_History (Circular_Buffer_Meta.T, Circular_Buffer_Meta.Representation.Image);
   package Log_Enabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Log_Info_Update_History_Package is new Printable_History (Logger_Info.T, Logger_Info.Representation.Image);
   package Dumping_Log_Memory_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Mode_History_Package is new Printable_History (Logger_Status.T, Logger_Status.Representation.Image);

   -- Packet history packages:
   package Log_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Logger_Package.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Logger.Implementation.Instance;
      -- Connector histories:
      Memory_Dump_Recv_Sync_History : Memory_Dump_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Log_Attempt_Failed_History : Log_Attempt_Failed_History_Package.Instance;
      Log_Disabled_History : Log_Disabled_History_Package.Instance;
      Log_Enabled_History : Log_Enabled_History_Package.Instance;
      Log_Info_Update_History : Log_Info_Update_History_Package.Instance;
      Dumping_Log_Memory_History : Dumping_Log_Memory_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Mode_History : Mode_History_Package.Instance;
      -- Packet histories:
      Log_Packet_History : Log_Packet_History_Package.Instance;
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The memory dump connector.
   overriding procedure Memory_Dump_Recv_Sync (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump);
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A log attempt failed with the following status.
   overriding procedure Log_Attempt_Failed (Self : in out Instance; Arg : Logger_Error.T);
   -- The log was disabled. No more data will be stored.
   overriding procedure Log_Disabled (Self : in out Instance; Arg : Circular_Buffer_Meta.T);
   -- The log was enabled. Data will now be stored.
   overriding procedure Log_Enabled (Self : in out Instance);
   -- The current meta data of the log was requested.
   overriding procedure Log_Info_Update (Self : in out Instance; Arg : Logger_Info.T);
   -- Currently dumping log memory from the following location.
   overriding procedure Dumping_Log_Memory (Self : in out Instance; Arg : Memory_Region.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Logger component.
   -- The current enabled/disabled mode of the component.
   overriding procedure Mode (Self : in out Instance; Arg : Logger_Status.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the logger.
   -- This packet contains log data.
   overriding procedure Log_Packet (Self : in out Instance; Arg : Packet.T);

end Component.Logger.Implementation.Tester;
