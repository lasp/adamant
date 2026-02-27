--------------------------------------------------------------------------------
-- Parameter_Store Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Parameter_Store_Reciprocal;
with Sys_Time;
with Printable_History;
with Command_Response.Representation;
with Parameters_Memory_Region_Release.Representation;
with Packet.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Invalid_Parameters_Memory_Region_Length.Representation;
with Invalid_Parameters_Memory_Region_Crc.Representation;
with Memory_Region.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;
with Parameters_Memory_Region.Representation;

-- The Parameters Component is responsible for storing and managing access to a memory region holding a parameter table. The managed memory region is usually located in nonvolatile storage and can serve as the backup or the default parameter values to use at startup for the system.
package Component.Parameter_Store.Implementation.Tester is

   use Component.Parameter_Store_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Parameters_Memory_Region_Release_T_Recv_Sync_History_Package is new Printable_History (Parameters_Memory_Region_Release.T, Parameters_Memory_Region_Release.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Memory_Region_Length_Mismatch_History_Package is new Printable_History (Invalid_Parameters_Memory_Region_Length.T, Invalid_Parameters_Memory_Region_Length.Representation.Image);
   package Memory_Region_Crc_Invalid_History_Package is new Printable_History (Invalid_Parameters_Memory_Region_Crc.T, Invalid_Parameters_Memory_Region_Crc.Representation.Image);
   package Dumped_Parameters_History_Package is new Printable_History (Natural, Natural'Image);
   package Parameter_Table_Updated_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Parameter_Table_Fetched_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Command_Dropped_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);
   package Memory_Region_Dropped_History_Package is new Printable_History (Parameters_Memory_Region.T, Parameters_Memory_Region.Representation.Image);
   package Table_Validation_Not_Supported_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);

   -- Packet history packages:
   package Stored_Parameters_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Parameter_Store_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Parameter_Store.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Parameters_Memory_Region_Release_T_Recv_Sync_History : Parameters_Memory_Region_Release_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Memory_Region_Length_Mismatch_History : Memory_Region_Length_Mismatch_History_Package.Instance;
      Memory_Region_Crc_Invalid_History : Memory_Region_Crc_Invalid_History_Package.Instance;
      Dumped_Parameters_History : Dumped_Parameters_History_Package.Instance;
      Parameter_Table_Updated_History : Parameter_Table_Updated_History_Package.Instance;
      Parameter_Table_Fetched_History : Parameter_Table_Fetched_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Command_Dropped_History : Command_Dropped_History_Package.Instance;
      Memory_Region_Dropped_History : Memory_Region_Dropped_History_Package.Instance;
      Table_Validation_Not_Supported_History : Table_Validation_Not_Supported_History_Package.Instance;
      -- Packet histories:
      Stored_Parameters_History : Stored_Parameters_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      Expect_Parameters_Memory_Region_T_Send_Dropped : Boolean := False;
      Parameters_Memory_Region_T_Send_Dropped_Count : Natural := 0;
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
   -- After a memory region is received on the Memory_Region_T_Recv_Async connector and then processed, it is released via a call to this connector. A status is also returned, so the downstream component can determine if the parameter table update was successful or not.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T);
   -- The parameter packet connector. A copy of the managed parameter table is dumped via this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -- This procedure is called when a Parameters_Memory_Region_T_Send message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Send_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Parameter Store component.
   -- A memory region was received with an invalid length. The length of the region must be the same size as the parameter table.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Length.T);
   -- A memory region parameter table was received with an invalid CRC. The computed CRC does not match the CRC found in the header.
   overriding procedure Memory_Region_Crc_Invalid (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Crc.T);
   -- Produced a packet with the contents of the parameter store.
   overriding procedure Dumped_Parameters (Self : in out Instance);
   -- Parameter table updated from a received memory region.
   overriding procedure Parameter_Table_Updated (Self : in out Instance; Arg : in Memory_Region.T);
   -- Starting parameter fetch into the received memory region.
   overriding procedure Parameter_Table_Fetched (Self : in out Instance; Arg : in Memory_Region.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T);
   -- A memory region was dropped due to a full queue.
   overriding procedure Memory_Region_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T);
   -- Parameter table validation is not supported.
   overriding procedure Table_Validation_Not_Supported (Self : in out Instance; Arg : in Memory_Region.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Parameter Store component.
   -- This packet contains a copy of all the parameters stored and managed by this component.
   overriding procedure Stored_Parameters (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Parameter_Store.Implementation.Tester;
