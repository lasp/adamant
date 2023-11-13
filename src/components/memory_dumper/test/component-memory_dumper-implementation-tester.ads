--------------------------------------------------------------------------------
-- Memory_Dumper Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Memory_Dumper_Reciprocal;
with Sys_Time;
with Printable_History;
with Command_Response.Representation;
with Memory_Packetizer_Types;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Memory_Region_Positive.Representation;
with Memory_Region_Crc.Representation;
with Invalid_Command_Info.Representation;
with Packet.Representation;
with Data_Product;
with History;

-- The memory dumper component is an active component that can dump memory regions or report the CRC of memory regions by command. It reports an error if an action is requested on a memory region outside of the address space that it is configured with during initialization.
package Component.Memory_Dumper.Implementation.Tester is

   use Component.Memory_Dumper_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Memory_Dump_Recv_Sync_History_Package is new History (Memory_Packetizer_Types.Memory_Dump);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Invalid_Memory_Region_History_Package is new Printable_History (Memory_Region_Positive.T, Memory_Region_Positive.Representation.Image);
   package Dumping_Memory_History_Package is new Printable_History (Memory_Region_Positive.T, Memory_Region_Positive.Representation.Image);
   package Crcing_Memory_History_Package is new Printable_History (Memory_Region_Positive.T, Memory_Region_Positive.Representation.Image);
   package Memory_Crc_History_Package is new Printable_History (Memory_Region_Crc.T, Memory_Region_Crc.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);

   -- Data product history packages:
   package Crc_Report_History_Package is new Printable_History (Memory_Region_Crc.T, Memory_Region_Crc.Representation.Image);

   -- Packet history packages:
   package Memory_Dump_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Memory_Dumper_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Memory_Dumper.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Memory_Dump_Recv_Sync_History : Memory_Dump_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Invalid_Memory_Region_History : Invalid_Memory_Region_History_Package.Instance;
      Dumping_Memory_History : Dumping_Memory_History_Package.Instance;
      Crcing_Memory_History : Crcing_Memory_History_Package.Instance;
      Memory_Crc_History : Memory_Crc_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      -- Data product histories:
      Crc_Report_History : Crc_Report_History_Package.Instance;
      -- Packet histories:
      Memory_Dump_Packet_History : Memory_Dump_Packet_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
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
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- The memory dump connector.
   overriding procedure Memory_Dump_Recv_Sync (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump);
   -- The data product invoker connector
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

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A command was sent to access a memory region with an invalid address and/or length.
   overriding procedure Invalid_Memory_Region (Self : in out Instance; Arg : in Memory_Region_Positive.T);
   -- The component is currently dumping the memory location for the following region.
   overriding procedure Dumping_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T);
   -- The component is currently CRCing the memory location for the following region.
   overriding procedure Crcing_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T);
   -- The memory region CRC has been calculated.
   overriding procedure Memory_Crc (Self : in out Instance; Arg : in Memory_Region_Crc.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the memory dumper component.
   -- The last computed CRC by the memory dumper component.
   overriding procedure Crc_Report (Self : in out Instance; Arg : in Memory_Region_Crc.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the memory dumper.
   -- This packet contains memory.
   overriding procedure Memory_Dump_Packet (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Memory_Dumper.Implementation.Tester;
