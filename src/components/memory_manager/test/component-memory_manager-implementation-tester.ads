--------------------------------------------------------------------------------
-- Memory_Manager Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Memory_Manager_Reciprocal;
with Sys_Time;
with Printable_History;
with Command_Response.Representation;
with Memory_Packetizer_Types;
with Data_Product.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Data_Product;
with Virtual_Memory_Region_Crc.Representation;
with Memory_Manager_State.Representation;
with Memory_Region.Representation;
with Event;
with Ided_Memory_Region.Representation;
with Virtual_Memory_Region_Positive.Representation;
with Invalid_Virtual_Memory_Region.Representation;
with Virtual_Memory_Region.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;
with Packet.Representation;
with History;

-- The component manages access to a single memory location through a single pointer. When requested, the component loans out access to the pointer if it is available. The length of the pointer will always be the entire length of the memory region. The component will reject any requests to access the pointer again until the pointer is returned from the requester. Request/release memory transactions are each provided a unique ID. To release the memory, the same ID must be provided that was issued upon request. This mechanism reduces the risk of an inadvertent call to release from causing an unintended release of the memory. The component includes a data product relating whether the memory is currently allocated or not. The component responds to commands to CRC, dump, write, and force-release the memory region. Note that this component is active only to provide a separate thread of execution on which to execute the CRC command and the memory write command, each of which could take a long time to execute.
package Component.Memory_Manager.Implementation.Tester is

   use Component.Memory_Manager_Reciprocal;
   -- Invoker connector history packages:
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Memory_Dump_Recv_Sync_History_Package is new History (Memory_Packetizer_Types.Memory_Dump);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Memory_Unavailable_History_Package is new Printable_History (Natural, Natural'Image);
   package Unexpected_Memory_Id_History_Package is new Printable_History (Ided_Memory_Region.T, Ided_Memory_Region.Representation.Image);
   package Memory_Already_Released_History_Package is new Printable_History (Ided_Memory_Region.T, Ided_Memory_Region.Representation.Image);
   package Dumping_Memory_History_Package is new Printable_History (Virtual_Memory_Region_Positive.T, Virtual_Memory_Region_Positive.Representation.Image);
   package Invalid_Memory_Region_History_Package is new Printable_History (Invalid_Virtual_Memory_Region.T, Invalid_Virtual_Memory_Region.Representation.Image);
   package Crcing_Memory_History_Package is new Printable_History (Virtual_Memory_Region_Positive.T, Virtual_Memory_Region_Positive.Representation.Image);
   package Memory_Crc_History_Package is new Printable_History (Virtual_Memory_Region_Crc.T, Virtual_Memory_Region_Crc.Representation.Image);
   package Writing_Memory_History_Package is new Printable_History (Virtual_Memory_Region.T, Virtual_Memory_Region.Representation.Image);
   package Memory_Written_History_Package is new Printable_History (Virtual_Memory_Region.T, Virtual_Memory_Region.Representation.Image);
   package Memory_Force_Released_History_Package is new Printable_History (Natural, Natural'Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Dropped_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);

   -- Data product history packages:
   package Crc_Report_History_Package is new Printable_History (Virtual_Memory_Region_Crc.T, Virtual_Memory_Region_Crc.Representation.Image);
   package Memory_Region_Status_History_Package is new Printable_History (Memory_Manager_State.T, Memory_Manager_State.Representation.Image);
   package Memory_Location_History_Package is new Printable_History (Memory_Region.T, Memory_Region.Representation.Image);

   -- Packet history packages:
   package Memory_Region_Packet_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Memory_Manager_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Memory_Manager.Implementation.Instance;
      -- Connector histories:
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Memory_Dump_Recv_Sync_History : Memory_Dump_Recv_Sync_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Memory_Unavailable_History : Memory_Unavailable_History_Package.Instance;
      Unexpected_Memory_Id_History : Unexpected_Memory_Id_History_Package.Instance;
      Memory_Already_Released_History : Memory_Already_Released_History_Package.Instance;
      Dumping_Memory_History : Dumping_Memory_History_Package.Instance;
      Invalid_Memory_Region_History : Invalid_Memory_Region_History_Package.Instance;
      Crcing_Memory_History : Crcing_Memory_History_Package.Instance;
      Memory_Crc_History : Memory_Crc_History_Package.Instance;
      Writing_Memory_History : Writing_Memory_History_Package.Instance;
      Memory_Written_History : Memory_Written_History_Package.Instance;
      Memory_Force_Released_History : Memory_Force_Released_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Dropped_Command_History : Dropped_Command_History_Package.Instance;
      -- Data product histories:
      Crc_Report_History : Crc_Report_History_Package.Instance;
      Memory_Region_Status_History : Memory_Region_Status_History_Package.Instance;
      Memory_Location_History : Memory_Location_History_Package.Instance;
      -- Packet histories:
      Memory_Region_Packet_History : Memory_Region_Packet_History_Package.Instance;
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

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The memory region was requested, but the memory is currently in use.
   overriding procedure Memory_Unavailable (Self : in out Instance);
   -- Cannot release a memory region with an unexpected ID.
   overriding procedure Unexpected_Memory_Id (Self : in out Instance; Arg : in Ided_Memory_Region.T);
   -- Cannot release a memory region when the memory region is currently available (i.e. already released).
   overriding procedure Memory_Already_Released (Self : in out Instance; Arg : in Ided_Memory_Region.T);
   -- The component is currently dumping the virtual memory location for the following region.
   overriding procedure Dumping_Memory (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T);
   -- The operation could not be performed on the requested virtual memory region, since the address and length fall outside the memory region managed by the component.
   overriding procedure Invalid_Memory_Region (Self : in out Instance; Arg : in Invalid_Virtual_Memory_Region.T);
   -- The component is currently CRCing the virtual memory location for the following region.
   overriding procedure Crcing_Memory (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T);
   -- The virtual memory region CRC has been calculated.
   overriding procedure Memory_Crc (Self : in out Instance; Arg : in Virtual_Memory_Region_Crc.T);
   -- The component is currently writing to the virtual memory location for the following region.
   overriding procedure Writing_Memory (Self : in out Instance; Arg : in Virtual_Memory_Region.T);
   -- The virtual memory region has been written.
   overriding procedure Memory_Written (Self : in out Instance; Arg : in Virtual_Memory_Region.T);
   -- The virtual memory region was force released.
   overriding procedure Memory_Force_Released (Self : in out Instance);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped because the component queue overflowed.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Memory Manager component.
   -- The last computed CRC by the memory manager component.
   overriding procedure Crc_Report (Self : in out Instance; Arg : in Virtual_Memory_Region_Crc.T);
   -- Status relating whether the memory region is currently allocated or not.
   overriding procedure Memory_Region_Status (Self : in out Instance; Arg : in Memory_Manager_State.T);
   -- Reports the physical start address and length of the virtual memory region allocated to this component.
   overriding procedure Memory_Location (Self : in out Instance; Arg : in Memory_Region.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Memory Manager.
   -- This packet contains memory region data.
   overriding procedure Memory_Region_Packet (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Memory_Manager.Implementation.Tester;
