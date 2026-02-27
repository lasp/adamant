--------------------------------------------------------------------------------
-- Memory_Manager Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Memory_Manager.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Memory_Dump_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Memory_Unavailable_History.Init (Depth => 100);
      Self.Unexpected_Memory_Id_History.Init (Depth => 100);
      Self.Memory_Already_Released_History.Init (Depth => 100);
      Self.Dumping_Memory_History.Init (Depth => 100);
      Self.Invalid_Memory_Region_History.Init (Depth => 100);
      Self.Crcing_Memory_History.Init (Depth => 100);
      Self.Memory_Crc_History.Init (Depth => 100);
      Self.Writing_Memory_History.Init (Depth => 100);
      Self.Memory_Written_History.Init (Depth => 100);
      Self.Memory_Force_Released_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Dropped_Command_History.Init (Depth => 100);
      -- Data product histories:
      Self.Crc_Report_History.Init (Depth => 100);
      Self.Memory_Region_Status_History.Init (Depth => 100);
      Self.Memory_Location_History.Init (Depth => 100);
      -- Packet histories:
      Self.Memory_Region_Packet_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Memory_Dump_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Memory_Unavailable_History.Destroy;
      Self.Unexpected_Memory_Id_History.Destroy;
      Self.Memory_Already_Released_History.Destroy;
      Self.Dumping_Memory_History.Destroy;
      Self.Invalid_Memory_Region_History.Destroy;
      Self.Crcing_Memory_History.Destroy;
      Self.Memory_Crc_History.Destroy;
      Self.Writing_Memory_History.Destroy;
      Self.Memory_Written_History.Destroy;
      Self.Memory_Force_Released_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Dropped_Command_History.Destroy;
      -- Data product histories:
      Self.Crc_Report_History.Destroy;
      Self.Memory_Region_Status_History.Destroy;
      Self.Memory_Location_History.Destroy;
      -- Packet histories:
      Self.Memory_Region_Packet_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Memory_Dump_Send (To_Component => Self'Unchecked_Access, Hook => Self.Memory_Dump_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Memory_Region_Request_T_Get (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Memory_Region_Request_T_Return_Access);
      Self.Attach_Ided_Memory_Region_T_Release_Reciprocal (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Ided_Memory_Region_T_Release_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- The memory dump connector.
   overriding procedure Memory_Dump_Recv_Sync (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Dump_Recv_Sync_History.Push (Arg);
   end Memory_Dump_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T) is
      Ignore : Command.T renames Arg;
   begin
      if not Self.Expect_Command_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Command_T_Send was called!");
      else
         Self.Command_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Command_T_Send_Dropped := False;
      end if;
   end Command_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The memory region was requested, but the memory is currently in use.
   overriding procedure Memory_Unavailable (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Unavailable_History.Push (Arg);
   end Memory_Unavailable;

   -- Cannot release a memory region with an unexpected ID.
   overriding procedure Unexpected_Memory_Id (Self : in out Instance; Arg : in Ided_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unexpected_Memory_Id_History.Push (Arg);
   end Unexpected_Memory_Id;

   -- Cannot release a memory region when the memory region is currently available (i.e. already released).
   overriding procedure Memory_Already_Released (Self : in out Instance; Arg : in Ided_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Already_Released_History.Push (Arg);
   end Memory_Already_Released;

   -- The component is currently dumping the virtual memory location for the following region.
   overriding procedure Dumping_Memory (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumping_Memory_History.Push (Arg);
   end Dumping_Memory;

   -- The operation could not be performed on the requested virtual memory region, since the address and length fall outside the memory region managed by the component.
   overriding procedure Invalid_Memory_Region (Self : in out Instance; Arg : in Invalid_Virtual_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Memory_Region_History.Push (Arg);
   end Invalid_Memory_Region;

   -- The component is currently CRCing the virtual memory location for the following region.
   overriding procedure Crcing_Memory (Self : in out Instance; Arg : in Virtual_Memory_Region_Positive.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Crcing_Memory_History.Push (Arg);
   end Crcing_Memory;

   -- The virtual memory region CRC has been calculated.
   overriding procedure Memory_Crc (Self : in out Instance; Arg : in Virtual_Memory_Region_Crc.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Crc_History.Push (Arg);
   end Memory_Crc;

   -- The component is currently writing to the virtual memory location for the following region.
   overriding procedure Writing_Memory (Self : in out Instance; Arg : in Virtual_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Writing_Memory_History.Push (Arg);
   end Writing_Memory;

   -- The virtual memory region has been written.
   overriding procedure Memory_Written (Self : in out Instance; Arg : in Virtual_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Written_History.Push (Arg);
   end Memory_Written;

   -- The virtual memory region was force released.
   overriding procedure Memory_Force_Released (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Force_Released_History.Push (Arg);
   end Memory_Force_Released;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command was dropped because the component queue overflowed.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Command_History.Push (Arg);
   end Dropped_Command;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Memory Manager component.
   -- The last computed CRC by the memory manager component.
   overriding procedure Crc_Report (Self : in out Instance; Arg : in Virtual_Memory_Region_Crc.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Crc_Report_History.Push (Arg);
   end Crc_Report;

   -- Status relating whether the memory region is currently allocated or not.
   overriding procedure Memory_Region_Status (Self : in out Instance; Arg : in Memory_Manager_State.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Status_History.Push (Arg);
   end Memory_Region_Status;

   -- Reports the physical start address and length of the virtual memory region allocated to this component.
   overriding procedure Memory_Location (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Location_History.Push (Arg);
   end Memory_Location;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Memory Manager.
   -- This packet contains memory region data.
   overriding procedure Memory_Region_Packet (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Packet_History.Push (Arg);
   end Memory_Region_Packet;

   -----------------------------------------------
   -- Special primitives for activating component
   -- queues:
   -----------------------------------------------
   -- Force the component to drain the entire queue
   not overriding function Dispatch_All (Self : in out Instance) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching all items off queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_All;
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_All;

   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural is
      Num_Dispatched : Natural;
   begin
      Self.Log ("    Dispatching up to " & String_Util.Trim_Both (Positive'Image (N)) & " items from queue.");
      Num_Dispatched := Self.Component_Instance.Dispatch_N (N);
      Self.Log ("    Dispatched " & String_Util.Trim_Both (Natural'Image (Num_Dispatched)) & " items from queue.");
      return Num_Dispatched;
   end Dispatch_N;

end Component.Memory_Manager.Implementation.Tester;
