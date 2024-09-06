--------------------------------------------------------------------------------
-- Memory_Dumper Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Memory_Dumper.Implementation.Tester is

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
      Self.Invalid_Memory_Region_History.Init (Depth => 100);
      Self.Dumping_Memory_History.Init (Depth => 100);
      Self.Crcing_Memory_History.Init (Depth => 100);
      Self.Memory_Crc_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Crc_Report_History.Init (Depth => 100);
      -- Packet histories:
      Self.Memory_Dump_Packet_History.Init (Depth => 100);
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
      Self.Invalid_Memory_Region_History.Destroy;
      Self.Dumping_Memory_History.Destroy;
      Self.Crcing_Memory_History.Destroy;
      Self.Memory_Crc_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Crc_Report_History.Destroy;
      -- Packet histories:
      Self.Memory_Dump_Packet_History.Destroy;

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

   -- The data product invoker connector
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
   -- A command was sent to access a memory region with an invalid address and/or length.
   overriding procedure Invalid_Memory_Region (Self : in out Instance; Arg : in Memory_Region_Positive.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Memory_Region_History.Push (Arg);
   end Invalid_Memory_Region;

   -- The component is currently dumping the memory location for the following region.
   overriding procedure Dumping_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumping_Memory_History.Push (Arg);
   end Dumping_Memory;

   -- The component is currently CRCing the memory location for the following region.
   overriding procedure Crcing_Memory (Self : in out Instance; Arg : in Memory_Region_Positive.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Crcing_Memory_History.Push (Arg);
   end Crcing_Memory;

   -- The memory region CRC has been calculated.
   overriding procedure Memory_Crc (Self : in out Instance; Arg : in Memory_Region_Crc.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Crc_History.Push (Arg);
   end Memory_Crc;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the memory dumper component.
   -- The last computed CRC by the memory dumper component.
   overriding procedure Crc_Report (Self : in out Instance; Arg : in Memory_Region_Crc.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Crc_Report_History.Push (Arg);
   end Crc_Report;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the memory dumper.
   -- This packet contains memory.
   overriding procedure Memory_Dump_Packet (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Dump_Packet_History.Push (Arg);
   end Memory_Dump_Packet;

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

end Component.Memory_Dumper.Implementation.Tester;
