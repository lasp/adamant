--------------------------------------------------------------------------------
-- Last_Chance_Manager Component Tester Body
--------------------------------------------------------------------------------

package body Component.Last_Chance_Manager.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Last_Chance_Handler_Called_History.Init (Depth => 100);
      Self.Dumped_Last_Chance_Handler_Region_History.Init (Depth => 100);
      Self.Cleared_Last_Chance_Handler_Region_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Lch_Stack_Trace_Info_History.Init (Depth => 100);
      -- Packet histories:
      Self.Lch_Memory_Region_Dump_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Last_Chance_Handler_Called_History.Destroy;
      Self.Dumped_Last_Chance_Handler_Region_History.Destroy;
      Self.Cleared_Last_Chance_Handler_Region_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Lch_Stack_Trace_Info_History.Destroy;
      -- Packet histories:
      Self.Lch_Memory_Region_Dump_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
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

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- Send a packet of data products.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- The component detected that the LCH was called by looking at the data in nonvolatile memory. The lowest level address of the stack trace is reported.
   overriding procedure Last_Chance_Handler_Called (Self : in out Instance; Arg : in Packed_Stack_Trace_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Chance_Handler_Called_History.Push (Arg);
   end Last_Chance_Handler_Called;

   -- The component dumped the last chance handler memory region into a packet for downlink.
   overriding procedure Dumped_Last_Chance_Handler_Region (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumped_Last_Chance_Handler_Region_History.Push (Arg);
   end Dumped_Last_Chance_Handler_Region;

   -- The component cleared the last chance handler memory region by writing all zeros to it.
   overriding procedure Cleared_Last_Chance_Handler_Region (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Cleared_Last_Chance_Handler_Region_History.Push (Arg);
   end Cleared_Last_Chance_Handler_Region;

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
   --    Data products for the Last Chance Manager component.
   -- Information on the current stack trace stored in the last chance handler memory store.
   overriding procedure Lch_Stack_Trace_Info (Self : in out Instance; Arg : in Packed_Stack_Trace_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Lch_Stack_Trace_Info_History.Push (Arg);
   end Lch_Stack_Trace_Info;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    The second packet listed here is not actually produced by the Last Chance Manager component, but instead should be produced by the implementation of the Last\_Chance\_Handler. This packet definition exists to ensure that the packet gets reflected in the documentation and ground system definitions.
   -- This packet contains a dump of the LCH nonvolatile memory region where exception information is thrown.
   overriding procedure Lch_Memory_Region_Dump (Self : in out Instance; Arg : in Packed_Exception_Occurrence.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Lch_Memory_Region_Dump_History.Push (Arg);
   end Lch_Memory_Region_Dump;

end Component.Last_Chance_Manager.Implementation.Tester;
