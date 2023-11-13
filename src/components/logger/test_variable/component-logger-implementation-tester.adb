--------------------------------------------------------------------------------
-- Logger Component Tester Body
--------------------------------------------------------------------------------

package body Component.Logger.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Memory_Dump_Recv_Sync_History.Init (Depth => 20);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 20);
      Self.Event_T_Recv_Sync_History.Init (Depth => 20);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 20);
      Self.Sys_Time_T_Return_History.Init (Depth => 20);
      -- Event histories:
      Self.Log_Attempt_Failed_History.Init (Depth => 20);
      Self.Log_Disabled_History.Init (Depth => 20);
      Self.Log_Enabled_History.Init (Depth => 20);
      Self.Log_Info_Update_History.Init (Depth => 20);
      Self.Dumping_Log_Memory_History.Init (Depth => 20);
      Self.Invalid_Command_Received_History.Init (Depth => 20);
      -- Data product histories:
      Self.Mode_History.Init (Depth => 20);
      -- Packet histories:
      Self.Log_Packet_History.Init (Depth => 20);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Memory_Dump_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Log_Attempt_Failed_History.Destroy;
      Self.Log_Disabled_History.Destroy;
      Self.Log_Enabled_History.Destroy;
      Self.Log_Info_Update_History.Destroy;
      Self.Dumping_Log_Memory_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Mode_History.Destroy;
      -- Packet histories:
      Self.Log_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Memory_Dump_Send (Self'Unchecked_Access, Self.Memory_Dump_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (Self'Unchecked_Access, Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (Self'Unchecked_Access, Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (Self'Unchecked_Access, Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Attach_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- The memory dump connector.
   overriding procedure Memory_Dump_Recv_Sync (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Dump_Recv_Sync_History.Push (Arg);
   end Memory_Dump_Recv_Sync;

   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A log attempt failed with the following status.
   overriding procedure Log_Attempt_Failed (Self : in out Instance; Arg : Logger_Error.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Log_Attempt_Failed_History.Push (Arg);
   end Log_Attempt_Failed;

   -- The log was disabled. No more data will be stored.
   overriding procedure Log_Disabled (Self : in out Instance; Arg : Circular_Buffer_Meta.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Log_Disabled_History.Push (Arg);
   end Log_Disabled;

   -- The log was enabled. Data will now be stored.
   overriding procedure Log_Enabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Log_Enabled_History.Push (Arg);
   end Log_Enabled;

   -- The current meta data of the log was requested.
   overriding procedure Log_Info_Update (Self : in out Instance; Arg : Logger_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Log_Info_Update_History.Push (Arg);
   end Log_Info_Update;

   -- Currently dumping log memory from the following location.
   overriding procedure Dumping_Log_Memory (Self : in out Instance; Arg : Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumping_Log_Memory_History.Push (Arg);
   end Dumping_Log_Memory;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Logger component.
   -- The current enabled/disabled mode of the component.
   overriding procedure Mode (Self : in out Instance; Arg : Logger_Status.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Mode_History.Push (Arg);
   end Mode;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the logger.
   -- This packet contains log data.
   overriding procedure Log_Packet (Self : in out Instance; Arg : Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Log_Packet_History.Push (Arg);
   end Log_Packet;

end Component.Logger.Implementation.Tester;
