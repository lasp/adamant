--------------------------------------------------------------------------------
-- Register_Stuffer Component Tester Body
--------------------------------------------------------------------------------

package body Component.Register_Stuffer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      -- Event histories:
      Self.Invalid_Register_Address_History.Init (Depth => 100);
      Self.Register_Written_History.Init (Depth => 100);
      Self.Register_Read_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Rejected_Protected_Register_Write_History.Init (Depth => 100);
      Self.Armed_History.Init (Depth => 100);
      Self.Unarmed_History.Init (Depth => 100);
      Self.Unarmed_Timeout_History.Init (Depth => 100);
      Self.Registers_Dumped_History.Init (Depth => 100);
      Self.Address_Range_Overflow_History.Init (Depth => 100);
      -- Data product histories:
      Self.Last_Register_Written_History.Init (Depth => 100);
      Self.Last_Register_Read_History.Init (Depth => 100);
      Self.Armed_State_History.Init (Depth => 100);
      Self.Armed_State_Timeout_History.Init (Depth => 100);
      -- Packet histories:
      Self.Register_Packet_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      -- Event histories:
      Self.Invalid_Register_Address_History.Destroy;
      Self.Register_Written_History.Destroy;
      Self.Register_Read_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Rejected_Protected_Register_Write_History.Destroy;
      Self.Armed_History.Destroy;
      Self.Unarmed_History.Destroy;
      Self.Unarmed_Timeout_History.Destroy;
      Self.Registers_Dumped_History.Destroy;
      Self.Address_Range_Overflow_History.Destroy;
      -- Data product histories:
      Self.Last_Register_Written_History.Destroy;
      Self.Last_Register_Read_History.Destroy;
      Self.Armed_State_History.Destroy;
      Self.Armed_State_Timeout_History.Destroy;
      -- Packet histories:
      Self.Register_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send the command response back to the command router.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- The event send connector.
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

   -- Packets are sent out of this connector
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Register Stuffer component.
   -- The register address provided does not start on a 32-bit boundary.
   overriding procedure Invalid_Register_Address (Self : in out Instance; Arg : in Packed_Address.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Register_Address_History.Push (Arg);
   end Invalid_Register_Address;

   -- The specified register was written to the commanded value.
   overriding procedure Register_Written (Self : in out Instance; Arg : in Register_Value.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Register_Written_History.Push (Arg);
   end Register_Written;

   -- The specified register was read from.
   overriding procedure Register_Read (Self : in out Instance; Arg : in Register_Value.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Register_Read_History.Push (Arg);
   end Register_Read;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- The specified register could not be written because the component was not armed
   -- first.
   overriding procedure Rejected_Protected_Register_Write (Self : in out Instance; Arg : in Register_Value.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Rejected_Protected_Register_Write_History.Push (Arg);
   end Rejected_Protected_Register_Write;

   -- The component received the arm command an is now armed.
   overriding procedure Armed (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Armed_History.Push (Arg);
   end Armed;

   -- The component received a command and is now unarmed.
   overriding procedure Unarmed (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unarmed_History.Push (Arg);
   end Unarmed;

   -- The component armed state timed out and is now unarmed.
   overriding procedure Unarmed_Timeout (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Unarmed_Timeout_History.Push (Arg);
   end Unarmed_Timeout;

   -- The specified registers were dumped.
   overriding procedure Registers_Dumped (Self : in out Instance; Arg : in Register_Dump_Packet_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Registers_Dumped_History.Push (Arg);
   end Registers_Dumped;

   -- The specified registers were dumped.
   overriding procedure Address_Range_Overflow (Self : in out Instance; Arg : in Register_Dump_Packet_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Address_Range_Overflow_History.Push (Arg);
   end Address_Range_Overflow;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Register Stuffer component.
   -- The address and value of the last written register.
   overriding procedure Last_Register_Written (Self : in out Instance; Arg : in Register_Value.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Register_Written_History.Push (Arg);
   end Last_Register_Written;

   -- The address and value of the last read register.
   overriding procedure Last_Register_Read (Self : in out Instance; Arg : in Register_Value.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Last_Register_Read_History.Push (Arg);
   end Last_Register_Read;

   -- The current armed/unarmed state of the component.
   overriding procedure Armed_State (Self : in out Instance; Arg : in Packed_Arm_State.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Armed_State_History.Push (Arg);
   end Armed_State;

   -- The time remaining (in ticks) until the armed state expires.
   overriding procedure Armed_State_Timeout (Self : in out Instance; Arg : in Packed_Arm_Timeout.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Armed_State_Timeout_History.Push (Arg);
   end Armed_State_Timeout;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the register stuffer.
   -- This packet contains dumped register values.
   overriding procedure Register_Packet (Self : in out Instance; Arg : in Register_Dump_Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Register_Packet_History.Push (Arg);
   end Register_Packet;

end Component.Register_Stuffer.Implementation.Tester;
