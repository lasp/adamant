--------------------------------------------------------------------------------
-- Command_Protector Component Tester Body
--------------------------------------------------------------------------------

package body Component.Command_Protector.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Rejected_Protected_Command_History.Init (Depth => 100);
      Self.Accepted_Protected_Command_History.Init (Depth => 100);
      Self.Armed_History.Init (Depth => 100);
      Self.Unarmed_History.Init (Depth => 100);
      Self.Unarmed_Timeout_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Armed_State_History.Init (Depth => 100);
      Self.Armed_State_Timeout_History.Init (Depth => 100);
      Self.Protected_Command_Reject_Count_History.Init (Depth => 100);
      Self.Protected_Command_Forward_Count_History.Init (Depth => 100);
      -- Packet histories:
      Self.Error_Packet_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Rejected_Protected_Command_History.Destroy;
      Self.Accepted_Protected_Command_History.Destroy;
      Self.Armed_History.Destroy;
      Self.Unarmed_History.Destroy;
      Self.Unarmed_Timeout_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Armed_State_History.Destroy;
      Self.Armed_State_Timeout_History.Destroy;
      Self.Protected_Command_Reject_Count_History.Destroy;
      Self.Protected_Command_Forward_Count_History.Destroy;
      -- Packet histories:
      Self.Error_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_Command_T_To_Forward_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_To_Forward_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Commands that are received on the Command_T_To_Forward_Recv_Sync connector are forwarded out this connector.
   overriding procedure Command_T_Recv_Sync (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_T_Recv_Sync_History.Push (Arg);
   end Command_T_Recv_Sync;

   -- This connector is used to register and respond to the component's specific commands.
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

   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- The packet send connector, used for sending error packets filled with protected commands that are rejected.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      -- Return the system time:
      To_Return : constant Sys_Time.T := Self.System_Time;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- A protected command was rejected (dropped) because the component was unarmed.
   overriding procedure Rejected_Protected_Command (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Rejected_Protected_Command_History.Push (Arg);
   end Rejected_Protected_Command;

   -- A protected command was accepted because the component was armed.
   overriding procedure Accepted_Protected_Command (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Accepted_Protected_Command_History.Push (Arg);
   end Accepted_Protected_Command;

   -- The component received the arm command and is now armed.
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
   --    Data products for the Command Protector.
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

   -- The number of protected commands rejected because the component was unarmed.
   overriding procedure Protected_Command_Reject_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Protected_Command_Reject_Count_History.Push (Arg);
   end Protected_Command_Reject_Count;

   -- The number of protected commands forwarded because the component was armed.
   overriding procedure Protected_Command_Forward_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Protected_Command_Forward_Count_History.Push (Arg);
   end Protected_Command_Forward_Count;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Command Protector component.
   -- This packet contains a protected command that was dropped due to the component not being `armed'.
   overriding procedure Error_Packet (Self : in out Instance; Arg : in Command.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Error_Packet_History.Push (Arg);
   end Error_Packet;

end Component.Command_Protector.Implementation.Tester;
