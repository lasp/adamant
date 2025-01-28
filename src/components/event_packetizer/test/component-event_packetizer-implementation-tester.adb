--------------------------------------------------------------------------------
-- Event_Packetizer Component Tester Body
--------------------------------------------------------------------------------

package body Component.Event_Packetizer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Packet_T_Recv_Sync_History.Init (Depth => 20);
      Self.Sys_Time_T_Return_History.Init (Depth => 400);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 400);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 20);
      -- Data product histories:
      Self.Events_Dropped_Count_History.Init (Depth => 20);
      Self.Bytes_Available_History.Init (Depth => 400);
      -- Packet histories:
      Self.Events_Packet_History.Init (Depth => 20);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      -- Data product histories:
      Self.Events_Dropped_Count_History.Destroy;
      Self.Bytes_Available_History.Destroy;
      -- Packet histories:
      Self.Events_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Packet_T_Send (Self'Unchecked_Access, Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (Self'Unchecked_Access, Self.Sys_Time_T_Return_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (Self'Unchecked_Access, Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (Self'Unchecked_Access, Self.Command_Response_T_Recv_Sync_Access);
      Self.Attach_Tick_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_Event_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Event_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (Self.Component_Instance'Unchecked_Access, Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Send a packet of events.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

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

   -- Data products are sent out of this connector.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler:
      Self.Dispatch_Data_Product (Arg);
   end Data_Product_T_Recv_Sync;

   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Event Packetizer component.
   -- The number of events dropped by the component.
   overriding procedure Events_Dropped_Count (Self : in out Instance; Arg : Packed_U32.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Events_Dropped_Count_History.Push (Arg);
   end Events_Dropped_Count;

   -- The current number of bytes available for event storage within the component.
   overriding procedure Bytes_Available (Self : in out Instance; Arg : Packed_Natural.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Bytes_Available_History.Push (Arg);
   end Bytes_Available;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the event packetizer
   -- This packet contains events as subpackets.
   overriding procedure Events_Packet (Self : in out Instance; Arg : Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Events_Packet_History.Push (Arg);
   end Events_Packet;

end Component.Event_Packetizer.Implementation.Tester;
