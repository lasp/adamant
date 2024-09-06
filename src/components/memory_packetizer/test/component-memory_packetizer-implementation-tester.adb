--------------------------------------------------------------------------------
-- Memory_Packetizer Component Tester Body
--------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Sys_Time.Arithmetic; use Sys_Time.Arithmetic;
with String_Util;

package body Component.Memory_Packetizer.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Max_Packet_Id_Exceeded_History.Init (Depth => 100);
      Self.Memory_Dump_Request_Dropped_History.Init (Depth => 100);
      Self.Max_Packet_Rate_Set_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Max_Packets_Per_Time_Period_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Max_Packet_Id_Exceeded_History.Destroy;
      Self.Memory_Dump_Request_Dropped_History.Destroy;
      Self.Max_Packet_Rate_Set_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Max_Packets_Per_Time_Period_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Memory_Dump_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Memory_Dump_Recv_Async_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Send a packet of data.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
   end Packet_T_Recv_Sync;

   -- This connector is used to register and respond to the component's commands.
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

   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T is
      Ignore : Instance renames Self;
      To_Return : Sys_Time.T;
      Current_Time : constant Time := Clock;
      Status : Sys_Time_Status;
      pragma Unreferenced (Status);
   begin
      Status := To_Sys_Time (Current_Time, To_Return);
      -- Push the argument onto the test history for looking at later:
      Self.Sys_Time_T_Return_History.Push (To_Return);
      return To_Return;
   end Sys_Time_T_Return;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Memory_Dump_Send message is dropped due to a full queue.
   overriding procedure Memory_Dump_Send_Dropped (Self : in out Instance; Arg : in Memory_Packetizer_Types.Memory_Dump) is
      Ignore : Memory_Packetizer_Types.Memory_Dump renames Arg;
   begin
      if not Self.Expect_Memory_Dump_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Memory_Dump_Send was called!");
      else
         Self.Memory_Dump_Send_Dropped_Count := @ + 1;
         Self.Expect_Memory_Dump_Send_Dropped := False;
      end if;
   end Memory_Dump_Send_Dropped;

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
   -- The maximum number of packet ids that the component can keep track of sequence numbers for has been exceeded. Packets of this id will be emitted with a sequence number of 0.
   overriding procedure Max_Packet_Id_Exceeded (Self : in out Instance; Arg : in Packet_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Max_Packet_Id_Exceeded_History.Push (Arg);
   end Max_Packet_Id_Exceeded;

   -- The queue for memory dump requests overflowed and a request to dump memory with the given packet id was dropped.
   overriding procedure Memory_Dump_Request_Dropped (Self : in out Instance; Arg : in Packet_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Dump_Request_Dropped_History.Push (Arg);
   end Memory_Dump_Request_Dropped;

   -- A new maximum rate has been set for the packetizer.
   overriding procedure Max_Packet_Rate_Set (Self : in out Instance; Arg : in Packets_Per_Period.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Max_Packet_Rate_Set_History.Push (Arg);
   end Max_Packet_Rate_Set;

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
   --    Data products for the Memory Packetizer component.
   -- The current maximum packet sends per time period.
   overriding procedure Max_Packets_Per_Time_Period (Self : in out Instance; Arg : in Packets_Per_Period.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Max_Packets_Per_Time_Period_History.Push (Arg);
   end Max_Packets_Per_Time_Period;

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

end Component.Memory_Packetizer.Implementation.Tester;
