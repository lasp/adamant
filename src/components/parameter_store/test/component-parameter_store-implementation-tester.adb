--------------------------------------------------------------------------------
-- Parameter_Store Component Tester Body
--------------------------------------------------------------------------------

with String_Util;

package body Component.Parameter_Store.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 10);
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Init (Depth => 10);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 10);
      Self.Event_T_Recv_Sync_History.Init (Depth => 10);
      Self.Sys_Time_T_Return_History.Init (Depth => 10);
      -- Event histories:
      Self.Memory_Region_Length_Mismatch_History.Init (Depth => 10);
      Self.Memory_Region_Crc_Invalid_History.Init (Depth => 10);
      Self.Dumped_Parameters_History.Init (Depth => 10);
      Self.Parameter_Table_Updated_History.Init (Depth => 10);
      Self.Parameter_Table_Fetched_History.Init (Depth => 10);
      Self.Invalid_Command_Received_History.Init (Depth => 10);
      Self.Command_Dropped_History.Init (Depth => 10);
      Self.Memory_Region_Dropped_History.Init (Depth => 10);
      -- Packet histories:
      Self.Stored_Parameters_History.Init (Depth => 10);
      Self.Table_Validation_Not_Supported_History.Init (Depth => 10);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Memory_Region_Length_Mismatch_History.Destroy;
      Self.Memory_Region_Crc_Invalid_History.Destroy;
      Self.Dumped_Parameters_History.Destroy;
      Self.Parameter_Table_Updated_History.Destroy;
      Self.Parameter_Table_Fetched_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Command_Dropped_History.Destroy;
      Self.Memory_Region_Dropped_History.Destroy;
      Self.Table_Validation_Not_Supported_History.Destroy;
      -- Packet histories:
      Self.Stored_Parameters_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Parameters_Memory_Region_Release_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Parameters_Memory_Region_Release_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
      Self.Attach_Parameters_Memory_Region_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Parameters_Memory_Region_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- After a memory region is received on the Memory_Region_T_Recv_Async connector and then processed, it is released via a call to this connector. A status is also returned, so the downstream component can determine if the parameter table update was successful or not.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Push (Arg);
   end Parameters_Memory_Region_Release_T_Recv_Sync;

   -- The parameter packet connector. A copy of the managed parameter table is dumped via this connector.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

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
      To_Return : Sys_Time.T;
   begin
      -- Return the system time:
      To_Return := Self.System_Time;
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

   -- This procedure is called when a Parameters_Memory_Region_T_Send message is dropped due to a full queue.
   overriding procedure Parameters_Memory_Region_T_Send_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
      Ignore : Parameters_Memory_Region.T renames Arg;
   begin
      if not Self.Expect_Parameters_Memory_Region_T_Send_Dropped then
         pragma Assert (False, "The component's queue filled up when Parameters_Memory_Region_T_Send was called!");
      else
         Self.Parameters_Memory_Region_T_Send_Dropped_Count := @ + 1;
         Self.Expect_Parameters_Memory_Region_T_Send_Dropped := False;
      end if;
   end Parameters_Memory_Region_T_Send_Dropped;

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Parameter Store component.
   -- A memory region was received with an invalid length. The length of the region must be the same size as the parameter table.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Length_Mismatch_History.Push (Arg);
   end Memory_Region_Length_Mismatch;

   -- A memory region parameter table was received with an invalid CRC. The computed CRC does not match the CRC found in the header.
   overriding procedure Memory_Region_Crc_Invalid (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Crc.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Crc_Invalid_History.Push (Arg);
   end Memory_Region_Crc_Invalid;

   -- Produced a packet with the contents of the parameter store.
   overriding procedure Dumped_Parameters (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumped_Parameters_History.Push (Arg);
   end Dumped_Parameters;

   -- Parameter table updated from a received memory region.
   overriding procedure Parameter_Table_Updated (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Table_Updated_History.Push (Arg);
   end Parameter_Table_Updated;

   -- Starting parameter fetch into the received memory region.
   overriding procedure Parameter_Table_Fetched (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Table_Fetched_History.Push (Arg);
   end Parameter_Table_Fetched;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command was dropped due to a full queue.
   overriding procedure Command_Dropped (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Dropped_History.Push (Arg);
   end Command_Dropped;

   -- A memory region was dropped due to a full queue.
   overriding procedure Memory_Region_Dropped (Self : in out Instance; Arg : in Parameters_Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Dropped_History.Push (Arg);
   end Memory_Region_Dropped;

   -- Parameter table validation is not supported.
   overriding procedure Table_Validation_Not_Supported (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Table_Validation_Not_Supported_History.Push (Arg);
   end Table_Validation_Not_Supported;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Parameter Store component.
   -- This packet contains a copy of all the parameters stored and managed by this component.
   overriding procedure Stored_Parameters (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Stored_Parameters_History.Push (Arg);
   end Stored_Parameters;

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

end Component.Parameter_Store.Implementation.Tester;
