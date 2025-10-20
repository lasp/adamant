--------------------------------------------------------------------------------
-- Parameters Component Tester Body
--------------------------------------------------------------------------------

-- Includes:
with String_Util;

package body Component.Parameters.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural; Parameter_Update_T_Provide_Count : in Connector_Count_Type) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size, Parameter_Update_T_Provide_Count => Parameter_Update_T_Provide_Count);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Parameter_Update_T_Modify_History.Init (Depth => 20);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 20);
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Init (Depth => 20);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 20);
      Self.Event_T_Recv_Sync_History.Init (Depth => 20);
      Self.Sys_Time_T_Return_History.Init (Depth => 20);
      -- Event histories:
      Self.Parameter_Update_Success_History.Init (Depth => 20);
      Self.Parameter_Update_Id_Not_Recognized_History.Init (Depth => 20);
      Self.Parameter_Stage_Failed_History.Init (Depth => 20);
      Self.Parameter_Update_Failed_History.Init (Depth => 20);
      Self.Parameter_Validation_Failed_History.Init (Depth => 20);
      Self.Parameter_Fetch_Failed_History.Init (Depth => 20);
      Self.Parameter_Fetch_Length_Mismatch_History.Init (Depth => 20);
      Self.Parameter_Fetch_Value_Mismatch_History.Init (Depth => 20);
      Self.Parameter_Update_Length_Mismatch_History.Init (Depth => 20);
      Self.Memory_Region_Length_Mismatch_History.Init (Depth => 20);
      Self.Memory_Region_Crc_Invalid_History.Init (Depth => 20);
      Self.Dumping_Parameters_History.Init (Depth => 20);
      Self.Finished_Dumping_Parameters_History.Init (Depth => 20);
      Self.Starting_Parameter_Table_Update_History.Init (Depth => 20);
      Self.Finished_Parameter_Table_Update_History.Init (Depth => 20);
      Self.Starting_Parameter_Table_Validate_History.Init (Depth => 20);
      Self.Finished_Parameter_Table_Validate_History.Init (Depth => 20);
      Self.Starting_Parameter_Table_Fetch_History.Init (Depth => 20);
      Self.Finished_Parameter_Table_Fetch_History.Init (Depth => 20);
      Self.Invalid_Command_Received_History.Init (Depth => 20);
      Self.Command_Dropped_History.Init (Depth => 20);
      Self.Memory_Region_Dropped_History.Init (Depth => 20);
      -- Packet histories:
      Self.Active_Parameters_History.Init (Depth => 20);

      -- Initialize test components, setting their IDs
      Self.Component_A.Set_Id_Bases (Parameter_Id_Base => 1);
      Self.Component_B.Set_Id_Bases (Parameter_Id_Base => 3);
      Self.Component_C.Set_Id_Bases (Parameter_Id_Base => 5);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Parameter_Update_T_Modify_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Parameter_Update_Success_History.Destroy;
      Self.Parameter_Update_Id_Not_Recognized_History.Destroy;
      Self.Parameter_Stage_Failed_History.Destroy;
      Self.Parameter_Update_Failed_History.Destroy;
      Self.Parameter_Validation_Failed_History.Destroy;
      Self.Parameter_Fetch_Failed_History.Destroy;
      Self.Parameter_Fetch_Length_Mismatch_History.Destroy;
      Self.Parameter_Fetch_Value_Mismatch_History.Destroy;
      Self.Parameter_Update_Length_Mismatch_History.Destroy;
      Self.Memory_Region_Length_Mismatch_History.Destroy;
      Self.Memory_Region_Crc_Invalid_History.Destroy;
      Self.Dumping_Parameters_History.Destroy;
      Self.Finished_Dumping_Parameters_History.Destroy;
      Self.Starting_Parameter_Table_Update_History.Destroy;
      Self.Finished_Parameter_Table_Update_History.Destroy;
      Self.Starting_Parameter_Table_Validate_History.Destroy;
      Self.Finished_Parameter_Table_Validate_History.Destroy;
      Self.Starting_Parameter_Table_Fetch_History.Destroy;
      Self.Finished_Parameter_Table_Fetch_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Command_Dropped_History.Destroy;
      Self.Memory_Region_Dropped_History.Destroy;
      -- Packet histories:
      Self.Active_Parameters_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      -- Self.Component_Instance.Attach_Parameter_Update_T_Provide (From_Index => 1, To_Component => Self'Unchecked_Access, Hook => Self.Parameter_Update_T_Modify_Access);
      -- Self.Component_Instance.Attach_Parameter_Update_T_Provide (From_Index => 2, To_Component => Self'Unchecked_Access, Hook => Self.Parameter_Update_T_Modify_Access);
      -- Self.Component_Instance.Attach_Parameter_Update_T_Provide (From_Index => 3, To_Component => Self'Unchecked_Access, Hook => Self.Parameter_Update_T_Modify_Access);
      -- ^^ Instead of connecting to the tester, lets connect the parameter component to actual component destinations
      Self.Component_Instance.Attach_Parameter_Update_T_Provide (From_Index => 1, To_Component => Self.Component_A'Unchecked_Access, Hook => Self.Component_A.Parameter_Update_T_Modify_Access);
      Self.Component_Instance.Attach_Parameter_Update_T_Provide (From_Index => 2, To_Component => Self.Component_B'Unchecked_Access, Hook => Self.Component_B.Parameter_Update_T_Modify_Access);
      Self.Component_Instance.Attach_Parameter_Update_T_Provide (From_Index => 3, To_Component => Self.Component_C'Unchecked_Access, Hook => Self.Component_C.Parameter_Update_T_Modify_Access);
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
   -- The arrayed parameter request connector. Parameters stages, updates, and
   -- fetches are sent out this connector and a status is returned.
   overriding procedure Parameter_Update_T_Modify (Self : in out Instance; Arg : in out Parameter_Update.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Update_T_Modify_History.Push (Arg);
   end Parameter_Update_T_Modify;

   -- This connector is used to send command responses.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Command_Response_T_Recv_Sync_History.Push (Arg);
   end Command_Response_T_Recv_Sync;

   -- After a memory region is received on the Memory_Region_T_Recv_Async connector
   -- and then processed, it is released via a call to this connector. A status is
   -- also returned, so the downstream component can determine if the parameter
   -- update was successful or not.
   overriding procedure Parameters_Memory_Region_Release_T_Recv_Sync (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameters_Memory_Region_Release_T_Recv_Sync_History.Push (Arg);
   end Parameters_Memory_Region_Release_T_Recv_Sync;

   -- The parameter packet connector. A copy of the active parameters is dumped via
   -- this connector.
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
   -- A parameter table entry was updated.
   overriding procedure Parameter_Update_Success (Self : in out Instance; Arg : in Parameter_Table_Entry_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Update_Success_History.Push (Arg);
   end Parameter_Update_Success;

   -- A parameter table entry could not be updated because the Entry ID is not
   -- recognized.
   overriding procedure Parameter_Update_Id_Not_Recognized (Self : in out Instance; Arg : in Parameter_Table_Entry_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Update_Id_Not_Recognized_History.Push (Arg);
   end Parameter_Update_Id_Not_Recognized;

   -- A parameter value could not be updated.
   overriding procedure Parameter_Stage_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Stage_Failed_History.Push (Arg);
   end Parameter_Stage_Failed;

   -- A parameter value could not be updated.
   overriding procedure Parameter_Update_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Update_Failed_History.Push (Arg);
   end Parameter_Update_Failed;

   -- A parameter value could not be validated.
   overriding procedure Parameter_Validation_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Validation_Failed_History.Push (Arg);
   end Parameter_Validation_Failed;

   -- A parameter value could not be updated.
   overriding procedure Parameter_Fetch_Failed (Self : in out Instance; Arg : in Parameter_Operation_Status.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Fetch_Failed_History.Push (Arg);
   end Parameter_Fetch_Failed;

   -- A parameter was fetched but contained an unexpected length.
   overriding procedure Parameter_Fetch_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameter_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Fetch_Length_Mismatch_History.Push (Arg);
   end Parameter_Fetch_Length_Mismatch;

   -- Multiple parameters in a grouped entry were fetched and contained different
   -- values. Using the first fetched value.
   overriding procedure Parameter_Fetch_Value_Mismatch (Self : in out Instance; Arg : in Parameter_Entry_Comparison.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Fetch_Value_Mismatch_History.Push (Arg);
   end Parameter_Fetch_Value_Mismatch;

   -- A parameter table entry command was received to update a parameter but it
   -- contained an unexpected length.
   overriding procedure Parameter_Update_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameter_Table_Entry_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Parameter_Update_Length_Mismatch_History.Push (Arg);
   end Parameter_Update_Length_Mismatch;

   -- A memory region was received with an invalid length. The length of the region
   -- must be the same size as the parameter table.
   overriding procedure Memory_Region_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Length_Mismatch_History.Push (Arg);
   end Memory_Region_Length_Mismatch;

   -- A memory region parameter table was received with an invalid CRC. The computed
   -- CRC does not match the CRC found in the header.
   overriding procedure Memory_Region_Crc_Invalid (Self : in out Instance; Arg : in Invalid_Parameters_Memory_Region_Crc.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Memory_Region_Crc_Invalid_History.Push (Arg);
   end Memory_Region_Crc_Invalid;

   -- Producing a packet with the currently staged parameter values contained within
   -- connected components.
   overriding procedure Dumping_Parameters (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumping_Parameters_History.Push (Arg);
   end Dumping_Parameters;

   -- Done dumping the parameters.
   overriding procedure Finished_Dumping_Parameters (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Finished_Dumping_Parameters_History.Push (Arg);
   end Finished_Dumping_Parameters;

   -- Starting updating of the parameters from a received memory region.
   overriding procedure Starting_Parameter_Table_Update (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Starting_Parameter_Table_Update_History.Push (Arg);
   end Starting_Parameter_Table_Update;

   -- Done updating the parameters from a received memory region with following
   -- status.
   overriding procedure Finished_Parameter_Table_Update (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Finished_Parameter_Table_Update_History.Push (Arg);
   end Finished_Parameter_Table_Update;

   -- Starting validation of the parameters from a received memory region.
   overriding procedure Starting_Parameter_Table_Validate (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Starting_Parameter_Table_Validate_History.Push (Arg);
   end Starting_Parameter_Table_Validate;

   -- Done validating the parameters from a received memory region with following
   -- status.
   overriding procedure Finished_Parameter_Table_Validate (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Finished_Parameter_Table_Validate_History.Push (Arg);
   end Finished_Parameter_Table_Validate;

   -- Starting updating of the parameters from a received memory region.
   overriding procedure Starting_Parameter_Table_Fetch (Self : in out Instance; Arg : in Memory_Region.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Starting_Parameter_Table_Fetch_History.Push (Arg);
   end Starting_Parameter_Table_Fetch;

   -- Done updating the parameters from a received memory region with following
   -- status.
   overriding procedure Finished_Parameter_Table_Fetch (Self : in out Instance; Arg : in Parameters_Memory_Region_Release.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Finished_Parameter_Table_Fetch_History.Push (Arg);
   end Finished_Parameter_Table_Fetch;

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

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Parameters Component.
   -- This packet contains a copy of all the active parameters managed by this
   -- component.
   overriding procedure Active_Parameters (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Active_Parameters_History.Push (Arg);
   end Active_Parameters;

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

end Component.Parameters.Implementation.Tester;
