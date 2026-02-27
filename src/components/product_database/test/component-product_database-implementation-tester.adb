--------------------------------------------------------------------------------
-- Product_Database Component Tester Body
--------------------------------------------------------------------------------

package body Component.Product_Database.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance) is
   begin
      -- Initialize tester heap:
      -- Connector histories:
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Data_Product_Update_Id_Out_Of_Range_History.Init (Depth => 100);
      Self.Data_Product_Fetch_Id_Out_Of_Range_History.Init (Depth => 100);
      Self.Data_Product_Fetch_Id_Not_Available_History.Init (Depth => 100);
      Self.Override_Cleared_History.Init (Depth => 100);
      Self.Override_Cleared_For_All_History.Init (Depth => 100);
      Self.Data_Product_Overridden_History.Init (Depth => 100);
      Self.Data_Product_Override_Serialization_Failure_History.Init (Depth => 100);
      Self.Data_Product_Override_Id_Out_Of_Range_History.Init (Depth => 100);
      Self.Data_Product_Clear_Override_Id_Out_Of_Range_History.Init (Depth => 100);
      Self.Data_Product_Dump_Id_Not_Available_History.Init (Depth => 100);
      Self.Data_Product_Dump_Id_Out_Of_Range_History.Init (Depth => 100);
      Self.Data_Product_Dumped_History.Init (Depth => 100);
      Self.Dumping_Data_Product_Poly_Type_History.Init (Depth => 100);
      Self.Dumped_Data_Product_Poly_Type_History.Init (Depth => 100);
      Self.Data_Product_Dump_Poly_Id_Not_Available_History.Init (Depth => 100);
      Self.Data_Product_Dump_Poly_Id_Out_Of_Range_History.Init (Depth => 100);
      Self.Data_Product_Poly_Type_Extraction_Failed_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      -- Data product histories:
      Self.Data_Product_Poly_Type_Dump_History.Init (Depth => 100);
      Self.Database_Override_History.Init (Depth => 100);
      -- Packet histories:
      Self.Dump_Packet_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Data_Product_Update_Id_Out_Of_Range_History.Destroy;
      Self.Data_Product_Fetch_Id_Out_Of_Range_History.Destroy;
      Self.Data_Product_Fetch_Id_Not_Available_History.Destroy;
      Self.Override_Cleared_History.Destroy;
      Self.Override_Cleared_For_All_History.Destroy;
      Self.Data_Product_Overridden_History.Destroy;
      Self.Data_Product_Override_Serialization_Failure_History.Destroy;
      Self.Data_Product_Override_Id_Out_Of_Range_History.Destroy;
      Self.Data_Product_Clear_Override_Id_Out_Of_Range_History.Destroy;
      Self.Data_Product_Dump_Id_Not_Available_History.Destroy;
      Self.Data_Product_Dump_Id_Out_Of_Range_History.Destroy;
      Self.Data_Product_Dumped_History.Destroy;
      Self.Dumping_Data_Product_Poly_Type_History.Destroy;
      Self.Dumped_Data_Product_Poly_Type_History.Destroy;
      Self.Data_Product_Dump_Poly_Id_Not_Available_History.Destroy;
      Self.Data_Product_Dump_Poly_Id_Out_Of_Range_History.Destroy;
      Self.Data_Product_Poly_Type_Extraction_Failed_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      -- Data product histories:
      Self.Data_Product_Poly_Type_Dump_History.Destroy;
      Self.Database_Override_History.Destroy;
      -- Packet histories:
      Self.Dump_Packet_History.Destroy;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Data_Product_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Data_Product_T_Recv_Sync_Access);
      Self.Attach_Data_Product_Fetch_T_Request (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Data_Product_Fetch_T_Service_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Sync_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Event_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the event to the correct handler:
      Self.Dispatch_Event (Arg);
   end Event_T_Recv_Sync;

   -- This connector is used to register and respond to the component's commands. This does not need to be connected if the command for this component will not be used.
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

   -- Send a packet of data - used to dump database items.
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
   -- A data product update was received with an ID that was out of range.
   overriding procedure Data_Product_Update_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Update_Id_Out_Of_Range_History.Push (Arg);
   end Data_Product_Update_Id_Out_Of_Range;

   -- A data product fetch was received with an ID that was out of range.
   overriding procedure Data_Product_Fetch_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Fetch_Id_Out_Of_Range_History.Push (Arg);
   end Data_Product_Fetch_Id_Out_Of_Range;

   -- A data product fetch was received with an ID that has not yet been stored in the database.
   overriding procedure Data_Product_Fetch_Id_Not_Available (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Fetch_Id_Not_Available_History.Push (Arg);
   end Data_Product_Fetch_Id_Not_Available;

   -- Override condition cleared for the data product of the provided ID.
   overriding procedure Override_Cleared (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Override_Cleared_History.Push (Arg);
   end Override_Cleared;

   -- Override condition cleared for all data products.
   overriding procedure Override_Cleared_For_All (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Override_Cleared_For_All_History.Push (Arg);
   end Override_Cleared_For_All;

   -- Data product overridden by command.
   overriding procedure Data_Product_Overridden (Self : in out Instance; Arg : in Data_Product_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Overridden_History.Push (Arg);
   end Data_Product_Overridden;

   -- Data product override could not be completed due to a serialization error.
   overriding procedure Data_Product_Override_Serialization_Failure (Self : in out Instance; Arg : in Data_Product_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Override_Serialization_Failure_History.Push (Arg);
   end Data_Product_Override_Serialization_Failure;

   -- A data product override command was received with an ID that was out of range.
   overriding procedure Data_Product_Override_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Override_Id_Out_Of_Range_History.Push (Arg);
   end Data_Product_Override_Id_Out_Of_Range;

   -- A data product clear override command was received with an ID that was out of range.
   overriding procedure Data_Product_Clear_Override_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Clear_Override_Id_Out_Of_Range_History.Push (Arg);
   end Data_Product_Clear_Override_Id_Out_Of_Range;

   -- A data product dump command was received with an ID that has not yet been stored in the database.
   overriding procedure Data_Product_Dump_Id_Not_Available (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Dump_Id_Not_Available_History.Push (Arg);
   end Data_Product_Dump_Id_Not_Available;

   -- A data product dump command was received with an ID that was out of range.
   overriding procedure Data_Product_Dump_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Dump_Id_Out_Of_Range_History.Push (Arg);
   end Data_Product_Dump_Id_Out_Of_Range;

   -- Data product dumped into a packet by command.
   overriding procedure Data_Product_Dumped (Self : in out Instance; Arg : in Data_Product_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Dumped_History.Push (Arg);
   end Data_Product_Dumped;

   -- Data product poly type dumped into a packet by command.
   overriding procedure Dumping_Data_Product_Poly_Type (Self : in out Instance; Arg : in Data_Product_Poly_Extract.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumping_Data_Product_Poly_Type_History.Push (Arg);
   end Dumping_Data_Product_Poly_Type;

   -- Data product poly type dumped into a packet by command.
   overriding procedure Dumped_Data_Product_Poly_Type (Self : in out Instance; Arg : in Data_Product_Poly_Event.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dumped_Data_Product_Poly_Type_History.Push (Arg);
   end Dumped_Data_Product_Poly_Type;

   -- A data product dump poly command was received with an ID that has not yet been stored in the database.
   overriding procedure Data_Product_Dump_Poly_Id_Not_Available (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Dump_Poly_Id_Not_Available_History.Push (Arg);
   end Data_Product_Dump_Poly_Id_Not_Available;

   -- A data product dump poly command was received with an ID that was out of range.
   overriding procedure Data_Product_Dump_Poly_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Dump_Poly_Id_Out_Of_Range_History.Push (Arg);
   end Data_Product_Dump_Poly_Id_Out_Of_Range;

   -- A data product dump poly command failed because the extraction could not succeed with the provided parameters.
   overriding procedure Data_Product_Poly_Type_Extraction_Failed (Self : in out Instance; Arg : in Data_Product_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Poly_Type_Extraction_Failed_History.Push (Arg);
   end Data_Product_Poly_Type_Extraction_Failed;

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
   --    Data products for the Product Database component.
   -- Data product poly type dumped into a data product by command.
   overriding procedure Data_Product_Poly_Type_Dump (Self : in out Instance; Arg : in Data_Product_Poly_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Poly_Type_Dump_History.Push (Arg);
   end Data_Product_Poly_Type_Dump;

   -- If set to Enabled then the database contains at least one data product that has been overridden by command.
   overriding procedure Database_Override (Self : in out Instance; Arg : in Packed_Enable_Disable_Type.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Database_Override_History.Push (Arg);
   end Database_Override;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Product Database.
   -- This packet contains dumped data products.
   overriding procedure Dump_Packet (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dump_Packet_History.Push (Arg);
   end Dump_Packet;

end Component.Product_Database.Implementation.Tester;
