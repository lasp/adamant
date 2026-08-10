--------------------------------------------------------------------------------
-- Product_Store Component Tester Body
--------------------------------------------------------------------------------

-- Includes:
with String_Util;
with Packed_U32;

package body Component.Product_Store.Implementation.Tester is

   ---------------------------------------
   -- Initialize heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural) is
   begin
      -- Initialize component heap:
      Self.Component_Instance.Init_Base (Queue_Size => Queue_Size);

      -- Initialize tester heap:
      -- Connector histories:
      Self.Data_Product_Fetch_T_Service_History.Init (Depth => 100);
      Self.Data_Product_T_Recv_Sync_History.Init (Depth => 100);
      Self.Packet_T_Recv_Sync_History.Init (Depth => 100);
      Self.Command_Response_T_Recv_Sync_History.Init (Depth => 100);
      Self.Event_T_Recv_Sync_History.Init (Depth => 100);
      Self.Sys_Time_T_Return_History.Init (Depth => 100);
      -- Event histories:
      Self.Products_Saved_History.Init (Depth => 100);
      Self.Products_Restored_History.Init (Depth => 100);
      Self.Store_Dumped_History.Init (Depth => 100);
      Self.Save_On_Tick_Enabled_History.Init (Depth => 100);
      Self.Save_On_Tick_Disabled_History.Init (Depth => 100);
      Self.Data_Product_Missing_On_Save_History.Init (Depth => 100);
      Self.Data_Product_Id_Out_Of_Range_History.Init (Depth => 100);
      Self.Data_Product_Length_Mismatch_History.Init (Depth => 100);
      Self.Stored_Length_Mismatch_History.Init (Depth => 100);
      Self.Store_Crc_Invalid_History.Init (Depth => 100);
      Self.Invalid_Command_Received_History.Init (Depth => 100);
      Self.Dropped_Command_History.Init (Depth => 100);
      -- Data product histories:
      Self.Save_Count_History.Init (Depth => 100);
      Self.Restore_Count_History.Init (Depth => 100);
      Self.Crc_Invalid_Count_History.Init (Depth => 100);
      -- Packet histories:
      Self.Stored_Products_History.Init (Depth => 100);
   end Init_Base;

   procedure Final_Base (Self : in out Instance) is
   begin
      -- Destroy tester heap:
      -- Connector histories:
      Self.Data_Product_Fetch_T_Service_History.Destroy;
      Self.Data_Product_T_Recv_Sync_History.Destroy;
      Self.Packet_T_Recv_Sync_History.Destroy;
      Self.Command_Response_T_Recv_Sync_History.Destroy;
      Self.Event_T_Recv_Sync_History.Destroy;
      Self.Sys_Time_T_Return_History.Destroy;
      -- Event histories:
      Self.Products_Saved_History.Destroy;
      Self.Products_Restored_History.Destroy;
      Self.Store_Dumped_History.Destroy;
      Self.Save_On_Tick_Enabled_History.Destroy;
      Self.Save_On_Tick_Disabled_History.Destroy;
      Self.Data_Product_Missing_On_Save_History.Destroy;
      Self.Data_Product_Id_Out_Of_Range_History.Destroy;
      Self.Data_Product_Length_Mismatch_History.Destroy;
      Self.Stored_Length_Mismatch_History.Destroy;
      Self.Store_Crc_Invalid_History.Destroy;
      Self.Invalid_Command_Received_History.Destroy;
      Self.Dropped_Command_History.Destroy;
      -- Data product histories:
      Self.Save_Count_History.Destroy;
      Self.Restore_Count_History.Destroy;
      Self.Crc_Invalid_Count_History.Destroy;
      -- Packet histories:
      Self.Stored_Products_History.Destroy;

      -- Destroy component heap:
      Self.Component_Instance.Final_Base;
   end Final_Base;

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance) is
   begin
      Self.Component_Instance.Attach_Data_Product_Fetch_T_Request (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_Fetch_T_Service_Access);
      Self.Component_Instance.Attach_Data_Product_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Data_Product_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Packet_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Packet_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Command_Response_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Command_Response_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Event_T_Send (To_Component => Self'Unchecked_Access, Hook => Self.Event_T_Recv_Sync_Access);
      Self.Component_Instance.Attach_Sys_Time_T_Get (To_Component => Self'Unchecked_Access, Hook => Self.Sys_Time_T_Return_Access);
      Self.Attach_Tick_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Tick_T_Recv_Sync_Access);
      Self.Attach_Command_T_Send (To_Component => Self.Component_Instance'Unchecked_Access, Hook => Self.Component_Instance.Command_T_Recv_Async_Access);
   end Connect;

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Fetch a data product item from the database for saving.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T is
      To_Return : Data_Product_Return.T;
      Dp : Data_Product.T;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Fetch_T_Service_History.Push (Arg);

      -- We need to simulate the return of an actual data product here. The id
      -- matches the assembly-assigned id of Test_Component_1, which is given a
      -- data product id base of 100 in the test assembly:
      Dp.Header.Time := Self.Dp_Time;
      case Arg.Id is
         -- A, U32
         when 100 =>
            Dp.Header.Id := 100;
            Dp.Header.Buffer_Length := Packed_U32.Serialization.Byte_Array'Length;
            Dp.Buffer (Dp.Buffer'First .. Dp.Buffer'First + Dp.Header.Buffer_Length - 1) := Packed_U32.Serialization.To_Byte_Array ((Value => 23));
         when others =>
            pragma Assert (False, "Unexpected id received.");
      end case;

      -- Override the length if necessary:
      if Self.Data_Product_Length_Override > 0 then
         Dp.Header.Buffer_Length := Self.Data_Product_Length_Override;
      end if;

      To_Return.The_Status := Self.Data_Product_Fetch_Return_Status;
      To_Return.The_Data_Product := Dp;
      return To_Return;
   end Data_Product_Fetch_T_Service;

   -- Data products are sent out of this connector, both when restoring the store
   -- contents into the data product database and when reporting this component's own
   -- counter data products.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the data product to the correct handler. Only the component's
      -- own counter data products can be dispatched to the typed histories.
      -- Restored data products belong to Test_Component_1 (id base 100 in the
      -- test assembly) and would fail the local id lookup:
      if Arg.Header.Id < 100 then
         Self.Dispatch_Data_Product (Arg);
      end if;
   end Data_Product_T_Recv_Sync;

   -- Send a packet holding the contents of the store when a dump is commanded.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Packet_T_Recv_Sync_History.Push (Arg);
      -- Dispatch the packet to the correct handler:
      Self.Dispatch_Packet (Arg);
   end Packet_T_Recv_Sync;

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
   -- Description:
   --    Events for the Product Store component.
   -- The data products were saved to the store by command.
   overriding procedure Products_Saved (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Products_Saved_History.Push (Arg);
   end Products_Saved;

   -- The data products held in the store were restored into the data product
   -- database.
   overriding procedure Products_Restored (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Products_Restored_History.Push (Arg);
   end Products_Restored;

   -- Produced a packet with the contents of the store.
   overriding procedure Store_Dumped (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Store_Dumped_History.Push (Arg);
   end Store_Dumped;

   -- The automatic saving of data products to the store upon receipt of a tick was
   -- enabled by command.
   overriding procedure Save_On_Tick_Enabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Save_On_Tick_Enabled_History.Push (Arg);
   end Save_On_Tick_Enabled;

   -- The automatic saving of data products to the store upon receipt of a tick was
   -- disabled by command.
   overriding procedure Save_On_Tick_Disabled (Self : in out Instance) is
      Arg : constant Natural := 0;
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Save_On_Tick_Disabled_History.Push (Arg);
   end Save_On_Tick_Disabled;

   -- A data product was not available from the database when fetched for saving, so
   -- its slot in the store was left unchanged, preserving the last saved value (or
   -- the never-saved marker if no value was ever saved). This event can be disabled
   -- per data product in the stored products model.
   overriding procedure Data_Product_Missing_On_Save (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Missing_On_Save_History.Push (Arg);
   end Data_Product_Missing_On_Save;

   -- A data product id was reported as out of range by the database when fetched for
   -- saving, so its slot in the store was left unchanged. This indicates a
   -- misconfiguration between the stored products model and the data product
   -- database.
   overriding procedure Data_Product_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Id_Out_Of_Range_History.Push (Arg);
   end Data_Product_Id_Out_Of_Range;

   -- A data product was fetched for saving but contained an unexpected length, so
   -- its slot in the store was left unchanged.
   overriding procedure Data_Product_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Data_Product_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Data_Product_Length_Mismatch_History.Push (Arg);
   end Data_Product_Length_Mismatch;

   -- A store entry held a stored length that is neither zero (never saved) nor the
   -- expected length for the data product, so the entry was not restored. This
   -- indicates that the stored products model has changed since the store was last
   -- written.
   overriding procedure Stored_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Stored_Length.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Stored_Length_Mismatch_History.Push (Arg);
   end Stored_Length_Mismatch;

   -- The store CRC did not validate prior to a restore, so the restore was not
   -- performed. This is expected on the first boot before the store has ever been
   -- written.
   overriding procedure Store_Crc_Invalid (Self : in out Instance; Arg : in Crc_Mismatch_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Store_Crc_Invalid_History.Push (Arg);
   end Store_Crc_Invalid;

   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Invalid_Command_Received_History.Push (Arg);
   end Invalid_Command_Received;

   -- A command was dropped due to a full queue.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Dropped_Command_History.Push (Arg);
   end Dropped_Command;

   -----------------------------------------------
   -- Data product handler primitive:
   -----------------------------------------------
   -- Description:
   --    Data products for the Product Store component.
   -- The number of times the data products have been saved to the store, either by
   -- tick or by command. This counter rolls over.
   overriding procedure Save_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Save_Count_History.Push (Arg);
   end Save_Count;

   -- The number of times the store contents have been successfully restored into the
   -- data product database. This counter rolls over.
   overriding procedure Restore_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Restore_Count_History.Push (Arg);
   end Restore_Count;

   -- The number of times a restore was refused because the store CRC did not
   -- validate. This counter rolls over.
   overriding procedure Crc_Invalid_Count (Self : in out Instance; Arg : in Packed_U16.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Crc_Invalid_Count_History.Push (Arg);
   end Crc_Invalid_Count;

   -----------------------------------------------
   -- Packet handler primitive:
   -----------------------------------------------
   -- Description:
   --    Packets for the Product Store component. The contents of this packet are
   --    populated based on the stored products model provided to the Product Store
   --    component at instantiation.
   -- This packet contains the contents of the data product store managed by this
   -- component, including the store CRC, the save time (if configured), and each
   -- stored data product (with its timestamp, if configured).
   overriding procedure Stored_Products (Self : in out Instance; Arg : in Packet.T) is
   begin
      -- Push the argument onto the test history for looking at later:
      Self.Stored_Products_History.Push (Arg);
   end Stored_Products;

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

end Component.Product_Store.Implementation.Tester;
