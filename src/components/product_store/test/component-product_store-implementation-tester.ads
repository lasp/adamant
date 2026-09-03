--------------------------------------------------------------------------------
-- Product_Store Component Tester Spec
--------------------------------------------------------------------------------

-- Includes:
with Component.Product_Store_Reciprocal;
with Printable_History;
with Data_Product_Return.Representation;
with Data_Product_Fetch.Representation;
with Data_Product.Representation;
with Packet.Representation;
with Command_Response.Representation;
with Event.Representation;
with Sys_Time.Representation;
with Event;
with Store_Copy_Info.Representation;
with Data_Product_Id.Representation;
with Invalid_Data_Product_Length.Representation;
with Invalid_Stored_Length.Representation;
with Copy_Crc_Mismatch_Info.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;
with Data_Product;
with Packed_U16.Representation;

-- Custom Includes:
with Test_Assembly_Stored_Products_Backup;
with Data_Product_Enums; use Data_Product_Enums;
with Data_Product_Types; use Data_Product_Types;

-- The product store saves a predefined set of data products from the database to
-- two byte arrays (memory regions) provided at initialization, usually located in
-- nonvolatile storage (i.e. MRAM). The set of data products to save is configured
-- via an autocoded table provided at instantiation, produced from a
-- stored_products.yaml model file named <assembly_name>.stored_products.yaml, or
-- <specific_name>.<assembly_name>.stored_products.yaml when more than one product
-- store is instantiated within the same assembly. Data products are saved upon
-- receipt of a tick (which can be divided down and enabled/disabled by command)
-- or by command, and can be restored back into the data product database by
-- command or at Set_Up. The store is double buffered so that a reboot in the
-- middle of a save can never corrupt the only good copy - each save writes the
-- copy NOT holding the most recent valid save, stamping it with a monotonic save
-- counter and writing its CRC last, and a restore reads from the valid copy
-- holding the newest counter. A reboot mid-save therefore costs at most one save
-- interval of freshness. Each copy is protected by a CRC which is written on
-- every save and checked prior to every restore, protecting against the
-- restoration of corrupted or never-initialized memory contents. Each store entry
-- also holds the stored length of its data product, where a length of zero marks
-- an entry that has never been saved. Entries whose data product cannot be
-- fetched during a save keep the values from the most recent valid save, and
-- entries that have never been saved are silently skipped on restore, leaving
-- those data products unavailable in the database rather than restoring
-- meaningless values. A command is provided to dump the current contents of both
-- store copies, each into its own packet. The autocoder limits the size of each
-- store copy to fit within a single Packet.T, and requires every stored data
-- product to have an always valid type, so a restore can never produce a value
-- that fails validation.
package Component.Product_Store.Implementation.Tester is

   use Component.Product_Store_Reciprocal;
   -- Invoker connector history packages:
   package Data_Product_Fetch_T_Service_History_Package is new Printable_History (Data_Product_Fetch.T, Data_Product_Fetch.Representation.Image);
   package Data_Product_Fetch_T_Service_Return_History_Package is new Printable_History (Data_Product_Return.T, Data_Product_Return.Representation.Image);
   package Data_Product_T_Recv_Sync_History_Package is new Printable_History (Data_Product.T, Data_Product.Representation.Image);
   package Packet_T_Recv_Sync_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Command_Response_T_Recv_Sync_History_Package is new Printable_History (Command_Response.T, Command_Response.Representation.Image);
   package Event_T_Recv_Sync_History_Package is new Printable_History (Event.T, Event.Representation.Image);
   package Sys_Time_T_Return_History_Package is new Printable_History (Sys_Time.T, Sys_Time.Representation.Image);

   -- Event history packages:
   package Products_Saved_History_Package is new Printable_History (Store_Copy_Info.T, Store_Copy_Info.Representation.Image);
   package Products_Restored_History_Package is new Printable_History (Store_Copy_Info.T, Store_Copy_Info.Representation.Image);
   package Store_Dumped_History_Package is new Printable_History (Natural, Natural'Image);
   package Save_On_Tick_Enabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Save_On_Tick_Disabled_History_Package is new Printable_History (Natural, Natural'Image);
   package Data_Product_Missing_On_Save_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Id_Out_Of_Range_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Length_Mismatch_History_Package is new Printable_History (Invalid_Data_Product_Length.T, Invalid_Data_Product_Length.Representation.Image);
   package Stored_Length_Mismatch_History_Package is new Printable_History (Invalid_Stored_Length.T, Invalid_Stored_Length.Representation.Image);
   package Store_Crc_Invalid_History_Package is new Printable_History (Copy_Crc_Mismatch_Info.T, Copy_Crc_Mismatch_Info.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Dropped_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);

   -- Data product history packages:
   package Save_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Restore_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);
   package Crc_Invalid_Count_History_Package is new Printable_History (Packed_U16.T, Packed_U16.Representation.Image);

   -- Packet history packages:
   package Stored_Products_A_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);
   package Stored_Products_B_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Product_Store_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Product_Store.Implementation.Instance (Test_Assembly_Stored_Products_Backup.Store_Description'Access);
      -- Connector histories:
      Data_Product_Fetch_T_Service_History : Data_Product_Fetch_T_Service_History_Package.Instance;
      Data_Product_T_Recv_Sync_History : Data_Product_T_Recv_Sync_History_Package.Instance;
      Packet_T_Recv_Sync_History : Packet_T_Recv_Sync_History_Package.Instance;
      Command_Response_T_Recv_Sync_History : Command_Response_T_Recv_Sync_History_Package.Instance;
      Event_T_Recv_Sync_History : Event_T_Recv_Sync_History_Package.Instance;
      Sys_Time_T_Return_History : Sys_Time_T_Return_History_Package.Instance;
      -- Event histories:
      Products_Saved_History : Products_Saved_History_Package.Instance;
      Products_Restored_History : Products_Restored_History_Package.Instance;
      Store_Dumped_History : Store_Dumped_History_Package.Instance;
      Save_On_Tick_Enabled_History : Save_On_Tick_Enabled_History_Package.Instance;
      Save_On_Tick_Disabled_History : Save_On_Tick_Disabled_History_Package.Instance;
      Data_Product_Missing_On_Save_History : Data_Product_Missing_On_Save_History_Package.Instance;
      Data_Product_Id_Out_Of_Range_History : Data_Product_Id_Out_Of_Range_History_Package.Instance;
      Data_Product_Length_Mismatch_History : Data_Product_Length_Mismatch_History_Package.Instance;
      Stored_Length_Mismatch_History : Stored_Length_Mismatch_History_Package.Instance;
      Store_Crc_Invalid_History : Store_Crc_Invalid_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Dropped_Command_History : Dropped_Command_History_Package.Instance;
      -- Data product histories:
      Save_Count_History : Save_Count_History_Package.Instance;
      Restore_Count_History : Restore_Count_History_Package.Instance;
      Crc_Invalid_Count_History : Crc_Invalid_Count_History_Package.Instance;
      -- Packet histories:
      Stored_Products_A_History : Stored_Products_A_History_Package.Instance;
      Stored_Products_B_History : Stored_Products_B_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      -- Status for data product return:
      Data_Product_Fetch_Return_Status : Fetch_Status.E := Fetch_Status.Success;
      -- If set to a nonzero id, fetches of that id alone return Not_Available. This
      -- can be used to simulate a single data product missing from the database:
      Fetch_Fail_Id : Data_Product_Types.Data_Product_Id := 0;
      -- This variable is used to override the data product return length, which can be used to
      -- induce a length mismatch error during testing. A value of zero does NOT override, any
      -- other value does.
      Data_Product_Length_Override : Data_Product_Buffer_Length_Type := 0;
      -- Data product time:
      Dp_Time : Sys_Time.T := (5, 11);
   end record;
   type Instance_Access is access all Instance;

   ---------------------------------------
   -- Initialize component heap variables:
   ---------------------------------------
   procedure Init_Base (Self : in out Instance; Queue_Size : in Natural);
   procedure Final_Base (Self : in out Instance);

   ---------------------------------------
   -- Test initialization functions:
   ---------------------------------------
   procedure Connect (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- Fetch a data product item from the database for saving.
   overriding function Data_Product_Fetch_T_Service (Self : in out Instance; Arg : in Data_Product_Fetch.T) return Data_Product_Return.T;
   -- Data products are sent out of this connector, both when restoring the store
   -- contents into the data product database and when reporting this component's own
   -- counter data products.
   overriding procedure Data_Product_T_Recv_Sync (Self : in out Instance; Arg : in Data_Product.T);
   -- Send a packet holding the contents of the store when a dump is commanded.
   overriding procedure Packet_T_Recv_Sync (Self : in out Instance; Arg : in Packet.T);
   -- This connector is used to register and respond to the component's commands.
   overriding procedure Command_Response_T_Recv_Sync (Self : in out Instance; Arg : in Command_Response.T);
   -- Events are sent out of this connector.
   overriding procedure Event_T_Recv_Sync (Self : in out Instance; Arg : in Event.T);
   -- The system time is retrieved via this connector.
   overriding function Sys_Time_T_Return (Self : in out Instance) return Sys_Time.T;

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Command_T_Send message is dropped due to a full queue.
   overriding procedure Command_T_Send_Dropped (Self : in out Instance; Arg : in Command.T);

   -----------------------------------------------
   -- Event handler primitive:
   -----------------------------------------------
   -- Description:
   --    Events for the Product Store component.
   -- The data products were saved to the store by command. The parameter reports the
   -- store copy that was written and the save counter it now holds.
   overriding procedure Products_Saved (Self : in out Instance; Arg : in Store_Copy_Info.T);
   -- The data products held in the store were restored into the data product
   -- database. The parameter reports the store copy that was restored from (the
   -- valid copy holding the newest save counter) and the save counter it holds.
   overriding procedure Products_Restored (Self : in out Instance; Arg : in Store_Copy_Info.T);
   -- Produced packets with the contents of both copies of the store.
   overriding procedure Store_Dumped (Self : in out Instance);
   -- The automatic saving of data products to the store upon receipt of a tick was
   -- enabled by command.
   overriding procedure Save_On_Tick_Enabled (Self : in out Instance);
   -- The automatic saving of data products to the store upon receipt of a tick was
   -- disabled by command.
   overriding procedure Save_On_Tick_Disabled (Self : in out Instance);
   -- A data product was not available from the database when fetched for saving, so
   -- its slot in the store was left unchanged, preserving the last saved value (or
   -- the never-saved marker if no value was ever saved). This event can be disabled
   -- per data product in the stored products model.
   overriding procedure Data_Product_Missing_On_Save (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product id was reported as out of range by the database when fetched for
   -- saving, so its slot in the store was left unchanged. This indicates a
   -- misconfiguration between the stored products model and the data product
   -- database.
   overriding procedure Data_Product_Id_Out_Of_Range (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product was fetched for saving but contained an unexpected length, so
   -- its slot in the store was left unchanged.
   overriding procedure Data_Product_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Data_Product_Length.T);
   -- A store entry held a stored length that is neither zero (never saved) nor the
   -- expected length for the data product, so the entry was not restored. This
   -- indicates that the stored products model has changed since the store was last
   -- written.
   overriding procedure Stored_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Stored_Length.T);
   -- A restore found no store copy with a valid CRC, so the restore was not
   -- performed. This event is produced once per copy, reporting that copy's CRC
   -- mismatch. This is expected on the first boot before the store has ever been
   -- written.
   overriding procedure Store_Crc_Invalid (Self : in out Instance; Arg : in Copy_Crc_Mismatch_Info.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T);

   -----------------------------------------------
   -- Data product handler primitives:
   -----------------------------------------------
   -- Description:
   --    Data products for the Product Store component.
   -- The number of times the data products have been saved to the store, either by
   -- tick or by command. This counter rolls over.
   overriding procedure Save_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of times the store contents have been successfully restored into the
   -- data product database. This counter rolls over.
   overriding procedure Restore_Count (Self : in out Instance; Arg : in Packed_U16.T);
   -- The number of times a restore was refused because neither store copy held a
   -- valid CRC. This counter rolls over.
   overriding procedure Crc_Invalid_Count (Self : in out Instance; Arg : in Packed_U16.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Product Store component. The contents of these packets are
   --    populated based on the stored products model provided to the Product Store
   --    component at instantiation. The store is double buffered, and each copy is
   --    dumped in its own packet.
   -- This packet contains the contents of copy A of the data product store managed
   -- by this component, including the store CRC, the save counter, the save time,
   -- and each stored data product (with its timestamp, if configured).
   overriding procedure Stored_Products_A (Self : in out Instance; Arg : in Packet.T);
   -- This packet contains the contents of copy B of the data product store managed
   -- by this component, including the store CRC, the save counter, the save time,
   -- and each stored data product (with its timestamp, if configured).
   overriding procedure Stored_Products_B (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Product_Store.Implementation.Tester;
