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
with Data_Product_Id.Representation;
with Invalid_Data_Product_Length.Representation;
with Store_Crc_Error.Representation;
with Invalid_Command_Info.Representation;
with Command_Header.Representation;

-- Custom Includes:
with Test_Assembly_Nt_Stored_Products;
with Data_Product_Enums; use Data_Product_Enums;
with Data_Product_Types; use Data_Product_Types;

-- The product store saves a predefined set of data products from the database to
-- a byte array (memory region) provided at initialization, usually located in
-- nonvolatile storage (i.e. MRAM). The set of data products to save is configured
-- via an autocoded table provided at instantiation, produced from a
-- stored_products.yaml model file. Data products are saved upon receipt of a tick
-- or by command, and can be restored back into the data product database by
-- command or at Set_Up. The store is protected by a CRC which is written on every
-- save and checked prior to every restore, protecting against the restoration of
-- corrupted or never-initialized memory contents. A command is provided to dump
-- the current contents of the store into a packet. The autocoder limits the total
-- size of the store to fit within a single Packet.T.
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
   package Products_Saved_History_Package is new Printable_History (Natural, Natural'Image);
   package Products_Restored_History_Package is new Printable_History (Natural, Natural'Image);
   package Store_Dumped_History_Package is new Printable_History (Natural, Natural'Image);
   package Data_Product_Missing_On_Save_History_Package is new Printable_History (Data_Product_Id.T, Data_Product_Id.Representation.Image);
   package Data_Product_Length_Mismatch_History_Package is new Printable_History (Invalid_Data_Product_Length.T, Invalid_Data_Product_Length.Representation.Image);
   package Store_Crc_Invalid_History_Package is new Printable_History (Store_Crc_Error.T, Store_Crc_Error.Representation.Image);
   package Invalid_Command_Received_History_Package is new Printable_History (Invalid_Command_Info.T, Invalid_Command_Info.Representation.Image);
   package Dropped_Command_History_Package is new Printable_History (Command_Header.T, Command_Header.Representation.Image);

   -- Packet history packages:
   package Stored_Products_History_Package is new Printable_History (Packet.T, Packet.Representation.Image);

   -- Component class instance:
   type Instance is new Component.Product_Store_Reciprocal.Base_Instance with record
      -- The component instance under test:
      Component_Instance : aliased Component.Product_Store.Implementation.Instance (Test_Assembly_Nt_Stored_Products.Store_Description'Access);
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
      Data_Product_Missing_On_Save_History : Data_Product_Missing_On_Save_History_Package.Instance;
      Data_Product_Length_Mismatch_History : Data_Product_Length_Mismatch_History_Package.Instance;
      Store_Crc_Invalid_History : Store_Crc_Invalid_History_Package.Instance;
      Invalid_Command_Received_History : Invalid_Command_Received_History_Package.Instance;
      Dropped_Command_History : Dropped_Command_History_Package.Instance;
      -- Packet histories:
      Stored_Products_History : Stored_Products_History_Package.Instance;
      -- Booleans to control assertion if message is dropped on async queue:
      Expect_Command_T_Send_Dropped : Boolean := False;
      Command_T_Send_Dropped_Count : Natural := 0;
      -- Status for data product return:
      Data_Product_Fetch_Return_Status : Fetch_Status.E := Fetch_Status.Success;
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
   -- Data products are sent out of this connector upon restore, usually to the data
   -- product database.
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
   -- The data products were saved to the store by command.
   overriding procedure Products_Saved (Self : in out Instance);
   -- The data products held in the store were restored into the data product
   -- database.
   overriding procedure Products_Restored (Self : in out Instance);
   -- Produced a packet with the contents of the store.
   overriding procedure Store_Dumped (Self : in out Instance);
   -- A data product was not available from the database when fetched for saving. The
   -- previous store contents for this data product were preserved if the store CRC
   -- was valid, otherwise the slot was zeroed.
   overriding procedure Data_Product_Missing_On_Save (Self : in out Instance; Arg : in Data_Product_Id.T);
   -- A data product was fetched for saving but contained an unexpected length, so it
   -- was not saved.
   overriding procedure Data_Product_Length_Mismatch (Self : in out Instance; Arg : in Invalid_Data_Product_Length.T);
   -- The store CRC did not validate prior to a restore, so the restore was not
   -- performed. This is expected on the first boot before the store has ever been
   -- written.
   overriding procedure Store_Crc_Invalid (Self : in out Instance; Arg : in Store_Crc_Error.T);
   -- A command was received with invalid parameters.
   overriding procedure Invalid_Command_Received (Self : in out Instance; Arg : in Invalid_Command_Info.T);
   -- A command was dropped due to a full queue.
   overriding procedure Dropped_Command (Self : in out Instance; Arg : in Command_Header.T);

   -----------------------------------------------
   -- Packet handler primitives:
   -----------------------------------------------
   -- Description:
   --    Packets for the Product Store component. The contents of this packet are
   --    populated based on the stored products model provided to the Product Store
   --    component at instantiation.
   -- This packet contains the contents of the data product store managed by this
   -- component, including the store CRC, the save time (if configured), and each
   -- stored data product (with its timestamp, if configured).
   overriding procedure Stored_Products (Self : in out Instance; Arg : in Packet.T);

   -----------------------------------------------
   -- Special primitives for activating component
   -- queue:
   -----------------------------------------------
   -- Tell the component to dispatch all items off of its queue:
   not overriding function Dispatch_All (Self : in out Instance) return Natural;
   -- Tell the component to dispatch n items off of its queue:
   not overriding function Dispatch_N (Self : in out Instance; N : in Positive := 1) return Natural;

end Component.Product_Store.Implementation.Tester;
