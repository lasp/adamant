--------------------------------------------------------------------------------
-- Product_Store Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Product_Store_Types;
with Tick;
with Command;

-- The product store saves a predefined set of data products from the database to
-- two byte arrays (memory regions) provided at initialization, usually located in
-- nonvolatile storage (i.e. MRAM). The set of data products to save is configured
-- via an autocoded table provided at instantiation, produced from a
-- stored_products.yaml model file. Data products are saved upon receipt of a tick
-- or by command, and can be restored back into the data product database by
-- command or at Set_Up. The store is double buffered so that a reboot in the
-- middle of a save can never corrupt the only good copy - each save writes the
-- copy NOT holding the most recent valid save, stamping it with a monotonic save
-- counter and writing its CRC last, and a restore reads from the valid copy
-- holding the newest counter. Each copy is protected by a CRC which is written on
-- every save and checked prior to every restore, protecting against the
-- restoration of corrupted or never-initialized memory contents. A command is
-- provided to dump the current contents of both store copies, each into its own
-- packet. The autocoder limits the size of each store copy to fit within a
-- single Packet.T.
package Component.Product_Store.Implementation is

   -- The component class instance record:
   -- This component requires a description of the data products it is responsible
   -- for saving and restoring. This description should be provided as an autocoded
   -- output from a stored_products.yaml model file.
   --
   -- Discriminant Parameters:
   -- Store_Description : Product_Store_Types.Store_Description_Access_Type - The
   -- description of the data product store to manage.
   --
   type Instance (Store_Description : not null Product_Store_Types.Store_Description_Access_Type) is new Product_Store.Base_Instance with private;

   --------------------------------------------------
   -- Subprogram for implementation init method:
   --------------------------------------------------
   -- The component is initialized by providing the two memory regions it is to
   -- manage, which hold the two copies of the data product store.
   --
   -- Init Parameters:
   -- Bytes_A : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes
   -- to be used for storing copy A of the data products. The size of this byte
   -- array MUST be at least Store_Description.Store_Size bytes in length (which
   -- includes the CRC, save counter, and save time header). Only the first
   -- Store_Description.Store_Size bytes will be used by this component. This
   -- allocation must not overlap the allocation provided for Bytes_B. The two
   -- copies may be placed in different memory banks so that a fault contained to
   -- one bank cannot corrupt both copies.
   -- Bytes_B : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes
   -- to be used for storing copy B of the data products, with the same size
   -- requirement as Bytes_A. This allocation must not overlap the allocation
   -- provided for Bytes_A.
   -- Restore_On_Set_Up : Boolean - If set to True, the component will attempt to
   -- restore the stored data products into the data product database during Set_Up,
   -- seeding the database with the values saved before the last reboot. If neither
   -- store copy holds a valid CRC (i.e. the store was never written or was
   -- corrupted) the restore is skipped and error events are produced.
   -- Ticks_Per_Save : Positive - The number of ticks that must be received before
   -- the data products are saved to the store. This allows the component to be
   -- connected to a rate group running at a speed appropriate for command
   -- responsiveness (i.e. 1 Hz), while saving to the store at a slower rate (i.e.
   -- every 600 ticks for a 10 minute save cadence).
   -- Commands_Dispatched_Per_Tick : Positive - The number of commands executed per
   -- tick, if any are in the queue.
   --
   overriding procedure Init (Self : in out Instance; Bytes_A : in not null Basic_Types.Byte_Array_Access; Bytes_B : in not null Basic_Types.Byte_Array_Access; Restore_On_Set_Up : in Boolean := False; Ticks_Per_Save : in Positive := 1; Commands_Dispatched_Per_Tick : in Positive := 3);

private

   -- The component class instance record:
   -- This component requires a description of the data products it is responsible
   -- for saving and restoring. This description should be provided as an autocoded
   -- output from a stored_products.yaml model file.
   --
   -- Discriminant Parameters:
   -- Store_Description : Product_Store_Types.Store_Description_Access_Type - The
   -- description of the data product store to manage.
   --
   type Instance (Store_Description : not null Product_Store_Types.Store_Description_Access_Type) is new Product_Store.Base_Instance with record
      -- The allocations of bytes used to hold the two copies of the store:
      Bytes_A : Basic_Types.Byte_Array_Access := null;
      Bytes_B : Basic_Types.Byte_Array_Access := null;
      -- Should the store be restored into the data product database at Set_Up?
      Restore_On_Set_Up : Boolean := False;
      -- The number of ticks that must be received before a save is performed:
      Ticks_Per_Save : Positive := 1;
      -- Counts received ticks, rolling over at Ticks_Per_Save:
      Tick_Count : Natural := 0;
      -- The number of commands dispatched from the queue per tick:
      Commands_Dispatched_Per_Tick : Positive := 3;
      -- Is the automatic saving of data products on tick currently enabled?
      Save_On_Tick : Boolean := True;
      -- Rolling counters reported as data products:
      Save_Count : Interfaces.Unsigned_16 := 0;
      Restore_Count : Interfaces.Unsigned_16 := 0;
      Crc_Invalid_Count : Interfaces.Unsigned_16 := 0;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- The counter data products are seeded with zero here. Then, if the component
   -- is configured with Restore_On_Set_Up, the store contents are restored into
   -- the data product database, seeding the database with the values saved before
   -- the last reboot. If the store CRC does not validate, the restore is skipped
   -- and an error event is produced.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Commands are dispatched from the
   -- queue on every tick, and the data products are saved to the store every
   -- Ticks_Per_Save ticks (when saving on tick is enabled).
   overriding procedure Tick_T_Recv_Sync (Self : in out Instance; Arg : in Tick.T);
   -- This is the command receive connector.
   overriding procedure Command_T_Recv_Async (Self : in out Instance; Arg : in Command.T);
   -- This procedure is called when a Command_T_Recv_Async message is dropped due to a full queue.
   overriding procedure Command_T_Recv_Async_Dropped (Self : in out Instance; Arg : in Command.T);

   ---------------------------------------
   -- Invoker connector primitives:
   ---------------------------------------
   -- This procedure is called when a Data_Product_T_Send message is dropped due to a full queue.
   overriding procedure Data_Product_T_Send_Dropped (Self : in out Instance; Arg : in Data_Product.T) is null;
   -- This procedure is called when a Packet_T_Send message is dropped due to a full queue.
   overriding procedure Packet_T_Send_Dropped (Self : in out Instance; Arg : in Packet.T) is null;
   -- This procedure is called when a Command_Response_T_Send message is dropped due to a full queue.
   overriding procedure Command_Response_T_Send_Dropped (Self : in out Instance; Arg : in Command_Response.T) is null;
   -- This procedure is called when a Event_T_Send message is dropped due to a full queue.
   overriding procedure Event_T_Send_Dropped (Self : in out Instance; Arg : in Event.T) is null;

   -----------------------------------------------
   -- Command handler primitives:
   -----------------------------------------------
   -- Description:
   --    These are the commands for the Product Store component.
   -- Save the configured data products from the database into the store. This
   -- works regardless of whether saving on tick is enabled or disabled. The
   -- current time is used as the save time, even if the store is configured for
   -- Tick_Time, since no tick is available.
   overriding function Save_Products (Self : in out Instance) return Command_Execution_Status.E;
   -- Restore the data product values held in the store back into the data product
   -- database. The CRC of each store copy is checked prior to the restore, and the
   -- values are restored from the valid copy holding the newest save counter. The
   -- command fails if neither copy's CRC validates.
   overriding function Restore_Products (Self : in out Instance) return Command_Execution_Status.E;
   -- Dump the current contents of both store copies, each into its own packet. The
   -- store contents are dumped as-is, without validating the CRCs, so that
   -- corrupted store contents can be inspected on the ground.
   overriding function Dump_Store (Self : in out Instance) return Command_Execution_Status.E;
   -- Enable the automatic saving of the data products to the store upon receipt of
   -- a tick.
   overriding function Enable_Save_On_Tick (Self : in out Instance) return Command_Execution_Status.E;
   -- Disable the automatic saving of the data products to the store upon receipt of
   -- a tick.
   overriding function Disable_Save_On_Tick (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Product_Store.Implementation;
