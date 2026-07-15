--------------------------------------------------------------------------------
-- Product_Store Component Implementation Spec
--------------------------------------------------------------------------------

-- Includes:
with Product_Store_Types;
with Tick;
with Command;

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
   -- The component is initialized by providing the memory region it is to manage,
   -- which holds the data product store.
   --
   -- Init Parameters:
   -- Bytes : Basic_Types.Byte_Array_Access - A pointer to an allocation of bytes to
   -- be used for storing the data products. The size of this byte array MUST be at
   -- least Store_Description.Store_Size bytes in length. Only the first
   -- Store_Description.Store_Size bytes will be used by this component.
   -- Restore_On_Set_Up : Boolean - If set to True, the component will attempt to
   -- restore the stored data products into the data product database during Set_Up,
   -- seeding the database with the values saved before the last reboot. If the store
   -- CRC does not validate (i.e. the store was never written or was corrupted) the
   -- restore is skipped and an error event is produced.
   -- Commands_Dispatched_Per_Tick : Positive - The number of commands executed per
   -- tick, if any are in the queue.
   --
   overriding procedure Init (Self : in out Instance; Bytes : in not null Basic_Types.Byte_Array_Access; Restore_On_Set_Up : in Boolean := False; Commands_Dispatched_Per_Tick : in Positive := 3);

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
      -- The allocation of bytes used to hold the store:
      Bytes : Basic_Types.Byte_Array_Access := null;
      -- Should the store be restored into the data product database at Set_Up?
      Restore_On_Set_Up : Boolean := False;
      -- The number of commands dispatched from the queue per tick:
      Commands_Dispatched_Per_Tick : Positive := 3;
   end record;

   ---------------------------------------
   -- Set Up Procedure
   ---------------------------------------
   -- If the component is configured with Restore_On_Set_Up, then the store
   -- contents are restored into the data product database here, seeding the
   -- database with the values saved before the last reboot. If the store CRC
   -- does not validate, the restore is skipped and an error event is produced.
   overriding procedure Set_Up (Self : in out Instance);

   ---------------------------------------
   -- Invokee connector primitives:
   ---------------------------------------
   -- This is the base tick for the component. Each tick received saves the data
   -- products to the store.
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
   -- performs the same operation as the receipt of a tick. If the store's save time
   -- is configured as Tick_Time, the current time is used instead, since no tick is
   -- available.
   overriding function Save_Products (Self : in out Instance) return Command_Execution_Status.E;
   -- Restore the data product values held in the store back into the data product
   -- database. The store CRC is checked prior to the restore, and the command fails
   -- if the CRC does not validate.
   overriding function Restore_Products (Self : in out Instance) return Command_Execution_Status.E;
   -- Dump the current contents of the store into a packet. The store contents are
   -- dumped as-is, without validating the CRC, so that corrupted store contents can
   -- be inspected on the ground.
   overriding function Dump_Store (Self : in out Instance) return Command_Execution_Status.E;

   -- Invalid command handler. This procedure is called when a command's arguments are found to be invalid:
   overriding procedure Invalid_Command (Self : in out Instance; Cmd : in Command.T; Errant_Field_Number : in Unsigned_32; Errant_Field : in Basic_Types.Poly_Type);

end Component.Product_Store.Implementation;
