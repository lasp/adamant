with Data_Product_Types;
with Packet_Types;

-- Types used by the Product Store component to describe the set of data
-- products that it saves and restores. Static instances of these types are
-- autocoded from a stored_products.yaml model file.
package Product_Store_Types is

   -- The source of the save time written to the store header on each save:
   type Save_Time_Type is (
      -- The current time, fetched from the system time connector, is written on each save.
      Current_Time,
      -- The time found on the incoming Tick.T is written on each save. If a save is
      -- commanded, the current time is used instead.
      Tick_Time
   );

   -- The timestamp applied to a data product when it is restored from the
   -- store into the data product database:
   type Restore_Time_Type is (
      -- Restore with a timestamp of zero.
      Use_Zeros,
      -- Restore with the save time held in the store header.
      Use_Save_Time,
      -- Restore with the data product's own timestamp held in the store. This is
      -- required (and only valid) if the entry is configured with Store_Timestamp
      -- set to True.
      Use_Stored_Dp_Time
   );

   -- Description of a single data product entry in the store:
   type Store_Entry_Type is record
      -- Data product identifier of the entry:
      Data_Product_Id : Data_Product_Types.Data_Product_Id := Data_Product_Types.Data_Product_Id'First;
      -- Should the data product's timestamp be saved to the store just before its value?
      Store_Timestamp : Boolean := False;
      -- The timestamp to apply to this data product when it is restored:
      Restore_Time : Restore_Time_Type := Use_Save_Time;
      -- Should an event be issued if the data product is missing from the database on save?
      Event_On_Missing : Boolean := True;
      -- Size of the data product (not including any timestamp) in bytes:
      Size : Data_Product_Types.Data_Product_Buffer_Length_Type := 0;
   end record;

   -- List of data product entries that make up the store:
   type Store_Entry_List_Type is array (Natural range <>) of Store_Entry_Type;
   type Store_Entry_List_Access_Type is access all Store_Entry_List_Type;

   -- The size of a single copy of the store is constrained such that each copy
   -- can always be dumped within a single Packet.T:
   subtype Store_Size_Type is Natural range 0 .. Packet_Types.Packet_Buffer_Type'Length;

   -- A record describing the entire data product store:
   type Store_Description_Type is record
      -- The source of the save time written to the store header:
      Save_Time : Save_Time_Type := Current_Time;
      -- The data product entries that make up the store:
      Entries : not null Store_Entry_List_Access_Type;
      -- The size of one copy of the store in bytes, including the CRC, save
      -- counter, and save time header. The component manages two copies of the
      -- store (double buffering), each of this size:
      Store_Size : Store_Size_Type := 0;
   end record;
   type Store_Description_Access_Type is access all Store_Description_Type;

end Product_Store_Types;
