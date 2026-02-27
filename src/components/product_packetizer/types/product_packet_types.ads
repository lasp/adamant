with Packet_Types;
with Data_Product_Types;
with Sys_Time.Arithmetic;

package Product_Packet_Types is

   -- Packet enabled state enumeration:
   type Packet_Enabled_Type is (Disabled, Enabled, On_Change);

   -- Packet item definition:
   type Packet_Item_Type is record
      -- Data product identifier of item:
      Data_Product_Id : Data_Product_Types.Data_Product_Id := Data_Product_Types.Data_Product_Id'First;
      -- Some special configuration booleans:
      Use_Timestamp : Boolean := False;      -- Should we use the timestamp of this DP as the packet timestamp
      Include_Timestamp : Boolean := False;  -- Should we include the DP timestamp in front of the DP data
      Event_On_Missing : Boolean := False;   -- Should an event be issued if the data product is missing from the db?
      Used_For_On_Change : Boolean := True;  -- Should this DP's timestamp be used to determine if packet is sent on change?
      Packet_Period_Item : Boolean := False; -- If set to true then the DP Id of this item is used to index into the
      -- packet_description_list and extract the period of the packet to report
      -- instead of being used to grab a DP from the db.
      -- Size of the item in bytes:
      Size : Data_Product_Types.Data_Product_Buffer_Length_Type := 0;
   end record;

   -- List of data product ids to put into a packet.
   type Packet_Items_Type is array (Natural range <>) of Packet_Item_Type;
   type Packet_Items_Access_Type is access all Packet_Items_Type;

   -- A record containing data necessary to build a packet filled with
   -- data products.
   type Packet_Description_Type is record
      -- The packet identifier:
      Id : Packet_Types.Packet_Id := Packet_Types.Packet_Id'First;
      -- Data product items that make up the packet:
      Items : Packet_Items_Access_Type := null;
      -- The period (in ticks) at which to build and send packet:
      Period : Natural := 1;
      -- The offset (in ticks) at which to stagger the construction of this packet. An offset of
      -- 5 will cause the packet to be built according to its period, but 5 ticks later than expected.
      -- Note that the offset should be less than the period otherwise it will be mod'ed by the period
      -- so that it is less than the period. For example, if the period is 3 and the offset is set to
      -- 5, the actual offset used will be 2.
      --
      -- This field can be used to stagger packet creation, allowing the user to evenly distribute
      -- the work that this component does, so as to not cause cycle slips when many packets need to
      -- be built on the same tick.
      Offset : Natural := 0;
      -- Is periodic sending of packet enabled? Can be Disabled, Enabled, or On_Change:
      Enabled : Packet_Enabled_Type := Product_Packet_Types.Enabled;
      -- If set to true then the packet is timestamped with the time found on the incoming Tick.T
      -- instead of the current time as fetched via the time connector.
      Use_Tick_Timestamp : Boolean := False;
      -- How many packets of this type have been created.
      Count : Packet_Types.Sequence_Count_Mod_Type := Packet_Types.Sequence_Count_Mod_Type'First;
      -- Flag to send packet via command:
      Send_Now : Boolean := False;
      -- Store the last time this packet was emitted.
      Last_Emission_Time : Sys_Time.T := Sys_Time.Arithmetic.Sys_Time_Zero;
   end record;

   -- Packet description array types:
   type Packet_Description_List_Type is array (Natural range <>) of Packet_Description_Type;
   type Packet_Description_List_Access_Type is access all Packet_Description_List_Type;

end Product_Packet_Types;
