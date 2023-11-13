with Ccsds_Primary_Header;
with Data_Product_Types;
with Interfaces; use Interfaces;

package Ccsds_Downsampler_Types is

   -- Define internal downsampler entry:
   type Ccsds_Downsampler_Tree_Entry is record
      -- Apid to track our filter counts and factors
      Apid : Ccsds_Primary_Header.Ccsds_Apid_Type := Ccsds_Primary_Header.Ccsds_Apid_Type'Last;
      -- Filter factor for an entry and set to no filtering
      Filter_Factor : Unsigned_16 := 1;
      -- Filter count for each entry
      Filter_Count : Unsigned_16 := Unsigned_16'First;
   end record;

   -- Product entry definition.
   type Ccsds_Downsample_Packet_Entry is record
      -- Apid of the packet to downsample
      Apid : Ccsds_Primary_Header.Ccsds_Apid_Type := Ccsds_Primary_Header.Ccsds_Apid_Type'First;
      -- Filter factor to downsample for the corresponding APID
      Filter_Factor : Unsigned_16 := Unsigned_16'First;
   end record;

   -- List of product entries of each apid:
   type Ccsds_Downsample_Packet_List is array (Data_Product_Types.Data_Product_Id range <>) of Ccsds_Downsample_Packet_Entry;
   type Ccsds_Downsample_Packet_List_Access is access all Ccsds_Downsample_Packet_List;

end Ccsds_Downsampler_Types;
