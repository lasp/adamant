with Basic_Types;
with Configuration;

package Packet_Types is
   -- Id type:
   type Packet_Id is new Natural range 0 .. 65_535;
   subtype Packet_Id_Base is Packet_Id;
   type Sequence_Count_Mod_Type is mod 2**14;
   -- Length type:
   subtype Packet_Buffer_Length_Type is Natural range 0 .. Configuration.Packet_Buffer_Size;
   subtype Packet_Buffer_Index_Type is Packet_Buffer_Length_Type range 0 .. Packet_Buffer_Length_Type'Last - 1;
   -- Buffer type:
   subtype Packet_Buffer_Type is Basic_Types.Byte_Array (Packet_Buffer_Index_Type);
end Packet_Types;
