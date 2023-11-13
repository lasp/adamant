with Byte_Array_Pointer;
with Packet_Types;

package Memory_Packetizer_Types is

   type Memory_Dump is record
      Id : Packet_Types.Packet_Id;
      Memory_Pointer : Byte_Array_Pointer.Instance;
   end record;

end Memory_Packetizer_Types;
