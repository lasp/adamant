package body Byte_Array_Pointer.Packed is

   function Pack (Self : in Instance) return Memory_Region.T is
   begin
      return (Address => Self.Address, Length => Self.Length);
   end Pack;

   -- Opposite of above function:
   function Unpack (Region : in Memory_Region.T) return Instance is
   begin
      return From_Address (Region.Address, Region.Length);
   end Unpack;

end Byte_Array_Pointer.Packed;
