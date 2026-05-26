--------------------------------------------------------------------------------
-- Parameter_Table_Util Body
--------------------------------------------------------------------------------

with Byte_Array_Pointer.Packed;

package body Parameter_Table_Util is

   -- Sanity check on the Parameter_Table_Header constants: the Crc_Table and
   -- Version sections must exactly cover the header. If the header packed
   -- record is changed and the constants drift out of sync, fail to compile.
   pragma Compile_Time_Error (
      Parameter_Table_Header.Crc_Section_Length + Parameter_Table_Header.Version_Length /=
         Parameter_Table_Header.Size_In_Bytes,
      "Parameter_Table_Header constants are inconsistent: " &
      "Crc_Section_Length + Version_Length must equal Size_In_Bytes."
   );

   function Compute_Table_Crc (Table_Ptr : in Byte_Array_Pointer.Instance) return Crc_16.Crc_16_Type is
      use Byte_Array_Pointer;
      -- Slice past the leading Crc_Table field so the CRC is computed over
      -- (Version + Data). Slice is pointer arithmetic only -- the underlying
      -- bytes are not copied.
      Crc_Region : constant Byte_Array_Pointer.Instance :=
         Slice (Table_Ptr,
                Start_Index => Parameter_Table_Header.Crc_Section_Length,
                End_Index   => Length (Table_Ptr) - 1);
   begin
      return Crc_16.Compute_Crc_16 (Crc_Region);
   end Compute_Table_Crc;

   function Get_Ptr_And_Header_From_Region (
      Region : in Memory_Region.T;
      Table_Header : out Parameter_Table_Header.T
   ) return Byte_Array_Pointer.Instance is
      use Byte_Array_Pointer;
      use Byte_Array_Pointer.Packed;

      Ptr : constant Byte_Array_Pointer.Instance := Unpack (Region);
      Ptr_Header : constant Byte_Array_Pointer.Instance := Slice (Ptr, Start_Index => 0, End_Index => Parameter_Table_Header.Size_In_Bytes - 1);
   begin
      Table_Header := Parameter_Table_Header.Serialization.From_Byte_Array (To_Byte_Array (Ptr_Header));
      return Ptr;
   end Get_Ptr_And_Header_From_Region;

end Parameter_Table_Util;
