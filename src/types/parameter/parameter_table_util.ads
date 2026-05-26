--------------------------------------------------------------------------------
-- Parameter_Table_Util Spec
--------------------------------------------------------------------------------

-- Shared helpers for components that handle parameter table memory regions:
-- the Parameters component, the Parameter_Store component, and the
-- Parameter_Table_Forwarder component all use these to compute the table CRC
-- and to extract the header from an incoming memory region.

with Byte_Array_Pointer;
with Crc_16;
with Memory_Region;
with Parameter_Table_Header;

package Parameter_Table_Util is

   -- Compute the CRC-16 of a parameter table buffer.
   --
   -- Table_Ptr addresses a parameter table laid out as a Parameter_Table_Header.T
   -- followed by table data:
   --   bytes [0 .. Crc_Section_Length - 1]            : Crc_Table   (Crc_16.Crc_16_Type)
   --   bytes [Crc_Section_Length .. Size_In_Bytes-1]  : Version     (Short_Float / F32)
   --   bytes [Size_In_Bytes .. Length-1]              : Table data
   --
   -- The returned CRC is computed over (Version + Data) -- i.e. starting at
   -- offset Crc_Section_Length, skipping the leading Crc_Table bytes that
   -- hold the previously stored CRC itself. The pointer is passed by reference,
   -- so the table bytes are not copied onto the stack -- this matters for tables
   -- large enough to bust a stack frame.
   function Compute_Table_Crc (Table_Ptr : in Byte_Array_Pointer.Instance) return Crc_16.Crc_16_Type
      with Pre => Byte_Array_Pointer.Length (Table_Ptr) >= Parameter_Table_Header.Size_In_Bytes;

   -- Extract the parameter table header from an incoming memory region.
   --
   -- Decodes the first Parameter_Table_Header.Size_In_Bytes bytes of Region
   -- into Table_Header. Returns a byte pointer that spans the entire region
   -- (header + payload). Callers typically pass this pointer to
   -- Compute_Table_Crc to verify the table's CRC after the header has been
   -- decoded.
   function Get_Ptr_And_Header_From_Region (
      Region : in Memory_Region.T;
      Table_Header : out Parameter_Table_Header.T
   ) return Byte_Array_Pointer.Instance
      with Pre => Region.Length >= Parameter_Table_Header.Size_In_Bytes;

end Parameter_Table_Util;
