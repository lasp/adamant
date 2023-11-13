with Memory_Region;
with Memory_Region_Positive;
with Byte_Array_Pointer;

package Memory_Manager_Types is

   -- Memory region array type for holding many memory regions.
   type Memory_Region_Array is array (Natural range <>) of Memory_Region.T;
   type Memory_Region_Array_Access is access all Memory_Region_Array;

   -- Types for describing whether a region is protected or not, meaning an "arm"
   -- command is required prior to executing an action on that region of memory.
   type Memory_Protection_Type is (Protected_Region, Unprotected_Region);
   type Memory_Protection_Array is array (Natural range <>) of Memory_Protection_Type;
   type Memory_Protection_Array_Access is access all Memory_Protection_Array;

   -- Determine if a memory region lies within a valid list of managed regions. Otherwise return false.
   -- A pointer to the valid memory region is returned if it was found.
   function Is_Region_Valid (Region : in Memory_Region.T; Managed_Regions : in Memory_Region_Array_Access; Memory_Pointer : out Byte_Array_Pointer.Instance; Region_Index : out Natural) return Boolean;
   function Is_Region_Valid (Positive_Region : in Memory_Region_Positive.T; Managed_Regions : in Memory_Region_Array_Access; Memory_Pointer : out Byte_Array_Pointer.Instance; Region_Index : out Natural) return Boolean;

end Memory_Manager_Types;
