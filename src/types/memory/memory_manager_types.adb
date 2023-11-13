with System.Storage_Elements;

package body Memory_Manager_Types is

   function Is_Region_Valid (Region : in Memory_Region.T; Managed_Regions : in Memory_Region_Array_Access; Memory_Pointer : out Byte_Array_Pointer.Instance; Region_Index : out Natural) return Boolean is
      use Byte_Array_Pointer;
   begin
      -- Initialize output pointer to null:
      Memory_Pointer := Null_Pointer;
      Region_Index := 0;

      -- Make sure the managed regions exist:
      if Managed_Regions /= null and then Managed_Regions.all'Length > 0 then
         -- For each managed region, make sure the desired region lies within the bounds of that region.
         -- If we find that the region lies within the bounds, then return true.
         for Idx in Managed_Regions.all'Range loop
            declare
               use System;
               use System.Storage_Elements;
               Managed_Region : Memory_Region.T renames Managed_Regions.all (Idx);
               Region_Start : System.Address renames Region.Address;
               Region_End : constant System.Address := Region.Address + Storage_Offset (Region.Length - 1);
               Managed_Region_Start : System.Address renames Managed_Region.Address;
               Managed_Region_End : constant System.Address := Managed_Region.Address + Storage_Offset (Managed_Region.Length - 1);
            begin
               if Region_Start >= Managed_Region_Start and then Region_Start <= Managed_Region_End and then Region_End >= Managed_Region_Start and then Region_End <= Managed_Region_End then
                  -- Return memory pointer and index into managed memory region array.
                  Memory_Pointer := From_Address (Addr => Region_Start, Size => Region.Length);
                  Region_Index := Idx;
                  return True;
               end if;
            end;
         end loop;
      end if;

      return False;
   end Is_Region_Valid;

   function Is_Region_Valid (Positive_Region : in Memory_Region_Positive.T; Managed_Regions : in Memory_Region_Array_Access; Memory_Pointer : out Byte_Array_Pointer.Instance; Region_Index : out Natural) return Boolean is
   begin
      return Is_Region_Valid (
         Region => (Address => Positive_Region.Address, Length => Positive_Region.Length),
         Managed_Regions => Managed_Regions,
         Memory_Pointer => Memory_Pointer,
         Region_Index => Region_Index
      );
   end Is_Region_Valid;

end Memory_Manager_Types;
