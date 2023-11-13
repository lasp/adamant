package body Safe_Deallocator is

   -- Do NOT perform the deallocation at all. We do not support
   -- deallocation of objects on deployment targets.
   procedure Deallocate_If_Testing (X : in out Name) is
      Ignore : Name renames X;
   begin
      null;
   end Deallocate_If_Testing;

end Safe_Deallocator;
