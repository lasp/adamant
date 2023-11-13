with Ada.Unchecked_Deallocation;

package body Safe_Deallocator is

   -- Perform the deallocation using Ada.Unchecked_Deallocation.
   procedure Deallocate_If_Testing (X : in out Name) is
      procedure Free_Object is new Ada.Unchecked_Deallocation (
         Object => Object,
         Name => Name
      );
   begin
      Free_Object (X);
   end Deallocate_If_Testing;

end Safe_Deallocator;
