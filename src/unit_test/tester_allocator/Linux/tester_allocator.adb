with Safe_Deallocator;

package body Tester_Allocator is

   -- Use Safe_Deallocator rather than Ada.Unchecked_Deallocation directly.
   -- Linux_Debug applies Ravenscar (No_Unchecked_Deallocation) via
   -- linux_debug.gpr, which would reject a direct UD here. Safe_Deallocator
   -- picks "testing" or "production" body based on target path, so the
   -- release build is a no-op (still legal under Ravenscar) while
   -- Linux_Test actually frees the heap.
   procedure Inner_Free is new Safe_Deallocator.Deallocate_If_Testing
     (Object => Tester_Inst,
      Name   => Tester_Access);

   function Allocate return Tester_Access is
   begin
      return new Tester_Inst;
   end Allocate;

   procedure Free (T : in out Tester_Access) is
   begin
      if T /= null then
         Inner_Free (T);
      end if;
   end Free;

end Tester_Allocator;
