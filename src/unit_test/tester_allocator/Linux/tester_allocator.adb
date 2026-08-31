with Safe_Deallocator;
with State_Snapshot;

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

   -- Fixture state snapshot (no-op body on this target; see the spec):
   package Snap is new State_Snapshot (Object_Type => Tester_Inst);

   function Allocate return Tester_Access is
      T : constant Tester_Access := new Tester_Inst;
   begin
      Snap.Save (T.all);
      return T;
   end Allocate;

   procedure Free (T : in out Tester_Access) is
   begin
      if T /= null then
         Snap.Restore (T.all);
         Inner_Free (T);
      end if;
   end Free;

end Tester_Allocator;
