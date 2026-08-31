-- Host body: no-ops. Each scenario gets a fresh heap Tester, so
-- there is nothing to restore (see the spec).
package body State_Snapshot is

   procedure Save (Object : in Object_Type) is
      pragma Unreferenced (Object);
   begin
      null;
   end Save;

   procedure Restore (Object : in out Object_Type) is
      pragma Unreferenced (Object);
   begin
      null;
   end Restore;

end State_Snapshot;
