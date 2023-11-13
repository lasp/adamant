with Ada.Text_IO; use Ada.Text_IO;

package body Binary_Tree.Tester is
   function Issorted (Self : in out Binary_Tree.Instance) return Boolean is
      Current_Element : Element_Type;
      Previous_Element : Element_Type;
   begin
      if Self.Size >= 1 then
         Put_Line ("Internal Binary Tree List:");
         Previous_Element := Self.Tree (1);
         Put_Line (Image (Previous_Element));
         for Index in 2 .. Self.Size loop
            Current_Element := Self.Tree (Index);
            Put_Line (Image (Current_Element));
            if Previous_Element > Current_Element then
               return False;
            end if;
            Previous_Element := Current_Element;
         end loop;
      end if;
      return True;
   end Issorted;
end Binary_Tree.Tester;
