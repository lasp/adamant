generic
   with function Image (Item : in Element_Type) return String;
package Binary_Tree.Tester is
   -- Checks if binary tree is sorted:
   function Issorted (Self : in out Binary_Tree.Instance) return Boolean;
end Binary_Tree.Tester;
