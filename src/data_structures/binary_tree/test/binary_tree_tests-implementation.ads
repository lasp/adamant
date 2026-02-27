--------------------------------------------------------------------------------
-- Binary_Tree Tests Spec
--------------------------------------------------------------------------------

with Binary_Tree.Tester;

-- This is a unit test suite for the binary tree object
package Binary_Tree_Tests.Implementation is
   -- Test data and state:
   type Instance is new Binary_Tree_Tests.Base_Instance with private;
private
   -- Fixture procedures:
   overriding procedure Set_Up_Test (Self : in out Instance);
   overriding procedure Tear_Down_Test (Self : in out Instance);

   -- This unit test tests adding and searching for elements in a binary tree.
   overriding procedure Test_Tree (Self : in out Instance);
   -- This unit test tests adding, removing, and searching for elements in a binary tree.
   overriding procedure Test_Tree_Removal (Self : in out Instance);

   -- Test data and state:
   package Positive_B_Tree is new Binary_Tree (Positive);
   package Positive_B_Tree_Tester is new Positive_B_Tree.Tester (Positive'Image);
   type Instance is new Binary_Tree_Tests.Base_Instance with record
      Tree : Positive_B_Tree.Instance_Access;
   end record;
end Binary_Tree_Tests.Implementation;
