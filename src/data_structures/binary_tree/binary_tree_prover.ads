with Binary_Tree;

-- This package exists solely so that GNATprove analyzes an instance of the
-- generic Binary_Tree package, since GNATprove analyzes generics only at
-- their instantiation points. Real instantiations elsewhere in a project are
-- verified at their own instantiation points when they occur in SPARK
-- analyzed code. Nothing references this package, so it contributes no code
-- to any build.
package Binary_Tree_Prover with SPARK_Mode => On is

   -- A representative element, similar in shape to the identifier keyed
   -- records that binary trees typically hold:
   type Example_Element is record
      Id : Natural := 0;
      Value : Natural := 0;
   end record;

   function "<" (Left, Right : Example_Element) return Boolean is (Left.Id < Right.Id);
   function ">" (Left, Right : Example_Element) return Boolean is (Left.Id > Right.Id);

   package Example_Tree is new Binary_Tree (Example_Element);

end Binary_Tree_Prover;
