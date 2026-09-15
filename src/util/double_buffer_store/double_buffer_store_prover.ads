with Double_Buffer_Store;
with Packed_U32;

-- This package exists solely so that GNATprove analyzes an instance of the generic
-- Double_Buffer_Store package, since GNATprove analyzes generics only at their
-- instantiation points. Real instantiations elsewhere in a project are verified at
-- their own instantiation points when they occur in SPARK analyzed code. Nothing
-- references this package, so it contributes no code to any build.
package Double_Buffer_Store_Prover with SPARK_Mode => On is

   -- A representative store over a packed record, the shape that components store:
   package Example_Store is new Double_Buffer_Store (Packed_U32.T);

end Double_Buffer_Store_Prover;
