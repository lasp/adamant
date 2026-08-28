with Protected_Variables;
with Interfaces;

-- This package exists solely so that GNATprove analyzes an instance of each
-- generic package inside Protected_Variables, since GNATprove analyzes
-- generics only at their instantiation points. Real instantiations elsewhere
-- in a project are verified at their own instantiation points when they
-- occur in SPARK analyzed code. Nothing references this package, so it
-- contributes no code to any build.
package Protected_Variables_Prover with SPARK_Mode => On is

   -- A representative record, similar in shape to the packed record types
   -- that components typically hold in a protected variable:
   type Example_Record is record
      Id : Interfaces.Unsigned_16 := 0;
      Value : Interfaces.Unsigned_16 := 0;
   end record;

   package Example_Variable is new Protected_Variables.Generic_Variable (Example_Record);
   package Example_Counter is new Protected_Variables.Generic_Protected_Counter (Interfaces.Unsigned_32);
   -- The countdown requires a range starting at zero. Natural is the typical
   -- actual, and a narrow range checks that nothing depends on T'Last:
   package Example_Countdown is new Protected_Variables.Generic_Protected_Counter_Decrement (Natural);
   type Narrow_Range is range 0 .. 7;
   package Example_Narrow_Countdown is new Protected_Variables.Generic_Protected_Counter_Decrement (Narrow_Range);
   package Example_Staged_Variable is new Protected_Variables.Generic_Staged_Variable (Example_Record);
   package Example_Periodic_Counter is new Protected_Variables.Generic_Protected_Periodic_Counter (Interfaces.Unsigned_16);

end Protected_Variables_Prover;
